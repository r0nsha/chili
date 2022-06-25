use super::ty::IntoLlvmType;
use super::{
    abi::{align_of, size_of},
    traits::IsALoadInst,
};
use crate::ast::{
    self,
    const_value::ConstValue,
    pattern::{NamePattern, Pattern, UnpackPattern, UnpackPatternKind},
    workspace::{BindingId, BindingInfo, ModuleId, ModuleInfo},
};
use crate::ast::{ty::*, workspace::Workspace};
use crate::ast::{FunctionId, Intrinsic};
use crate::common::build_options;
use crate::common::{
    builtin::{BUILTIN_FIELD_DATA, BUILTIN_FIELD_LEN},
    scopes::Scopes,
    target::TargetMetrics,
};
use crate::infer::{normalize::Normalize, ty_ctx::TyCtx};
use inkwell::passes::PassManagerBuilder;
use inkwell::OptimizationLevel;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    passes::PassManager,
    types::{BasicType, BasicTypeEnum, IntType},
    values::{BasicValue, BasicValueEnum, FunctionValue, GlobalValue, PointerValue},
    AddressSpace, IntPredicate,
};
use std::collections::HashMap;
use ustr::{ustr, Ustr, UstrMap};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum CodegenDecl<'ctx> {
    Function(FunctionValue<'ctx>),
    Global(GlobalValue<'ctx>),
    Local(PointerValue<'ctx>),
}

impl<'ctx> CodegenDecl<'ctx> {
    pub fn into_function_value(&self) -> FunctionValue<'ctx> {
        match self {
            CodegenDecl::Function(f) => *f,
            _ => panic!("can't get function value of {:?}", self),
        }
    }

    pub fn into_global_value(&self) -> GlobalValue<'ctx> {
        match self {
            CodegenDecl::Global(g) => *g,
            _ => panic!("can't get global value of {:?}", self),
        }
    }

    pub fn into_pointer_value(&self) -> PointerValue<'ctx> {
        match self {
            CodegenDecl::Function(f) => f.as_global_value().as_pointer_value(),
            CodegenDecl::Global(g) => g.as_pointer_value(),
            CodegenDecl::Local(p) => *p,
        }
    }
}

pub struct Codegen<'cg, 'ctx> {
    pub workspace: &'cg Workspace,
    pub tycx: &'cg TyCtx,
    pub typed_ast: &'cg ast::TypedAst,

    pub target_metrics: TargetMetrics,

    pub context: &'ctx Context,
    pub module: &'cg Module<'ctx>,
    pub builder: &'cg Builder<'ctx>,
    pub ptr_sized_int_type: IntType<'ctx>,

    pub global_decls: HashMap<BindingId, CodegenDecl<'ctx>>,
    pub types: HashMap<BindingId, BasicTypeEnum<'ctx>>,
    pub static_strs: UstrMap<PointerValue<'ctx>>,
    pub functions: HashMap<FunctionId, FunctionValue<'ctx>>,
    pub extern_functions: UstrMap<FunctionValue<'ctx>>,
    pub intrinsics: HashMap<Intrinsic, FunctionValue<'ctx>>,
}

#[derive(Clone)]
pub struct CodegenState<'ctx> {
    pub module_info: ModuleInfo,
    pub function: FunctionValue<'ctx>,
    pub fn_type: FunctionType,
    pub return_ptr: Option<PointerValue<'ctx>>,
    pub loop_blocks: Vec<LoopBlock<'ctx>>,
    pub decl_block: BasicBlock<'ctx>,
    pub curr_block: BasicBlock<'ctx>,
    pub scopes: Scopes<BindingId, CodegenDecl<'ctx>>, // TODO: switch to BindingInfoId
}

impl<'ctx> CodegenState<'ctx> {
    pub fn new(
        module_info: ModuleInfo,
        function: FunctionValue<'ctx>,
        fn_type: FunctionType,
        return_ptr: Option<PointerValue<'ctx>>,
        decl_block: BasicBlock<'ctx>,
        entry_block: BasicBlock<'ctx>,
    ) -> Self {
        Self {
            module_info,
            function,
            fn_type,
            return_ptr,
            loop_blocks: vec![],
            decl_block,
            curr_block: entry_block,
            scopes: Scopes::default(),
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push_scope();
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop_scope();
    }
}

#[derive(Clone, Copy)]
pub struct LoopBlock<'ctx> {
    head: BasicBlock<'ctx>,
    exit: BasicBlock<'ctx>,
}

impl<'cg, 'ctx> Codegen<'cg, 'ctx> {
    pub fn start(&mut self) {
        self.gen_top_level_binding(self.workspace.entry_point_function_id.unwrap());
        self.gen_entry_point_function();
    }

    pub fn optimize(&mut self) {
        let pass_manager_builder = PassManagerBuilder::create();

        let optimization_level: OptimizationLevel =
            self.workspace.build_options.optimization_level.into();

        let size_level: u32 = match self.workspace.build_options.optimization_level {
            build_options::OptimizationLevel::Debug => 1,
            build_options::OptimizationLevel::Release => 2,
        };

        pass_manager_builder.set_optimization_level(optimization_level);
        pass_manager_builder.set_size_level(size_level);

        let pass_manager = PassManager::create(());
        pass_manager_builder.populate_module_pass_manager(&pass_manager);
        pass_manager.run_on(&self.module);

        let link_time_optimizations = PassManager::create(());
        pass_manager_builder.populate_lto_pass_manager(&link_time_optimizations, false, true);
        link_time_optimizations.run_on(&self.module);
    }

    pub fn gen_top_level_binding(&mut self, id: BindingId) -> CodegenDecl<'ctx> {
        if let Some(decl) = self.global_decls.get(&id) {
            return *decl;
        } else if let Some(binding) = self.typed_ast.get_binding(id) {
            if let Some(expr) = binding.value.as_ref() {
                if let ast::Ast::Const(ast::Const {
                    value: ConstValue::Function(function),
                    ..
                }) = expr.as_ref()
                {
                    let function = self.gen_function(function.id, None);
                    self.insert_global_decl(id, CodegenDecl::Function(function))
                } else {
                    self.declare_global_binding(id, binding)
                }
            } else {
                self.declare_global_binding(id, binding)
            }
        } else {
            panic!("{:#?}", self.workspace.binding_infos.get(id))
        }
    }

    pub fn insert_global_decl(
        &mut self,
        id: BindingId,
        decl: CodegenDecl<'ctx>,
    ) -> CodegenDecl<'ctx> {
        self.global_decls.insert(id, decl);
        decl
    }

    pub fn declare_global_binding(
        &mut self,
        id: BindingId,
        binding: &ast::Binding,
    ) -> CodegenDecl<'ctx> {
        // forward declare the global value, i.e: `let answer = 42`
        // the global value will is initialized by the entry point function

        if let Some(decl) = self.global_decls.get(&id) {
            return *decl;
        }

        let binding_info = self.workspace.binding_infos.get(id).unwrap();

        if let Some(redirect) = binding_info.redirects_to {
            self.gen_top_level_binding(redirect)
        } else {
            let global_value = if binding.kind.is_extern() {
                let ty = binding.ty.llvm_type(self);
                self.add_global_uninit(id, ty, Linkage::External)
            } else {
                self.add_global(id, Linkage::Private)
            };

            self.insert_global_decl(id, CodegenDecl::Global(global_value))
        }
    }

    pub fn add_global(&mut self, id: BindingId, linkage: Linkage) -> GlobalValue<'ctx> {
        let binding_info = self.workspace.binding_infos.get(id).unwrap();

        let ty = binding_info.ty.llvm_type(self);

        let global_value = self.add_global_uninit(id, ty, linkage);

        let value = if let Some(const_value) = &binding_info.const_value {
            self.gen_const_value(None, const_value, &self.tycx.ty_kind(binding_info.ty))
        } else {
            ty.const_zero()
        };

        global_value.set_initializer(&value);

        global_value
    }

    pub fn add_global_uninit(
        &mut self,
        id: BindingId,
        ty: BasicTypeEnum<'ctx>,
        linkage: Linkage,
    ) -> GlobalValue<'ctx> {
        let binding_info = self.workspace.binding_infos.get(id).unwrap();
        let global_value = self.module.add_global(ty, None, &binding_info.name);
        global_value.set_linkage(linkage);
        global_value
    }

    pub fn find_binding_info_by_name(
        &mut self,
        module_name: impl Into<Ustr>,
        name: impl Into<Ustr>,
    ) -> &BindingInfo {
        let module_name: Ustr = module_name.into();
        let name: Ustr = name.into();

        let module_id = self
            .workspace
            .module_infos
            .iter()
            .position(|(_, m)| m.name == module_name)
            .map(ModuleId::from)
            .unwrap_or_else(|| panic!("couldn't find {}", module_name));

        self.workspace
            .binding_infos
            .iter()
            .map(|(_, b)| b)
            .find(|b| b.module_id == module_id && b.name == name)
            .unwrap_or_else(|| panic!("couldn't find {} in {}", name, module_name))
    }

    pub fn find_decl_by_name(
        &mut self,
        module_name: impl Into<Ustr>,
        name: impl Into<Ustr>,
    ) -> CodegenDecl<'ctx> {
        let id = self.find_binding_info_by_name(module_name, name).id;
        self.gen_top_level_binding(id)
    }

    pub fn gen_binding_pattern_with_expr(
        &mut self,
        state: &mut CodegenState<'ctx>,
        pattern: &Pattern,
        ty: &Type,
        expr: &Option<Box<ast::Ast>>,
    ) {
        if ty.is_type() {
            return;
        }

        match pattern {
            Pattern::Name(NamePattern { id: binding_id, .. }) => {
                self.gen_local_and_store_expr(state, *binding_id, &expr, ty);
            }
            Pattern::StructUnpack(pattern) => match ty.maybe_deref_once() {
                Type::Module(_) => {
                    self.gen_binding_module_unpack_pattern(state, pattern);
                }
                Type::Struct(struct_ty) => {
                    let ptr = self.gen_local_and_store_expr(state, BindingId::unknown(), &expr, ty);

                    self.gen_binding_struct_unpack_pattern(state, pattern, ty, &struct_ty, ptr);
                }
                ty => panic!("unexpected type: {}", ty),
            },
            Pattern::TupleUnpack(pattern) => {
                let ptr = self.gen_local_and_store_expr(state, BindingId::unknown(), &expr, ty);

                self.gen_binding_tuple_unpack_pattern(state, pattern, ty, ptr);
            }
            Pattern::Hybrid(pattern) => match &pattern.unpack {
                UnpackPatternKind::Struct(unpack) => match ty.maybe_deref_once() {
                    Type::Module(_) => {
                        self.gen_local_and_store_expr(state, pattern.name.id, &expr, ty);
                        self.gen_binding_module_unpack_pattern(state, unpack);
                    }
                    Type::Struct(struct_ty) => {
                        let ptr = self.gen_local_and_store_expr(state, pattern.name.id, &expr, ty);

                        self.gen_binding_struct_unpack_pattern(state, unpack, ty, &struct_ty, ptr);
                    }
                    ty => panic!("unexpected type: {}", ty),
                },
                UnpackPatternKind::Tuple(unpack) => {
                    let ptr = self.gen_local_and_store_expr(state, pattern.name.id, &expr, ty);
                    self.gen_binding_tuple_unpack_pattern(state, unpack, ty, ptr);
                }
            },
        }
    }

    pub fn gen_binding_module_unpack_pattern(
        &mut self,
        state: &mut CodegenState<'ctx>,
        pattern: &UnpackPattern,
    ) {
        for pattern in pattern.symbols.iter() {
            if pattern.ignore {
                continue;
            }

            let redirect_id = self
                .workspace
                .binding_infos
                .get(pattern.id)
                .unwrap()
                .redirects_to
                .unwrap();

            let decl = self.gen_top_level_binding(redirect_id);

            state.scopes.insert(pattern.id, decl);
        }
    }

    pub fn gen_binding_struct_unpack_pattern(
        &mut self,
        state: &mut CodegenState<'ctx>,
        pattern: &UnpackPattern,
        ty: &Type,
        struct_ty: &StructType,
        ptr: PointerValue<'ctx>,
    ) {
        let struct_llvm_type = Some(ty.llvm_type(self));

        for pattern in pattern.symbols.iter() {
            if pattern.ignore {
                continue;
            }

            let field_index = struct_ty.find_field_position(pattern.name).unwrap();

            let value = self.gen_struct_access(ptr.into(), field_index as u32, struct_llvm_type);

            self.gen_local_with_alloca(
                state,
                pattern.id,
                if ty.is_pointer() {
                    value
                } else {
                    self.build_load(value)
                },
            );
        }
    }

    pub fn gen_binding_tuple_unpack_pattern(
        &mut self,
        state: &mut CodegenState<'ctx>,
        pattern: &UnpackPattern,
        ty: &Type,
        ptr: PointerValue<'ctx>,
    ) {
        let llvm_type = Some(ty.llvm_type(self));

        for (i, pattern) in pattern.symbols.iter().enumerate() {
            if pattern.ignore {
                continue;
            }

            let value = self.gen_struct_access(ptr.into(), i as u32, llvm_type);

            self.gen_local_with_alloca(
                state,
                pattern.id,
                if ty.is_pointer() {
                    value
                } else {
                    self.build_load(value)
                },
            );
        }
    }

    pub fn gen_binding_pattern_with_value(
        &mut self,
        state: &mut CodegenState<'ctx>,
        pattern: &Pattern,
        ty: &Type,
        value: BasicValueEnum<'ctx>,
    ) {
        match pattern {
            Pattern::Name(symbol) => {
                self.gen_local_with_alloca(state, symbol.id, value);
            }
            Pattern::StructUnpack(pattern) => {
                self.gen_binding_struct_unpack_pattern_with_value(state, pattern, ty, value);
            }
            Pattern::TupleUnpack(pattern) => {
                self.gen_binding_tuple_unpack_pattern_with_value(state, pattern, ty, value);
            }
            Pattern::Hybrid(pattern) => {
                let ptr = self.gen_local_with_alloca(state, pattern.name.id, value);

                match &pattern.unpack {
                    UnpackPatternKind::Struct(unpack) => {
                        self.gen_binding_struct_unpack_pattern(
                            state,
                            unpack,
                            ty,
                            ty.maybe_deref_once().as_struct(),
                            ptr,
                        );
                    }
                    UnpackPatternKind::Tuple(unpack) => {
                        self.gen_binding_tuple_unpack_pattern(state, unpack, ty, ptr);
                    }
                }
            }
        }
    }

    pub fn gen_binding_struct_unpack_pattern_with_value(
        &mut self,
        state: &mut CodegenState<'ctx>,
        pattern: &UnpackPattern,
        ty: &Type,
        value: BasicValueEnum<'ctx>,
    ) {
        let struct_ty = ty.maybe_deref_once().into_struct();

        for i in 0..pattern.symbols.len() {
            let NamePattern {
                id: binding_id,
                name: symbol,
                ignore,
                ..
            } = pattern.symbols[i];

            if ignore {
                continue;
            }

            let field_index = struct_ty.find_field_position(symbol).unwrap();

            let llvm_type = Some(ty.llvm_type(self));

            let value = self.gen_struct_access(value, field_index as u32, llvm_type);

            let value = if ty.is_pointer() {
                value
            } else {
                self.build_load(value)
            };

            self.gen_local_with_alloca(state, binding_id, value);
        }
    }

    pub fn gen_binding_tuple_unpack_pattern_with_value(
        &mut self,
        state: &mut CodegenState<'ctx>,
        pattern: &UnpackPattern,
        ty: &Type,
        value: BasicValueEnum<'ctx>,
    ) {
        for i in 0..pattern.symbols.len() {
            let NamePattern {
                id: binding_id,
                ignore,
                ..
            } = pattern.symbols[i];

            if ignore {
                continue;
            }

            let llvm_type = Some(ty.llvm_type(self));

            let value = self.gen_struct_access(value, i as u32, llvm_type);

            let value = if ty.is_pointer() {
                value
            } else {
                self.build_load(value)
            };

            self.gen_local_with_alloca(state, binding_id, value);
        }
    }

    pub fn gen_local_uninit(
        &mut self,
        state: &mut CodegenState<'ctx>,
        id: BindingId,
        ty: &Type,
    ) -> PointerValue<'ctx> {
        let ty = ty.llvm_type(self);
        let ptr = self.build_alloca_named(state, ty, id);
        self.gen_local_inner(state, id, ptr);
        ptr
    }

    pub fn gen_local_with_alloca(
        &mut self,
        state: &mut CodegenState<'ctx>,
        id: BindingId,
        value: BasicValueEnum<'ctx>,
    ) -> PointerValue<'ctx> {
        let ptr = self.build_alloca_named(state, value.get_type(), id);
        self.build_store(ptr, value);
        self.gen_local_inner(state, id, ptr);
        ptr
    }

    pub fn gen_local_or_load_addr(
        &mut self,
        state: &mut CodegenState<'ctx>,
        id: BindingId,
        value: BasicValueEnum<'ctx>,
    ) -> PointerValue<'ctx> {
        if value.is_a_load_inst() {
            self.get_operand(value).into_pointer_value()
        } else {
            self.gen_local_with_alloca(state, id, value)
        }
    }

    pub fn build_alloca_or_load_addr(
        &mut self,
        state: &mut CodegenState<'ctx>,
        value: BasicValueEnum<'ctx>,
    ) -> PointerValue<'ctx> {
        if value.is_a_load_inst() {
            self.get_operand(value).into_pointer_value()
        } else {
            let ptr = self.build_alloca(state, value.get_type());
            self.build_store(ptr, value);
            ptr
        }
    }

    fn gen_local_inner(
        &mut self,
        state: &mut CodegenState<'ctx>,
        id: BindingId,
        ptr: PointerValue<'ctx>,
    ) {
        let name = self
            .workspace
            .binding_infos
            .get(id)
            .map_or(ustr(""), |b| b.name);

        ptr.set_name(&name);

        let align = align_of(
            ptr.get_type().get_element_type().try_into().unwrap(),
            self.target_metrics.word_size,
        );

        ptr.as_instruction_value()
            .unwrap()
            .set_alignment(align as u32)
            .unwrap();

        if id != BindingId::unknown() {
            state.scopes.insert(id, CodegenDecl::Local(ptr));
        }
    }

    pub fn gen_expr(
        &mut self,
        state: &mut CodegenState<'ctx>,
        expr: &ast::Ast,
        deref: bool,
    ) -> BasicValueEnum<'ctx> {
        if self.current_block().get_terminator().is_some() {
            return self.gen_unit();
        }

        let value = match expr {
            ast::Ast::Binding(binding) => {
                match &binding.kind {
                    ast::BindingKind::Normal => {
                        self.gen_binding_pattern_with_expr(
                            state,
                            &binding.pattern,
                            &binding.ty.normalize(self.tycx),
                            &binding.value,
                        );
                    }
                    ast::BindingKind::Extern(_) | ast::BindingKind::Intrinsic(_) => {
                        let pattern = binding.pattern.as_name_ref();

                        let decl = if let Some(expr) = &binding.value {
                            if let ast::Ast::Const(ast::Const {
                                value: ConstValue::Function(function),
                                ..
                            }) = expr.as_ref()
                            {
                                let function = self.gen_function(function.id, None);
                                CodegenDecl::Function(function)
                            } else {
                                self.declare_global_binding(pattern.id, binding)
                            }
                        } else {
                            self.declare_global_binding(pattern.id, binding)
                        };

                        state.scopes.insert(pattern.id, decl);
                    }
                }

                self.gen_unit()
            }
            ast::Ast::Assignment(assignment) => {
                let left = self
                    .gen_expr(state, &assignment.lhs, false)
                    .into_pointer_value();

                let right = self.gen_expr(state, &assignment.rhs, true);

                // println!("left: {:#?}", left);
                // println!("right: {:#?}", right);

                self.build_store(left, right);

                self.gen_unit()
            }
            ast::Ast::Cast(info) => self.gen_cast(state, &info.expr, &info.ty),
            ast::Ast::Builtin(builtin) => match &builtin.kind {
                ast::BuiltinKind::Import(_) => self.gen_unit(), // panic!("unexpected import builtin"),
                ast::BuiltinKind::SizeOf(expr) => match expr.ty().normalize(self.tycx) {
                    Type::Type(ty) => ty.llvm_type(self).size_of().unwrap().into(),
                    ty => unreachable!("got {}", ty),
                },
                ast::BuiltinKind::AlignOf(expr) => match expr.ty().normalize(self.tycx) {
                    Type::Type(ty) => ty.llvm_type(self).align_of().into(),
                    ty => unreachable!("got {}", ty),
                },
                ast::BuiltinKind::Panic(msg_expr) => {
                    let message = if let Some(msg_expr) = msg_expr {
                        self.gen_expr(state, msg_expr, true)
                    } else {
                        self.const_str_slice("", "").into()
                    };

                    self.gen_panic(state, message, expr.span());
                    self.gen_unit()
                }
                ast::BuiltinKind::Run(_) => {
                    todo!("remove this")
                    // let ty = expr.ty().normalize(self.tycx);
                    // let value = self.gen_const_value(Some(state), result.as_ref().unwrap(), &ty);

                    // if deref {
                    //     self.build_load(value)
                    // } else {
                    //     value
                    // }
                }
            },
            ast::Ast::Function(_) => {
                panic!("should've been lowered to ConstValue::Function")
                // let function = self.gen_fn_expr(state.module_info, func, Some(state.clone()));

                // self.start_block(state, state.curr_block);

                // function.as_global_value().as_pointer_value().into()
            }
            ast::Ast::While(while_) => {
                let loop_head = self.append_basic_block(state, "loop_head");
                let loop_body = self.append_basic_block(state, "loop_body");
                let loop_exit = self.append_basic_block(state, "loop_exit");

                self.builder.build_unconditional_branch(loop_head);
                self.start_block(state, loop_head);

                let condition = self
                    .gen_expr(state, &while_.condition, true)
                    .into_int_value();

                self.builder
                    .build_conditional_branch(condition, loop_body, loop_exit);

                self.start_block(state, loop_body);

                state.loop_blocks.push(LoopBlock {
                    head: loop_head,
                    exit: loop_exit,
                });

                self.gen_block(state, &while_.block, false);

                state.loop_blocks.pop();

                if self.current_block().get_terminator().is_none() {
                    self.builder.build_unconditional_branch(loop_head);
                }

                self.start_block(state, loop_exit);

                return self.gen_unit();
            }
            ast::Ast::For(for_) => {
                let loop_head = self.append_basic_block(state, "loop_head");
                let loop_body = self.append_basic_block(state, "loop_body");
                let loop_exit = self.append_basic_block(state, "loop_exit");

                let (start, end) = match &for_.iterator {
                    ast::ForIter::Range(start, end) => {
                        let start = self.gen_expr(state, start, true).into_int_value();
                        let end = self.gen_expr(state, end, true).into_int_value();
                        (start, end)
                    }
                    ast::ForIter::Value(value, ..) => {
                        let start = self.ptr_sized_int_type.const_zero();
                        let end = match value.ty().normalize(self.tycx).maybe_deref_once() {
                            Type::Array(_, len) => {
                                self.ptr_sized_int_type.const_int(len as u64, false)
                            }
                            Type::Slice(..) => {
                                let agg = self.gen_expr(state, value, true);
                                self.gen_load_slice_len(agg)
                            }
                            ty => unreachable!("unexpected type `{}`", ty),
                        };

                        (start, end)
                    }
                };

                state.push_scope();

                let it = match &for_.iterator {
                    ast::ForIter::Range(_, _) => {
                        self.gen_local_with_alloca(state, for_.iter_binding.id, start.into())
                    }
                    ast::ForIter::Value(value) => {
                        // TODO: remove by_ref?
                        let by_ref = value.ty().normalize(self.tycx).is_pointer();

                        let agg = self.gen_expr(state, value, false).into_pointer_value();
                        let agg = self.maybe_load_double_pointer(agg);

                        let item = self.gen_subscript(
                            agg.into(),
                            &value.ty().normalize(self.tycx),
                            start,
                            !by_ref,
                        );

                        self.gen_local_with_alloca(state, for_.iter_binding.id, item)
                    }
                };

                let it_index = self.gen_local_with_alloca(
                    state,
                    for_.index_binding
                        .as_ref()
                        .map_or(BindingId::unknown(), |x| x.id),
                    start.into(),
                );

                self.builder.build_unconditional_branch(loop_head);
                self.start_block(state, loop_head);

                let curr_index = self.build_load(it_index.into()).into_int_value();

                let continue_condition = self.builder.build_int_compare(
                    match &for_.iterator {
                        ast::ForIter::Range(..) => IntPredicate::SLE,
                        ast::ForIter::Value(..) => IntPredicate::SLT,
                    },
                    curr_index,
                    end,
                    "for_loop_cond",
                );

                self.builder
                    .build_conditional_branch(continue_condition, loop_body, loop_exit);

                self.start_block(state, loop_body);

                state.loop_blocks.push(LoopBlock {
                    head: loop_head,
                    exit: loop_exit,
                });

                self.gen_block(state, &for_.block, false);

                if self.current_block().get_terminator().is_none() {
                    let step = start.get_type().const_int(1, true);

                    let next_index = self
                        .builder
                        .build_int_add(curr_index, step, "for_next_index");

                    self.build_store(it_index, next_index.into());

                    match &for_.iterator {
                        ast::ForIter::Range(_, _) => {
                            let it_value = self.build_load(it.into()).into_int_value();
                            let next_it =
                                self.builder.build_int_add(it_value, step, "for_next_index");

                            self.build_store(it, next_it.into());
                        }
                        ast::ForIter::Value(value) => {
                            let by_ref = value.ty().normalize(self.tycx).is_pointer();

                            let agg = self.gen_expr(state, value, false).into_pointer_value();
                            let agg = self.maybe_load_double_pointer(agg);

                            let item = self.gen_subscript(
                                agg.into(),
                                &value.ty().normalize(self.tycx),
                                next_index,
                                !by_ref,
                            );

                            self.build_store(it, item);
                        }
                    }

                    self.builder.build_unconditional_branch(loop_head);
                }

                self.start_block(state, loop_exit);

                state.loop_blocks.pop();
                state.pop_scope();

                self.gen_unit()
            }
            ast::Ast::Break(_) => {
                let exit_block = state.loop_blocks.last().unwrap().exit;
                self.builder.build_unconditional_branch(exit_block);

                self.gen_unit()
            }
            ast::Ast::Continue(_) => {
                let head_block = state.loop_blocks.last().unwrap().head;
                self.builder.build_unconditional_branch(head_block);

                self.gen_unit()
            }
            ast::Ast::Return(ret) => {
                let value = ret
                    .expr
                    .as_ref()
                    .map(|expr| self.gen_expr(state, expr, true));

                self.gen_return(state, value);

                self.gen_unit()
            }
            ast::Ast::If(if_) => self.gen_if_expr(state, if_),
            ast::Ast::Block(block) => self.gen_block(state, block, deref),
            ast::Ast::Binary(binary) => self.gen_binary(state, binary),
            ast::Ast::Unary(unary) => self.gen_unary(state, unary, deref),
            ast::Ast::Subscript(sub) => {
                let value = self.gen_expr(state, &sub.expr, false);
                let index = self.gen_expr(state, &sub.index, true).into_int_value();

                let ty = sub.expr.ty().normalize(self.tycx);

                let len = match ty.maybe_deref_once() {
                    Type::Array(_, size) => Some(index.get_type().const_int(size as _, false)),
                    Type::Slice(..) => Some(self.gen_load_slice_len(value)),
                    Type::MultiPointer(..) => None,
                    ty => unreachable!("got {}", ty),
                };

                if let Some(len) = len {
                    self.gen_runtime_check_index_out_of_bounds(state, index, len, expr.span());
                }

                self.gen_subscript(value, &ty, index, deref)
            }
            ast::Ast::Slice(slice) => {
                let ptr = self
                    .gen_expr(state, &slice.expr, false)
                    .into_pointer_value();
                let ty = slice.expr.ty().normalize(self.tycx);

                let data = match ty {
                    Type::Slice(..) => {
                        let data = self.gen_load_slice_data(ptr.into());
                        self.build_load(data.into()).into_pointer_value()
                    }
                    Type::MultiPointer(..) => self.build_load(ptr.into()).into_pointer_value(),
                    Type::Array(..) => ptr,
                    _ => unreachable!("got {}", ty),
                };

                let low = match &slice.low {
                    Some(low) => self.gen_expr(state, low, true).into_int_value(),
                    None => match &slice.high {
                        Some(high) => high.ty().llvm_type(self).into_int_type(),
                        None => self.ptr_sized_int_type,
                    }
                    .const_zero(),
                };

                let high = match &slice.high {
                    Some(high) => self.gen_expr(state, high, true).into_int_value(),
                    None => match ty {
                        Type::Array(_, len) => low.get_type().const_int(len as u64, false),
                        Type::Slice(..) => self.gen_load_slice_len(ptr.into()),
                        _ => unreachable!(),
                    },
                };

                self.gen_runtime_check_slice_end_before_start(state, low, high, expr.span());

                let len = match ty {
                    Type::Slice(..) => Some(self.gen_load_slice_len(ptr.into())),
                    Type::Array(_, size) => {
                        Some(self.ptr_sized_int_type.const_int(size as _, false))
                    }
                    Type::MultiPointer(..) => None,
                    _ => unreachable!("got {}", ty),
                };

                if let Some(len) = len {
                    self.gen_runtime_check_slice_range_out_of_bounds(
                        state,
                        low,
                        high,
                        len,
                        expr.span(),
                    );
                }

                let slice_llvm_ty = expr.ty().llvm_type(self);
                let slice_ptr = self.build_alloca(state, slice_llvm_ty);

                self.gen_slice(
                    slice_ptr,
                    data.into(),
                    low,
                    high,
                    ty.element_type().unwrap(),
                );

                if deref {
                    self.build_load(slice_ptr.into())
                } else {
                    slice_ptr.into()
                }
            }
            ast::Ast::Call(call) => self.gen_fn_call_expr(state, call),
            ast::Ast::MemberAccess(access) => {
                let value = self.gen_expr(state, &access.expr, false);
                let accessed_ty = access.expr.ty().normalize(self.tycx);

                let value = if accessed_ty.is_pointer() {
                    self.build_load(value)
                } else {
                    value
                };

                let derefed_ty = accessed_ty.maybe_deref_once();
                match derefed_ty {
                    Type::Tuple(_) => {
                        let index = access.member.parse::<usize>().unwrap();
                        let llvm_ty = Some(derefed_ty.llvm_type(self));
                        let value = self.gen_struct_access(value, index as u32, llvm_ty);

                        if deref {
                            self.build_load(value)
                        } else {
                            value
                        }
                    }
                    Type::Struct(ref struct_ty) => {
                        let struct_llvm_ty = Some(derefed_ty.llvm_type(self));

                        if struct_ty.is_union() {
                            let field = struct_ty
                                .fields
                                .iter()
                                .find(|f| f.name == access.member)
                                .unwrap();

                            let field_ty = field.ty.llvm_type(self);

                            let casted_ptr = self.builder.build_pointer_cast(
                                value.into_pointer_value(),
                                field_ty.ptr_type(AddressSpace::Generic),
                                "",
                            );

                            let value = casted_ptr.into();

                            if deref {
                                self.build_load(value)
                            } else {
                                value
                            }
                        } else {
                            let field_index = struct_ty.find_field_position(access.member).unwrap();
                            let value =
                                self.gen_struct_access(value, field_index as u32, struct_llvm_ty);

                            if deref {
                                self.build_load(value)
                            } else {
                                value
                            }
                        }
                    }
                    Type::Array(_, len) => match access.member.as_str() {
                        BUILTIN_FIELD_LEN => {
                            self.ptr_sized_int_type.const_int(len as _, false).into()
                        }
                        _ => unreachable!("got field `{}`", access.member),
                    },
                    Type::Slice(..) => match access.member.as_str() {
                        BUILTIN_FIELD_LEN => self.gen_load_slice_len(value).into(),
                        BUILTIN_FIELD_DATA => self
                            .maybe_load_double_pointer(self.gen_load_slice_data(value))
                            .into(),
                        _ => unreachable!("got field `{}`", access.member),
                    },
                    Type::Module(module_id) => {
                        let id = self
                            .workspace
                            .binding_infos
                            .iter()
                            .position(|(_, info)| {
                                info.module_id == module_id && info.name == access.member
                            })
                            .map(BindingId::from)
                            .unwrap_or_else(|| {
                                panic!(
                                    "couldn't find member `{}` in module `{}`",
                                    self.workspace.module_infos.get(module_id).unwrap().name,
                                    access.member
                                )
                            });

                        let decl = self.gen_top_level_binding(id);

                        let ptr = decl.into_pointer_value();

                        if deref {
                            self.build_load(ptr.into())
                        } else {
                            ptr.into()
                        }
                    }
                    _ => unreachable!("invalid ty `{}`", accessed_ty),
                }
            }
            ast::Ast::Ident(ident) => {
                assert!(ident.binding_id != BindingId::unknown());

                let decl = match state.scopes.get(ident.binding_id) {
                    Some((_, decl)) => decl.clone(),
                    None => self.gen_top_level_binding(ident.binding_id),
                };

                let ptr = decl.into_pointer_value();

                if deref {
                    self.build_load(ptr.into())
                } else {
                    ptr.into()
                }
            }
            ast::Ast::ArrayLiteral(lit) => {
                let ty = expr.ty().normalize(self.tycx);
                let array_ptr = match &lit.kind {
                    ast::ArrayLiteralKind::List(elements) => {
                        let elements: Vec<BasicValueEnum> = elements
                            .iter()
                            .map(|e| self.gen_expr(state, e, true))
                            .collect();

                        let size = match &ty {
                            Type::Array(_, size) => *size,
                            _ => unreachable!("got ty `{}`", ty),
                        };

                        let llvm_ty = ty.llvm_type(self);
                        let array_ptr = self.build_alloca(state, llvm_ty);

                        for index in 0..size {
                            let access = unsafe {
                                self.builder.build_in_bounds_gep(
                                    array_ptr,
                                    &[
                                        self.context.i32_type().const_zero(),
                                        self.context.i32_type().const_int(index as u64, true),
                                    ],
                                    "",
                                )
                            };

                            self.build_store(access, elements[index]);
                        }

                        array_ptr.into()
                    }
                    ast::ArrayLiteralKind::Fill {
                        expr: element_expr,
                        len: _,
                    } => {
                        let value = self.gen_expr(state, element_expr, true);

                        let array_type = ty.llvm_type(self);
                        let element_type = array_type.into_array_type().get_element_type();
                        let array_ptr = self.build_alloca(state, array_type);

                        let src_ptr = self.build_alloca_or_load_addr(state, value);

                        let loop_head = self.append_basic_block(state, "array_fill_head");
                        let loop_body = self.append_basic_block(state, "array_fill_body");
                        let loop_exit = self.append_basic_block(state, "array_fill_exit");

                        let start = self.ptr_sized_int_type.const_zero();
                        let end = self
                            .ptr_sized_int_type
                            .const_int(array_type.into_array_type().len() as _, false);

                        let it = self.build_alloca(state, start.get_type().into());
                        self.build_store(it, start.into());

                        self.builder.build_unconditional_branch(loop_head);
                        self.start_block(state, loop_head);

                        let it_value = self.build_load(it.into()).into_int_value();

                        let condition =
                            self.builder
                                .build_int_compare(IntPredicate::SLT, it_value, end, "");

                        self.builder
                            .build_conditional_branch(condition, loop_body, loop_exit);

                        self.start_block(state, loop_body);

                        let dst_ptr = unsafe {
                            self.builder.build_in_bounds_gep(
                                array_ptr,
                                &[self.ptr_sized_int_type.const_zero(), it_value],
                                "",
                            )
                        };

                        let sz = size_of(element_type, self.target_metrics.word_size);
                        self.build_copy_nonoverlapping(
                            src_ptr,
                            dst_ptr,
                            self.ptr_sized_int_type.const_int(sz as _, false),
                        );

                        let next_it = self.builder.build_int_add(
                            it_value,
                            it_value.get_type().const_int(1, false),
                            "",
                        );

                        self.build_store(it, next_it.into());

                        self.builder.build_unconditional_branch(loop_head);

                        self.start_block(state, loop_exit);

                        array_ptr.into()
                    }
                };

                if deref {
                    self.build_load(array_ptr)
                } else {
                    array_ptr
                }
            }
            ast::Ast::TupleLiteral(lit) => {
                let ty = expr.ty().normalize(self.tycx);
                if ty.is_type() {
                    return self.gen_unit();
                }

                let values: Vec<BasicValueEnum> = lit
                    .elements
                    .iter()
                    .map(|e| self.gen_expr(state, e, true))
                    .collect();

                let llvm_ty = expr.ty().llvm_type(self);
                let tuple = self.build_alloca(state, llvm_ty);

                for (i, value) in values.iter().enumerate() {
                    let ptr = self.builder.build_struct_gep(tuple, i as u32, "").unwrap();
                    self.build_store(ptr, *value);
                }

                if deref {
                    self.build_load(tuple.into())
                } else {
                    tuple.into()
                }
            }
            ast::Ast::StructLiteral(lit) => {
                self.gen_struct_literal(state, &expr.ty().normalize(self.tycx), &lit.fields, deref)
            }

            ast::Ast::Literal(_) => {
                panic!("Literal expression should have been lowered to a ConstValue")
            }

            ast::Ast::PointerType(..)
            | ast::Ast::MultiPointerType(..)
            | ast::Ast::ArrayType(..)
            | ast::Ast::SliceType(..)
            | ast::Ast::StructType(..)
            | ast::Ast::SelfType(..)
            | ast::Ast::Placeholder(..)
            | ast::Ast::FunctionType(..) => self.gen_unit(),

            ast::Ast::Const(const_value) => {
                let ty = expr.ty().normalize(self.tycx);
                self.gen_const_value(Some(state), &const_value.value, &ty)
            }

            ast::Ast::Error(_) => panic!("unexpected error node"),
        };

        if expr.ty().normalize(self.tycx).is_never() {
            self.build_unreachable();
        }

        value
    }

    pub fn gen_block(
        &mut self,
        state: &mut CodegenState<'ctx>,
        block: &ast::Block,
        deref: bool,
    ) -> BasicValueEnum<'ctx> {
        let mut yielded_value = self.gen_unit();

        state.push_scope();

        for (i, expr) in block.statements.iter().enumerate() {
            let value = self.gen_expr(state, expr, true);
            if block.yields && i == block.statements.len() - 1 {
                yielded_value = value;
            }
        }

        state.pop_scope();

        if deref && !block.statements.is_empty() && block.yields {
            self.build_load(yielded_value)
        } else {
            yielded_value
        }
    }

    pub fn gen_const_value(
        &mut self,
        state: Option<&CodegenState<'ctx>>,
        const_value: &ConstValue,
        ty: &Type,
    ) -> BasicValueEnum<'ctx> {
        match const_value {
            ConstValue::Unit(_) | ConstValue::Type(_) => self.gen_unit(),
            ConstValue::Bool(v) => self
                .context
                .bool_type()
                .const_int(if *v { 1 } else { 0 }, false)
                .into(),
            ConstValue::Int(v) => {
                if ty.is_any_integer() {
                    ty.llvm_type(self)
                        .into_int_type()
                        .const_int(*v as u64, ty.is_signed_int())
                        .into()
                } else {
                    ty.llvm_type(self)
                        .into_float_type()
                        .const_float(*v as f64)
                        .into()
                }
            }
            ConstValue::Uint(v) => {
                if ty.is_any_integer() {
                    ty.llvm_type(self)
                        .into_int_type()
                        .const_int(*v, ty.is_signed_int())
                        .into()
                } else {
                    ty.llvm_type(self)
                        .into_float_type()
                        .const_float(*v as f64)
                        .into()
                }
            }
            ConstValue::Float(v) => ty
                .llvm_type(self)
                .into_float_type()
                .const_float(*v as f64)
                .into(),
            ConstValue::Str(v) => self.const_str_slice("", *v).into(),
            ConstValue::Array(array) => {
                let el_ty = array.element_ty.normalize(self.tycx);

                let values: Vec<BasicValueEnum> = array
                    .values
                    .iter()
                    .map(|v| self.gen_const_value(state, v, &el_ty))
                    .collect();

                el_ty.llvm_type(self).const_array(&values).into()
            }
            ConstValue::Tuple(elements) => {
                let values = elements
                    .iter()
                    .map(|el| self.gen_const_value(state, &el.value, &el.ty.normalize(self.tycx)))
                    .collect::<Vec<BasicValueEnum>>();

                self.const_struct(&values).into()
            }
            ConstValue::Struct(fields) => {
                let values = fields
                    .values()
                    .map(|el| self.gen_const_value(state, &el.value, &el.ty.normalize(self.tycx)))
                    .collect::<Vec<BasicValueEnum>>();

                // self.context.const_struct(&values, false).into();
                ty.llvm_type(self)
                    .into_struct_type()
                    .const_named_struct(&values)
                    .into()
            }
            ConstValue::Function(f) => {
                let function = self.gen_function(f.id, state.cloned());
                function.as_global_value().as_pointer_value().into()
                // let decl = match state.and_then(|s| s.scopes.get(f.id)) {
                //     Some((_, decl)) => decl.clone(),
                //     None => self.gen_top_level_binding(f.id),
                // };

                // decl.into_pointer_value().into()
            }
        }
    }

    pub fn gen_local_and_store_expr(
        &mut self,
        state: &mut CodegenState<'ctx>,
        id: BindingId,
        expr: &Option<Box<ast::Ast>>,
        ty: &Type,
    ) -> PointerValue<'ctx> {
        if let Some(expr) = expr {
            let value = self.gen_expr(state, expr, true);
            let ptr = self.gen_local_or_load_addr(state, id, value);
            state.scopes.insert(id, CodegenDecl::Local(ptr));
            ptr
        } else {
            self.gen_local_uninit(state, id, ty)
        }
    }

    pub fn gen_cast(
        &mut self,
        state: &mut CodegenState<'ctx>,
        expr: &Box<ast::Ast>,
        target_ty: &TypeId,
    ) -> BasicValueEnum<'ctx> {
        let value = self.gen_expr(state, expr, true);
        self.gen_cast_inner(
            state,
            value,
            &expr.ty().normalize(self.tycx),
            &target_ty.normalize(self.tycx),
        )
    }

    fn gen_cast_inner(
        &mut self,
        state: &mut CodegenState<'ctx>,
        value: BasicValueEnum<'ctx>,
        from_ty: &Type,
        target_ty: &Type,
    ) -> BasicValueEnum<'ctx> {
        if from_ty == target_ty {
            return value;
        }

        const INST_NAME: &str = "cast";

        let cast_type = target_ty.llvm_type(self);

        match (from_ty, target_ty) {
            (Type::Bool, Type::Int(_)) | (Type::Bool, Type::Uint(_)) => self
                .builder
                .build_int_z_extend(value.into_int_value(), cast_type.into_int_type(), INST_NAME)
                .into(),
            (Type::Int(_) | Type::Uint(_), Type::Int(_) | Type::Uint(_)) => self
                .builder
                .build_int_cast(value.into_int_value(), cast_type.into_int_type(), INST_NAME)
                .into(),

            (Type::Int(_), Type::Float(_)) => self
                .builder
                .build_signed_int_to_float(
                    value.into_int_value(),
                    cast_type.into_float_type(),
                    INST_NAME,
                )
                .into(),

            (Type::Uint(_), Type::Float(_)) => self
                .builder
                .build_unsigned_int_to_float(
                    value.into_int_value(),
                    cast_type.into_float_type(),
                    INST_NAME,
                )
                .into(),

            (Type::Float(_), Type::Int(_)) => self
                .builder
                .build_float_to_signed_int(
                    value.into_float_value(),
                    cast_type.into_int_type(),
                    INST_NAME,
                )
                .into(),
            (Type::Float(_), Type::Uint(_)) => self
                .builder
                .build_float_to_unsigned_int(
                    value.into_float_value(),
                    cast_type.into_int_type(),
                    INST_NAME,
                )
                .into(),
            (Type::Float(_), Type::Float(_)) => self
                .builder
                .build_float_cast(
                    value.into_float_value(),
                    cast_type.into_float_type(),
                    INST_NAME,
                )
                .into(),

            (
                Type::Pointer(..) | Type::MultiPointer(..),
                Type::Pointer(..) | Type::MultiPointer(..),
            ) => self
                .builder
                .build_pointer_cast(
                    value.into_pointer_value(),
                    cast_type.into_pointer_type(),
                    INST_NAME,
                )
                .into(),

            // pointer <=> int | uint
            (Type::Pointer(..), Type::Int(..) | Type::Uint(..)) => self
                .builder
                .build_ptr_to_int(
                    value.into_pointer_value(),
                    cast_type.into_int_type(),
                    INST_NAME,
                )
                .into(),

            // int | uint <=> pointer
            (Type::Int(..) | Type::Uint(..), Type::Pointer(..)) => self
                .builder
                .build_int_to_ptr(
                    value.into_int_value(),
                    cast_type.into_pointer_type(),
                    INST_NAME,
                )
                .into(),

            (Type::Pointer(t, _), Type::Slice(t_slice, ..)) => match t.as_ref() {
                Type::Array(_, size) => {
                    let slice_ty = self.slice_type(t_slice);
                    let ptr = self.build_alloca(state, slice_ty);

                    self.gen_slice(
                        ptr,
                        value,
                        self.ptr_sized_int_type.const_zero(),
                        self.ptr_sized_int_type.const_int(*size as u64, false),
                        t_slice.as_ref(),
                    );

                    self.build_load(ptr.into())
                }
                _ => unreachable!(),
            },

            _ => unreachable!("can't cast {} to {}", from_ty, target_ty),
        }
    }

    pub fn gen_return(
        &mut self,
        state: &mut CodegenState<'ctx>,
        value: Option<BasicValueEnum<'ctx>>,
    ) {
        let abi_fn = self.get_abi_compliant_fn(&state.fn_type);

        if abi_fn.ret.kind.is_indirect() {
            let return_ptr = state.return_ptr.unwrap();

            match value {
                Some(value) => {
                    let size = size_of(abi_fn.ret.ty, self.target_metrics.word_size);
                    if value.is_a_load_inst() && size > self.target_metrics.word_size {
                        let ptr = self.build_alloca_or_load_addr(state, value);
                        self.build_copy_nonoverlapping(
                            ptr,
                            return_ptr,
                            self.ptr_sized_int_type.const_int(size as u64, true),
                        );
                    } else {
                        self.build_store(return_ptr, value);
                    }
                }
                _ => {
                    self.build_store(return_ptr, self.gen_unit());
                }
            };

            self.builder.build_return(None);
        } else {
            let value = value.unwrap_or_else(|| self.gen_unit());

            let value = self.build_transmute(
                state,
                value,
                state.function.get_type().get_return_type().unwrap(),
            );

            self.builder.build_return(Some(&value));
        }
    }
}
