use crate::ty::IntoLlvmType;

use super::{
    abi::{align_of, size_of, AbiFn},
    util::is_a_load_inst,
};
use chili_ast::{
    ast,
    pattern::{Pattern, SymbolPattern},
    workspace::{BindingInfo, BindingInfoId, ModuleId, ModuleInfo},
};
use chili_ast::{ty::*, workspace::Workspace};
use chili_check::{normalize::NormalizeTy, ty_ctx::TyCtx};
use common::{
    builtin::{BUILTIN_FIELD_DATA, BUILTIN_FIELD_LEN},
    scopes::Scopes,
    target::TargetMetrics,
};
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
    Local(PointerValue<'ctx>),
    Global(GlobalValue<'ctx>),
    Module(ModuleInfo),
}

impl<'ctx> CodegenDecl<'ctx> {
    pub fn into_pointer_value(&self) -> PointerValue<'ctx> {
        match self {
            CodegenDecl::Function(f) => f.as_global_value().as_pointer_value(),
            CodegenDecl::Local(p) => *p,
            CodegenDecl::Global(g) => g.as_pointer_value(),
            CodegenDecl::Module(..) => {
                panic!("can't get the pointer value of a module")
            }
        }
    }

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
}

pub struct Codegen<'cg, 'ctx> {
    pub workspace: &'cg Workspace,
    pub tycx: &'cg TyCtx,
    pub ast: &'cg ast::TypedAst,

    pub target_metrics: TargetMetrics,

    pub context: &'ctx Context,
    pub module: &'cg Module<'ctx>,
    pub fpm: &'cg PassManager<FunctionValue<'ctx>>,
    pub builder: &'cg Builder<'ctx>,
    pub ptr_sized_int_type: IntType<'ctx>,

    pub global_decls: HashMap<BindingInfoId, CodegenDecl<'ctx>>,
    pub types: HashMap<BindingInfoId, BasicTypeEnum<'ctx>>,
    pub fn_types: HashMap<FnTy, AbiFn<'ctx>>,
    pub static_strs: UstrMap<PointerValue<'ctx>>,
}

#[derive(Clone)]
pub(super) struct CodegenState<'ctx> {
    pub(super) module_info: ModuleInfo,
    pub(super) function: FunctionValue<'ctx>,
    pub(super) fn_type: FnTy,
    pub(super) return_ptr: Option<PointerValue<'ctx>>,
    pub(super) loop_blocks: Vec<LoopBlock<'ctx>>,
    pub(super) decl_block: BasicBlock<'ctx>,
    pub(super) curr_block: BasicBlock<'ctx>,
    pub(super) scopes: Scopes<BindingInfoId, CodegenDecl<'ctx>>, // TODO: switch to BindingInfoId
}

impl<'ctx> CodegenState<'ctx> {
    pub(super) fn new(
        module_info: ModuleInfo,
        function: FunctionValue<'ctx>,
        fn_type: FnTy,
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
            scopes: Scopes::new(),
        }
    }

    pub(super) fn push_scope(&mut self) {
        self.scopes.push_scope();
    }

    pub(super) fn pop_scope(&mut self) {
        self.scopes.pop_scope();
    }
}

#[derive(Clone, Copy)]
pub(super) struct LoopBlock<'ctx> {
    head: BasicBlock<'ctx>,
    exit: BasicBlock<'ctx>,
}

impl<'w, 'cg, 'ctx> Codegen<'cg, 'ctx> {
    pub fn start(&mut self) {
        let ep_id = self.workspace.entry_point_function_id.unwrap();
        self.gen_top_level_binding(ep_id);

        let ep_function = self.global_decls.get(&ep_id).unwrap().into_function_value();
        self.gen_entry_point_function(ep_function);
    }

    pub(super) fn find_or_gen_top_level_binding(&mut self, id: BindingInfoId) -> CodegenDecl<'ctx> {
        match self.global_decls.get(&id) {
            Some(decl) => decl.clone(),
            None => self.gen_top_level_binding(id),
        }
    }

    pub(super) fn gen_top_level_binding(&mut self, id: BindingInfoId) -> CodegenDecl<'ctx> {
        if let Some(binding) = self.ast.bindings.get(&id) {
            let module_info = *self.workspace.get_module_info(binding.module_id).unwrap();

            match binding.expr.as_ref() {
                Some(expr) => match &expr.kind {
                    ast::ExprKind::Fn(func) => {
                        let function = self.declare_fn_sig(module_info, &func.sig);
                        let decl = CodegenDecl::Function(function);
                        self.global_decls.insert(id, decl);
                        self.gen_fn(module_info, func, None);
                        decl
                    }
                    ast::ExprKind::FnType(sig) => {
                        let function = self.declare_fn_sig(module_info, sig);
                        let decl = CodegenDecl::Function(function);
                        self.global_decls.insert(id, decl);
                        decl
                    }
                    _ => self.declare_global(id, binding),
                },
                None => self.declare_global(id, binding),
            }
        } else if let Some(import) = self.ast.imports.get(&id) {
            self.gen_import(import)
        } else {
            unreachable!("{:?}", id)
        }
    }

    pub(super) fn gen_import(&mut self, import: &ast::Import) -> CodegenDecl<'ctx> {
        self.gen_top_level_binding(import.target_binding_info_id.unwrap())
    }

    pub(super) fn declare_global(
        &mut self,
        id: BindingInfoId,
        binding: &ast::Binding,
    ) -> CodegenDecl<'ctx> {
        // forward declare the global value, i.e: `let answer = 42`
        // the global value will is initialized by the entry point function
        let ty = binding.ty.llvm_type(self);

        let global_value = if binding.lib_name.is_some() {
            self.add_global_uninit(id, ty, Linkage::External)
        } else {
            self.add_global(id, ty, Linkage::Private)
        };

        let decl = CodegenDecl::Global(global_value);
        self.global_decls.insert(id, decl);

        decl
    }

    pub(super) fn add_global(
        &mut self,
        id: BindingInfoId,
        ty: BasicTypeEnum<'ctx>,
        linkage: Linkage,
    ) -> GlobalValue<'ctx> {
        let global_value = self.add_global_uninit(id, ty, linkage);
        global_value.set_initializer(&ty.const_zero());
        global_value
    }

    pub(super) fn add_global_uninit(
        &mut self,
        id: BindingInfoId,
        ty: BasicTypeEnum<'ctx>,
        linkage: Linkage,
    ) -> GlobalValue<'ctx> {
        let binding_info = self.workspace.get_binding_info(id).unwrap();
        let global_value = self.module.add_global(ty, None, &binding_info.symbol);
        global_value.set_linkage(linkage);
        global_value
    }

    pub(super) fn find_binding_info_by_name(
        &mut self,
        module_name: impl Into<Ustr>,
        symbol: impl Into<Ustr>,
    ) -> &BindingInfo {
        let module_name: Ustr = module_name.into();
        let symbol: Ustr = symbol.into();

        let module_id = ModuleId(
            self.workspace
                .module_infos
                .iter()
                .position(|m| m.name == module_name)
                .expect(&format!("couldn't find {}", module_name)),
        );

        self.workspace
            .binding_infos
            .iter()
            .find(|b| {
                !b.is_temp_recursive_function_binding()
                    && b.module_id == module_id
                    && b.symbol == symbol
            })
            .expect(&format!("couldn't find {} in {}", symbol, module_name))
    }

    pub(super) fn find_decl_by_name(
        &mut self,
        module_name: impl Into<Ustr>,
        symbol: impl Into<Ustr>,
    ) -> CodegenDecl<'ctx> {
        let binding_info = self.find_binding_info_by_name(module_name, symbol);
        let id = binding_info.id;
        self.global_decls
            .get(&id)
            .cloned()
            .unwrap_or_else(|| self.gen_top_level_binding(id))
    }

    pub(super) fn gen_binding_pattern_with_expr(
        &mut self,
        state: &mut CodegenState<'ctx>,
        pattern: &Pattern,
        ty: &TyKind,
        expr: &Option<ast::Expr>,
    ) {
        if ty.is_type() || ty.is_module() {
            return;
        }

        match pattern {
            Pattern::Single(SymbolPattern {
                binding_info_id, ..
            }) => {
                self.gen_local_and_store_expr(state, *binding_info_id, &expr, ty);
            }
            Pattern::StructUnpack(pattern) => {
                let ptr = self.gen_local_and_store_expr(state, BindingInfoId::unknown(), &expr, ty);

                let ty = ty.normalize(self.tycx);
                let struct_ty = ty.maybe_deref_once().into_struct().clone();

                for SymbolPattern {
                    binding_info_id,
                    symbol,
                    ignore,
                    ..
                } in pattern.symbols.iter()
                {
                    if *ignore {
                        continue;
                    }

                    let field_index = struct_ty
                        .fields
                        .iter()
                        .position(|f| f.symbol == *symbol)
                        .unwrap();

                    let llvm_type = Some(ty.llvm_type(self));
                    let value = self.gen_struct_access(ptr.into(), field_index as u32, llvm_type);

                    self.gen_local_with_alloca(
                        state,
                        *binding_info_id,
                        if ty.is_pointer() {
                            value
                        } else {
                            self.build_load(value)
                        },
                    );
                }
            }
            Pattern::TupleUnpack(pattern) => {
                let ptr = self.gen_local_and_store_expr(state, BindingInfoId::unknown(), &expr, ty);

                for (
                    i,
                    SymbolPattern {
                        binding_info_id,
                        ignore,
                        ..
                    },
                ) in pattern.symbols.iter().enumerate()
                {
                    if *ignore {
                        continue;
                    }

                    let ty = ty.normalize(self.tycx);
                    let llvm_type = Some(ty.llvm_type(self));

                    let value = self.gen_struct_access(ptr.into(), i as u32, llvm_type);

                    self.gen_local_with_alloca(
                        state,
                        *binding_info_id,
                        if ty.is_pointer() {
                            value
                        } else {
                            self.build_load(value)
                        },
                    );
                }
            }
        }
    }

    pub(super) fn gen_binding_pattern_with_value(
        &mut self,
        state: &mut CodegenState<'ctx>,
        pattern: &Pattern,
        ty: TyKind,
        value: BasicValueEnum<'ctx>,
    ) {
        match pattern {
            Pattern::Single(symbol) => {
                self.gen_local_with_alloca(state, symbol.binding_info_id, value);
            }
            Pattern::StructUnpack(pattern) => {
                let struct_ty = ty.maybe_deref_once().into_struct().clone();

                for i in 0..pattern.symbols.len() {
                    let SymbolPattern {
                        binding_info_id,
                        symbol,
                        ignore,
                        ..
                    } = pattern.symbols[i];

                    if ignore {
                        continue;
                    }

                    let field_index = struct_ty
                        .fields
                        .iter()
                        .position(|f| f.symbol == symbol)
                        .unwrap();

                    let llvm_type = Some(ty.llvm_type(self));
                    let value = self.gen_struct_access(value, field_index as u32, llvm_type);
                    let value = if ty.is_pointer() {
                        value
                    } else {
                        self.build_load(value)
                    };

                    self.gen_local_with_alloca(state, binding_info_id, value);
                }
            }
            Pattern::TupleUnpack(pattern) => {
                for i in 0..pattern.symbols.len() {
                    let SymbolPattern {
                        binding_info_id,
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

                    self.gen_local_with_alloca(state, binding_info_id, value);
                }
            }
        }
    }

    pub(super) fn gen_local_uninit(
        &mut self,
        state: &mut CodegenState<'ctx>,
        id: BindingInfoId,
        ty: &TyKind,
    ) -> PointerValue<'ctx> {
        let ty = ty.llvm_type(self);
        let ptr = self.build_alloca_named(state, ty, id);
        self.gen_local_inner(state, id, ptr);
        ptr
    }

    pub(super) fn gen_local_with_alloca(
        &mut self,
        state: &mut CodegenState<'ctx>,
        id: BindingInfoId,
        value: BasicValueEnum<'ctx>,
    ) -> PointerValue<'ctx> {
        let ptr = self.build_alloca_named(state, value.get_type(), id);
        self.build_store(ptr, value);
        self.gen_local_inner(state, id, ptr);
        ptr
    }

    pub(super) fn gen_local_or_load_addr(
        &mut self,
        state: &mut CodegenState<'ctx>,
        id: BindingInfoId,
        value: BasicValueEnum<'ctx>,
    ) -> PointerValue<'ctx> {
        if is_a_load_inst(value) {
            self.get_operand(value).into_pointer_value()
        } else {
            self.gen_local_with_alloca(state, id, value)
        }
    }

    pub(super) fn build_alloca_or_load_addr(
        &mut self,
        state: &mut CodegenState<'ctx>,
        value: BasicValueEnum<'ctx>,
    ) -> PointerValue<'ctx> {
        if is_a_load_inst(value) {
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
        id: BindingInfoId,
        ptr: PointerValue<'ctx>,
    ) {
        let name = self
            .workspace
            .get_binding_info(id)
            .map_or(ustr(""), |b| b.symbol);

        ptr.set_name(&name);

        let align = align_of(
            ptr.get_type().get_element_type().try_into().unwrap(),
            self.target_metrics.word_size,
        );

        ptr.as_instruction_value()
            .unwrap()
            .set_alignment(align as u32)
            .unwrap();

        if id != BindingInfoId::unknown() {
            state.scopes.insert(id, CodegenDecl::Local(ptr));
        }
    }

    pub(super) fn gen_expr(
        &mut self,
        state: &mut CodegenState<'ctx>,
        expr: &ast::Expr,
        deref: bool,
    ) -> BasicValueEnum<'ctx> {
        if self.current_block().get_terminator().is_some() {
            return self.gen_unit();
        }

        let value = match &expr.kind {
            ast::ExprKind::Import(imports) => {
                for import in imports.iter() {
                    let decl = self.gen_import(import);
                    state.scopes.insert(import.binding_info_id, decl);
                }
                self.gen_unit()
            }
            ast::ExprKind::Foreign(bindings) => {
                for binding in bindings.iter() {
                    let ty = binding.ty.normalize(self.tycx);
                    if ty.is_fn() {
                        self.gen_binding_pattern_with_expr(
                            state,
                            &binding.pattern,
                            &ty,
                            &binding.expr,
                        );
                    } else {
                        let pat = binding.pattern.as_single_ref();
                        let ty = ty.llvm_type(self);
                        let global_value =
                            self.add_global_uninit(pat.binding_info_id, ty, Linkage::External);
                        state
                            .scopes
                            .insert(pat.binding_info_id, CodegenDecl::Global(global_value));
                    }
                }

                self.gen_unit()
            }
            ast::ExprKind::Binding(binding) => {
                self.gen_binding_pattern_with_expr(
                    state,
                    &binding.pattern,
                    &binding.ty.normalize(self.tycx),
                    &binding.expr,
                );

                self.gen_unit()
            }
            ast::ExprKind::Defer(_) => self.gen_unit(),
            ast::ExprKind::Assign(assign) => {
                let left = self
                    .gen_expr(state, &assign.lvalue, false)
                    .into_pointer_value();
                let right = self.gen_expr(state, &assign.rvalue, true);

                // println!("left: {:#?}", left);
                // println!("right: {:#?}", right);

                self.build_store(left, right);

                self.gen_unit()
            }
            ast::ExprKind::Cast(info) => self.gen_cast(state, &info.expr, &info.target_ty),
            ast::ExprKind::Builtin(builtin) => match builtin {
                ast::Builtin::SizeOf(expr) => match expr.ty.normalize(self.tycx) {
                    TyKind::Type(ty) => ty.llvm_type(self).size_of().unwrap().into(),
                    ty => unreachable!("got {}", ty),
                },
                ast::Builtin::AlignOf(expr) => match expr.ty.normalize(self.tycx) {
                    TyKind::Type(ty) => ty.llvm_type(self).align_of().into(),
                    ty => unreachable!("got {}", ty),
                },
                ast::Builtin::Panic(msg_expr) => {
                    let message = if let Some(msg_expr) = msg_expr {
                        self.gen_expr(state, msg_expr, true)
                    } else {
                        self.gen_global_str("", "", true)
                    };

                    self.gen_panic(state, message, expr.span);
                    self.gen_unit()
                }
            },
            ast::ExprKind::Fn(func) => {
                let function = self.gen_fn(state.module_info, func, Some(state.clone()));

                self.start_block(state, state.curr_block);

                function.as_global_value().as_pointer_value().into()
            }
            ast::ExprKind::While(while_) => {
                let loop_head = self.append_basic_block(state, "loop_head");
                let loop_body = self.append_basic_block(state, "loop_body");
                let loop_exit = self.append_basic_block(state, "loop_exit");

                self.builder.build_unconditional_branch(loop_head);
                self.start_block(state, loop_head);

                let cond = self.gen_expr(state, &while_.cond, true).into_int_value();

                self.builder
                    .build_conditional_branch(cond, loop_body, loop_exit);

                self.start_block(state, loop_body);

                state.loop_blocks.push(LoopBlock {
                    head: loop_head,
                    exit: loop_exit,
                });

                self.gen_block(state, &while_.block);

                state.loop_blocks.pop();

                if self.current_block().get_terminator().is_none() {
                    self.builder.build_unconditional_branch(loop_head);
                }

                self.start_block(state, loop_exit);

                return self.gen_unit();
            }
            ast::ExprKind::For(for_) => {
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
                        let end = match value.ty.normalize(self.tycx).maybe_deref_once() {
                            TyKind::Array(_, len) => {
                                self.ptr_sized_int_type.const_int(len as u64, false)
                            }
                            TyKind::Slice(..) => {
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
                        self.gen_local_with_alloca(state, for_.iter_id, start.into())
                    }
                    ast::ForIter::Value(value) => {
                        let by_ref = value.ty.normalize(self.tycx).is_pointer();

                        let agg = self.gen_expr(state, value, false).into_pointer_value();
                        let agg = self.maybe_load_double_pointer(agg);

                        let item = self.gen_subscript(
                            agg.into(),
                            &value.ty.normalize(self.tycx),
                            start,
                            !by_ref,
                        );

                        self.gen_local_with_alloca(state, for_.iter_id, item)
                    }
                };

                let it_index = self.gen_local_with_alloca(state, for_.iter_index_id, start.into());

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

                self.gen_expr(state, &for_.block, true);

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
                            let by_ref = value.ty.normalize(self.tycx).is_pointer();

                            let agg = self.gen_expr(state, value, false).into_pointer_value();
                            let agg = self.maybe_load_double_pointer(agg);

                            let item = self.gen_subscript(
                                agg.into(),
                                &value.ty.normalize(self.tycx),
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
            ast::ExprKind::Break(e) => {
                self.gen_expr_list(state, &e.deferred);

                let exit_block = state.loop_blocks.last().unwrap().exit;
                self.builder.build_unconditional_branch(exit_block);

                self.gen_unit()
            }
            ast::ExprKind::Continue(e) => {
                self.gen_expr_list(state, &e.deferred);

                let head_block = state.loop_blocks.last().unwrap().head;
                self.builder.build_unconditional_branch(head_block);

                self.gen_unit()
            }
            ast::ExprKind::Return(ret) => {
                let value = ret
                    .expr
                    .as_ref()
                    .map(|expr| self.gen_expr(state, expr, true));
                self.gen_return(state, value, &ret.deferred)
            }
            ast::ExprKind::If(if_) => self.gen_if_expr(state, if_),
            ast::ExprKind::Block(block) => self.gen_block(state, block),
            ast::ExprKind::Binary(binary) => self.gen_binary(state, binary, expr.span),
            ast::ExprKind::Unary(unary) => self.gen_unary(state, unary, expr.span, deref),
            ast::ExprKind::Subscript(sub) => {
                let value = self.gen_expr(state, &sub.expr, false);
                let index = self.gen_expr(state, &sub.index, true).into_int_value();

                let ty = sub.expr.ty.normalize(self.tycx);

                let len = match ty.maybe_deref_once() {
                    TyKind::Array(_, size) => Some(index.get_type().const_int(size as _, false)),
                    TyKind::Slice(..) => Some(self.gen_load_slice_len(value)),
                    TyKind::MultiPointer(..) => None,
                    ty => unreachable!("got {}", ty),
                };

                if let Some(len) = len {
                    self.gen_runtime_check_index_out_of_bounds(state, index, len, expr.span);
                }

                self.gen_subscript(value, &ty, index, deref)
            }
            ast::ExprKind::Slice(slice) => {
                let ptr = self
                    .gen_expr(state, &slice.expr, false)
                    .into_pointer_value();
                let ty = slice.expr.ty.normalize(self.tycx);

                let data = match ty {
                    TyKind::Slice(..) => {
                        let data = self.gen_load_slice_data(ptr.into());
                        self.build_load(data.into()).into_pointer_value()
                    }
                    TyKind::MultiPointer(..) => self.build_load(ptr.into()).into_pointer_value(),
                    TyKind::Array(..) => ptr,
                    _ => unreachable!("got {}", ty),
                };

                let low = match &slice.low {
                    Some(low) => self.gen_expr(state, low, true).into_int_value(),
                    None => match &slice.high {
                        Some(high) => high.ty.llvm_type(self).into_int_type(),
                        None => self.ptr_sized_int_type,
                    }
                    .const_zero(),
                };

                let high = match &slice.high {
                    Some(high) => self.gen_expr(state, high, true).into_int_value(),
                    None => match ty {
                        TyKind::Array(_, len) => low.get_type().const_int(len as u64, false),
                        TyKind::Slice(..) => self.gen_load_slice_len(ptr.into()),
                        _ => unreachable!(),
                    },
                };

                self.gen_runtime_check_slice_end_before_start(state, low, high, expr.span);

                let len = match ty {
                    TyKind::Slice(..) => Some(self.gen_load_slice_len(ptr.into())),
                    TyKind::Array(_, size) => {
                        Some(self.ptr_sized_int_type.const_int(size as _, false))
                    }
                    TyKind::MultiPointer(..) => None,
                    _ => unreachable!("got {}", ty),
                };

                if let Some(len) = len {
                    self.gen_runtime_check_slice_range_out_of_bounds(
                        state, low, high, len, expr.span,
                    );
                }

                let slice_llvm_ty = expr.ty.llvm_type(self);
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
            ast::ExprKind::FnCall(call) => self.gen_fn_call_expr(state, call, expr.ty),
            ast::ExprKind::MemberAccess(access) => {
                let value = self.gen_expr(state, &access.expr, false);
                let accessed_ty = access.expr.ty.normalize(self.tycx);

                let value = if accessed_ty.is_pointer() {
                    self.build_load(value)
                } else {
                    value
                };

                let derefed_ty = accessed_ty.maybe_deref_once();
                let value = match accessed_ty.maybe_deref_once() {
                    TyKind::Tuple(_) => {
                        let index = access.member.parse::<usize>().unwrap();
                        let llvm_ty = Some(derefed_ty.llvm_type(self));
                        self.gen_struct_access(value, index as u32, llvm_ty)
                    }
                    TyKind::Struct(struct_ty) => {
                        let struct_llvm_ty = Some(derefed_ty.llvm_type(self));

                        if struct_ty.is_union() {
                            let field = struct_ty
                                .fields
                                .iter()
                                .find(|f| f.symbol == access.member)
                                .unwrap();
                            let field_ty = field.ty.llvm_type(self);
                            let casted_ptr = self.builder.build_pointer_cast(
                                value.into_pointer_value(),
                                field_ty.ptr_type(AddressSpace::Generic),
                                "",
                            );
                            casted_ptr.into()
                        } else {
                            let index = struct_ty
                                .fields
                                .iter()
                                .position(|f| f.symbol == access.member)
                                .unwrap();
                            self.gen_struct_access(value, index as u32, struct_llvm_ty)
                        }
                    }
                    TyKind::Array(_, len) => match access.member.as_str() {
                        BUILTIN_FIELD_LEN => {
                            self.ptr_sized_int_type.const_int(len as _, false).into()
                        }
                        _ => unreachable!("got field `{}`", access.member),
                    },
                    TyKind::Slice(..) => match access.member.as_str() {
                        BUILTIN_FIELD_LEN => self.gen_load_slice_len(value).into(),
                        BUILTIN_FIELD_DATA => self.gen_load_slice_data(value).into(),
                        _ => unreachable!("got field `{}`", access.member),
                    },
                    TyKind::Module(module_id) => {
                        let id = BindingInfoId(
                            self.workspace
                                .binding_infos
                                .iter()
                                .position(|info| {
                                    info.module_id == module_id && info.symbol == access.member
                                })
                                .expect(&format!(
                                    "couldn't find member `{}` in module `{}`",
                                    self.workspace.get_module_info(module_id).unwrap().name,
                                    access.member
                                )),
                        );

                        let decl = self.find_or_gen_top_level_binding(id);

                        match decl {
                            CodegenDecl::Module { .. } => self.gen_unit(),
                            _ => {
                                let ptr = decl.into_pointer_value();

                                if deref {
                                    self.build_load(ptr.into())
                                } else {
                                    ptr.into()
                                }
                            }
                        }
                    }
                    _ => unreachable!("invalid ty `{}`", accessed_ty),
                };

                if deref {
                    self.build_load(value)
                } else {
                    value
                }
            }
            ast::ExprKind::Ident(ident) => {
                assert!(ident.binding_info_id != BindingInfoId::unknown());

                let decl = match state.scopes.get(ident.binding_info_id) {
                    Some((_, decl)) => decl.clone(),
                    None => self.find_or_gen_top_level_binding(ident.binding_info_id),
                };

                match decl {
                    CodegenDecl::Module { .. } => self.gen_unit(),
                    _ => {
                        let ptr = decl.into_pointer_value();
                        if deref {
                            self.build_load(ptr.into())
                        } else {
                            ptr.into()
                        }
                    }
                }
            }
            ast::ExprKind::ArrayLiteral(kind) => {
                let ty = expr.ty.normalize(self.tycx);
                let array_ptr = match kind {
                    ast::ArrayLiteralKind::List(elements) => {
                        let elements: Vec<BasicValueEnum> = elements
                            .iter()
                            .map(|e| self.gen_expr(state, e, true))
                            .collect();

                        let size = match &ty {
                            TyKind::Array(_, size) => *size,
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

                        let cond =
                            self.builder
                                .build_int_compare(IntPredicate::SLT, it_value, end, "");

                        self.builder
                            .build_conditional_branch(cond, loop_body, loop_exit);

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
            ast::ExprKind::TupleLiteral(elements) => {
                let ty = expr.ty.normalize(self.tycx);
                if ty.is_type() {
                    return self.gen_unit();
                }

                let values: Vec<BasicValueEnum> = elements
                    .iter()
                    .map(|e| self.gen_expr(state, e, true))
                    .collect();

                let llvm_ty = expr.ty.llvm_type(self);
                let tuple = self.build_alloca(state, llvm_ty);

                for (i, value) in values.iter().enumerate() {
                    self.build_store(
                        self.builder
                            .build_struct_gep(tuple, i as u32, &format!("tuple_value_{}", i))
                            .unwrap(),
                        *value,
                    );
                }

                if deref {
                    self.build_load(tuple.into())
                } else {
                    tuple.into()
                }
            }
            ast::ExprKind::StructLiteral(lit) => self.gen_struct_literal_named(
                state,
                &expr.ty.normalize(self.tycx),
                &lit.fields,
                deref,
            ),

            ast::ExprKind::Literal(value) => {
                self.gen_literal_value(value, &expr.ty.normalize(self.tycx), deref)
            }

            ast::ExprKind::PointerType(..)
            | ast::ExprKind::MultiPointerType(..)
            | ast::ExprKind::ArrayType(..)
            | ast::ExprKind::SliceType(..)
            | ast::ExprKind::StructType(..)
            | ast::ExprKind::SelfType
            | ast::ExprKind::UnitType
            | ast::ExprKind::NeverType
            | ast::ExprKind::PlaceholderType => self.gen_unit(),

            ast::ExprKind::FnType(sig) => {
                if sig.lib_name.is_some() {
                    // this is a foreign function
                    let function = self.declare_fn_sig(state.module_info, sig);
                    function.as_global_value().as_pointer_value().into()
                } else {
                    // this is a type
                    self.gen_unit()
                }
            }
        };

        if expr.ty.normalize(self.tycx).is_never() {
            self.build_unreachable();
        }

        value
    }

    pub(super) fn gen_block(
        &mut self,
        state: &mut CodegenState<'ctx>,
        block: &ast::Block,
    ) -> BasicValueEnum<'ctx> {
        let mut value = self.gen_unit();

        state.push_scope();

        for expr in &block.exprs {
            value = self.gen_expr(state, expr, true);
        }

        self.gen_expr_list(state, &block.deferred);

        state.pop_scope();

        value
    }

    pub(super) fn gen_expr_list(
        &mut self,
        state: &mut CodegenState<'ctx>,
        expr_list: &Vec<ast::Expr>,
    ) {
        for expr in expr_list {
            self.gen_expr(state, expr, true);
        }
    }

    pub(super) fn gen_local_and_store_expr(
        &mut self,
        state: &mut CodegenState<'ctx>,
        id: BindingInfoId,
        expr: &Option<ast::Expr>,
        ty: &TyKind,
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

    pub(super) fn gen_cast(
        &mut self,
        state: &mut CodegenState<'ctx>,
        expr: &Box<ast::Expr>,
        target_ty: &Ty,
    ) -> BasicValueEnum<'ctx> {
        let value = self.gen_expr(state, expr, true);
        self.gen_cast_inner(
            state,
            value,
            &expr.ty.normalize(self.tycx),
            &target_ty.normalize(self.tycx),
        )
    }

    fn gen_cast_inner(
        &mut self,
        state: &mut CodegenState<'ctx>,
        value: BasicValueEnum<'ctx>,
        from_ty: &TyKind,
        target_ty: &TyKind,
    ) -> BasicValueEnum<'ctx> {
        if from_ty == target_ty {
            return value;
        }

        const INST_NAME: &str = "cast";

        let cast_type = target_ty.llvm_type(self);

        match (from_ty, target_ty) {
            (TyKind::Bool, TyKind::AnyInt(_))
            | (TyKind::Bool, TyKind::Int(_))
            | (TyKind::Bool, TyKind::UInt(_)) => self
                .builder
                .build_int_z_extend(value.into_int_value(), cast_type.into_int_type(), INST_NAME)
                .into(),
            (
                TyKind::AnyInt(_) | TyKind::Int(_) | TyKind::UInt(_),
                TyKind::AnyInt(_) | TyKind::Int(_) | TyKind::UInt(_),
            ) => self
                .builder
                .build_int_cast(value.into_int_value(), cast_type.into_int_type(), INST_NAME)
                .into(),

            (TyKind::AnyInt(_) | TyKind::Int(_), TyKind::Float(_)) => self
                .builder
                .build_signed_int_to_float(
                    value.into_int_value(),
                    cast_type.into_float_type(),
                    INST_NAME,
                )
                .into(),

            (TyKind::UInt(_), TyKind::Float(_)) => self
                .builder
                .build_unsigned_int_to_float(
                    value.into_int_value(),
                    cast_type.into_float_type(),
                    INST_NAME,
                )
                .into(),

            (TyKind::Float(_), TyKind::AnyInt(_) | TyKind::Int(_)) => self
                .builder
                .build_float_to_signed_int(
                    value.into_float_value(),
                    cast_type.into_int_type(),
                    INST_NAME,
                )
                .into(),
            (TyKind::Float(_), TyKind::UInt(_)) => self
                .builder
                .build_float_to_unsigned_int(
                    value.into_float_value(),
                    cast_type.into_int_type(),
                    INST_NAME,
                )
                .into(),
            (TyKind::Float(_), TyKind::Float(_)) => self
                .builder
                .build_float_cast(
                    value.into_float_value(),
                    cast_type.into_float_type(),
                    INST_NAME,
                )
                .into(),

            (
                TyKind::Pointer(..) | TyKind::MultiPointer(..),
                TyKind::Pointer(..) | TyKind::MultiPointer(..),
            ) => self
                .builder
                .build_pointer_cast(
                    value.into_pointer_value(),
                    cast_type.into_pointer_type(),
                    INST_NAME,
                )
                .into(),

            // pointer <=> int | uint
            (TyKind::Pointer(..), TyKind::AnyInt(_) | TyKind::Int(..) | TyKind::UInt(..)) => self
                .builder
                .build_ptr_to_int(
                    value.into_pointer_value(),
                    cast_type.into_int_type(),
                    INST_NAME,
                )
                .into(),

            // int | uint <=> pointer
            (TyKind::AnyInt(_) | TyKind::Int(..) | TyKind::UInt(..), TyKind::Pointer(..)) => self
                .builder
                .build_int_to_ptr(
                    value.into_int_value(),
                    cast_type.into_pointer_type(),
                    INST_NAME,
                )
                .into(),

            (TyKind::Pointer(t, _), TyKind::Slice(t_slice, ..)) => match t.as_ref() {
                TyKind::Array(_, size) => {
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

    pub(super) fn gen_return(
        &mut self,
        state: &mut CodegenState<'ctx>,
        value: Option<BasicValueEnum<'ctx>>,
        deferred: &Vec<ast::Expr>,
    ) -> BasicValueEnum<'ctx> {
        let abi_fn = self.fn_types.get(&state.fn_type).unwrap().clone();

        if abi_fn.ret.kind.is_indirect() {
            let return_ptr = state.return_ptr.unwrap();
            match value {
                Some(value) => {
                    let size = size_of(abi_fn.ret.ty, self.target_metrics.word_size);
                    if is_a_load_inst(value) && size > self.target_metrics.word_size {
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

            self.gen_expr_list(state, deferred);
            self.builder.build_return(None);
        } else {
            let value = value.unwrap_or(self.gen_unit());
            let value = self.build_transmute(
                state,
                value,
                state.function.get_type().get_return_type().unwrap(),
            );
            self.gen_expr_list(state, deferred);
            self.builder.build_return(Some(&value));
        }

        self.gen_unit()
    }
}
