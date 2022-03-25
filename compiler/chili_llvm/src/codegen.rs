use super::{
    abi::{align_of, size_of, AbiFn},
    util::is_a_load_inst,
};
use chili_ast::{
    ast::{ArrayLiteralKind, Binding, Builtin, Expr, ExprKind, ForIter, Import, ModuleInfo},
    pattern::{Pattern, SymbolPattern},
};
use chili_ast::{ty::*, workspace::Workspace};
use common::{
    builtin::{BUILTIN_FIELD_DATA, BUILTIN_FIELD_LEN},
    env::Env,
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
}

pub struct Codegen<'w, 'cg, 'ctx> {
    pub workspace: &'cg Workspace,
    pub target_metrics: TargetMetrics,

    pub context: &'ctx Context,
    pub module: &'cg Module<'ctx>,
    pub fpm: &'cg PassManager<FunctionValue<'ctx>>,
    pub builder: &'cg Builder<'ctx>,
    pub ptr_sized_int_type: IntType<'ctx>,

    pub module_decl_map: ModuleToCodegenDeclsMap<'ctx>,
    pub type_map: UstrMap<BasicTypeEnum<'ctx>>,
    pub global_str_map: UstrMap<PointerValue<'ctx>>,
    pub fn_type_map: HashMap<FnTy, AbiFn<'ctx>>,
}

pub(super) type ModuleToCodegenDeclsMap<'ctx> = UstrMap<CodegenDeclsMap<'ctx>>;
pub(super) type CodegenDeclsMap<'ctx> = UstrMap<CodegenDecl<'ctx>>;

#[derive(Clone)]
pub(super) struct CodegenState<'ctx> {
    pub(super) module_info: ModuleInfo,
    pub(super) function: FunctionValue<'ctx>,
    pub(super) fn_type: FnTy,
    pub(super) return_ptr: Option<PointerValue<'ctx>>,
    pub(super) loop_blocks: Vec<LoopBlock<'ctx>>,
    pub(super) decl_block: BasicBlock<'ctx>,
    pub(super) curr_block: BasicBlock<'ctx>,
    pub(super) env: Env<CodegenDecl<'ctx>>,
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
            env: Env::new(),
        }
    }

    pub(super) fn push_named_scope(&mut self, name: Ustr) {
        self.env.push_named_scope(name);
    }

    pub(super) fn push_scope(&mut self) {
        self.env.push_scope();
    }

    pub(super) fn pop_scope(&mut self) {
        self.env.pop_scope();
    }
}

#[derive(Clone, Copy)]
pub(super) struct LoopBlock<'ctx> {
    head: BasicBlock<'ctx>,
    exit: BasicBlock<'ctx>,
}

impl<'w, 'cg, 'ctx> Codegen<'w, 'cg, 'ctx> {
    pub fn codegen(&mut self) {
        let root_module = self.ir.root_module();

        let startup_binding = root_module
            .bindings
            .iter()
            .find(|binding| match &binding.value {
                Some(expr) => match &expr.kind {
                    ExprKind::Fn(func) => func.is_startup,
                    _ => false,
                },
                None => false,
            })
            .expect("couldn't find startup function");

        self.gen_top_level_binding(root_module.info, startup_binding);
    }

    pub(super) fn find_or_gen_binding_by_name(
        &mut self,
        module_name: impl Into<Ustr>,
        symbol: impl Into<Ustr>,
    ) -> CodegenDecl<'ctx> {
        self.find_or_gen_top_level_decl(self.ir.module_info(module_name.into()), symbol.into())
    }

    pub(super) fn find_or_gen_top_level_decl(
        &mut self,
        module_info: ModuleInfo,
        symbol: Ustr,
    ) -> CodegenDecl<'ctx> {
        match self.module_decl_map.get(&module_info.name) {
            Some(module) => match module.get(&symbol) {
                Some(decl) => return decl.clone(),
                None => (),
            },
            None => (),
        }

        let module = self.ir.modules.get(&module_info.name).unwrap();

        if let Some(binding) = module.find_binding(symbol) {
            if !binding.should_codegen {
                unreachable!()
            }

            self.gen_top_level_binding(module_info, binding)
        } else if let Some(import) = module.find_import(symbol) {
            if import.import_path.is_empty() {
                let decl = CodegenDecl::Module(import.module_info);

                self.get_or_insert_new_module(module_info.name)
                    .insert(import.alias, decl);

                decl
            } else {
                self.resolve_decl_from_import(import)
            }
        } else {
            unreachable!(
                "couldn't find top level symbol `{}` in module `{}`",
                symbol, module_info.name
            )
        }
    }

    pub(crate) fn resolve_decl_from_import(&mut self, import: &Import) -> CodegenDecl<'ctx> {
        let mut decl = CodegenDecl::Module(import.module_info);

        if !import.import_path.is_empty() {
            // go over the import_path, and get the relevant symbol
            let mut current_module_info = import.module_info;

            for symbol in import.import_path.iter() {
                decl =
                    self.find_or_gen_top_level_decl(current_module_info, symbol.value.as_symbol());

                match decl {
                    CodegenDecl::Module(info) => {
                        current_module_info = info;
                    }
                    _ => (),
                }
            }
        }

        decl
    }

    pub(super) fn gen_top_level_binding(
        &mut self,
        module_info: ModuleInfo,
        binding: &Binding,
    ) -> CodegenDecl<'ctx> {
        match binding.value.as_ref() {
            Some(expr) => match &expr.kind {
                ExprKind::Fn(func) => {
                    // * generate top level function
                    let function = self.gen_fn(module_info, func, None);

                    let decl = CodegenDecl::Function(function);
                    self.get_or_insert_new_module(module_info.name)
                        .insert(func.sig.name, decl);

                    return decl;
                }
                ExprKind::FnType(sig) => {
                    // * generate top level function type
                    let function = self.declare_fn_sig(module_info, sig);

                    let decl = CodegenDecl::Function(function);
                    self.get_or_insert_new_module(module_info.name)
                        .insert(sig.name, decl);

                    return decl;
                }
                _ => (),
            },
            None => (),
        }

        // * forward declare the global value, i.e: `let answer = 42`
        // * the global value will is initialized by the startup function, see
        //   `fn gen_startup`
        let symbol = binding.pattern.into_single().symbol;
        let ty = self.llvm_type(&binding.ty);

        let global_value = if binding.lib_name.is_some() {
            self.add_global_uninit(&symbol, ty, Linkage::External)
        } else {
            self.add_global(
                &if module_info.name.is_empty() {
                    symbol.to_string()
                } else {
                    format!("{}.{}", module_info.name, symbol)
                },
                ty,
                Linkage::Private,
            )
        };

        self.get_or_insert_new_module(module_info.name)
            .insert(symbol, CodegenDecl::Global(global_value));

        CodegenDecl::Global(global_value)
    }

    pub(super) fn add_global(
        &mut self,
        name: &str,
        ty: BasicTypeEnum<'ctx>,
        linkage: Linkage,
    ) -> GlobalValue<'ctx> {
        let global_value = self.add_global_uninit(name, ty, linkage);
        global_value.set_initializer(&ty.const_zero());
        global_value
    }

    pub(super) fn add_global_uninit(
        &mut self,
        name: &str,
        ty: BasicTypeEnum<'ctx>,
        linkage: Linkage,
    ) -> GlobalValue<'ctx> {
        let global_value = self.module.add_global(ty, None, name);
        global_value.set_linkage(linkage);
        global_value
    }

    pub(super) fn gen_binding_pattern_from_expr(
        &mut self,
        state: &mut CodegenState<'ctx>,
        pattern: &Pattern,
        ty: &Ty,
        expr: &Option<Expr>,
    ) {
        match pattern {
            Pattern::Single(SymbolPattern { symbol, .. }) => {
                self.gen_local_and_store_expr(state, *symbol, &expr, ty);
            }
            Pattern::StructDestructor(pattern) => {
                let ptr =
                    self.gen_local_and_store_expr(state, ustr("struct_destr_alloca"), &expr, ty);

                let struct_ty = ty.maybe_deref_once().into_struct().clone();

                for SymbolPattern {
                    symbol,
                    alias,
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

                    let llvm_type = Some(self.llvm_type(ty));
                    let value = self.gen_struct_access(ptr.into(), field_index as u32, llvm_type);

                    self.gen_local_with_alloca(
                        state,
                        alias.unwrap_or(*symbol),
                        if ty.is_pointer() {
                            value
                        } else {
                            self.build_load(value)
                        },
                    );
                }
            }
            Pattern::TupleDestructor(pattern) => {
                let ptr =
                    self.gen_local_and_store_expr(state, ustr("tuple_destr_alloca"), &expr, ty);

                for (i, SymbolPattern { symbol, ignore, .. }) in pattern.symbols.iter().enumerate()
                {
                    if *ignore {
                        continue;
                    }

                    let llvm_type = Some(self.llvm_type(ty));
                    let value = self.gen_struct_access(ptr.into(), i as u32, llvm_type);
                    self.gen_local_with_alloca(
                        state,
                        *symbol,
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

    pub(super) fn gen_binding_pattern_from_value(
        &mut self,
        state: &mut CodegenState<'ctx>,
        pattern: &Pattern,
        ty: &Ty,
        value: BasicValueEnum<'ctx>,
    ) {
        match pattern {
            Pattern::Single(symbol) => {
                self.gen_local_with_alloca(state, symbol.symbol, value);
            }
            Pattern::StructDestructor(pattern) => {
                let struct_ty = ty.maybe_deref_once().into_struct().clone();

                for i in 0..pattern.symbols.len() {
                    let SymbolPattern {
                        symbol,
                        alias,
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

                    let llvm_type = Some(self.llvm_type(ty));
                    let value = self.gen_struct_access(value, field_index as u32, llvm_type);
                    let value = if ty.is_pointer() {
                        value
                    } else {
                        self.build_load(value)
                    };

                    self.gen_local_with_alloca(state, alias.unwrap_or(symbol), value);
                }
            }
            Pattern::TupleDestructor(pattern) => {
                for i in 0..pattern.symbols.len() {
                    let SymbolPattern { symbol, ignore, .. } = pattern.symbols[i];

                    if ignore {
                        continue;
                    }

                    let llvm_type = Some(self.llvm_type(ty));
                    let value = self.gen_struct_access(value, i as u32, llvm_type);
                    let value = if ty.is_pointer() {
                        value
                    } else {
                        self.build_load(value)
                    };

                    self.gen_local_with_alloca(state, symbol, value);
                }
            }
        }
    }

    pub(super) fn gen_local_uninit(
        &mut self,
        state: &mut CodegenState<'ctx>,
        name: Ustr,
        ty: &Ty,
    ) -> PointerValue<'ctx> {
        let ty = self.llvm_type(ty);
        let ptr = self.build_alloca_named(state, ty, name);
        self.gen_local_internal(state, name, ptr);
        ptr
    }

    pub(super) fn gen_local_with_alloca(
        &mut self,
        state: &mut CodegenState<'ctx>,
        name: Ustr,
        value: BasicValueEnum<'ctx>,
    ) -> PointerValue<'ctx> {
        let ptr = self.build_alloca_named(state, value.get_type(), name);
        self.build_store(ptr, value);
        self.gen_local_internal(state, name, ptr);
        value.set_name(&name);
        ptr
    }

    pub(super) fn gen_local_or_load_addr(
        &mut self,
        state: &mut CodegenState<'ctx>,
        name: Ustr,
        value: BasicValueEnum<'ctx>,
    ) -> PointerValue<'ctx> {
        if is_a_load_inst(value) {
            self.get_operand(value).into_pointer_value()
        } else {
            self.gen_local_with_alloca(state, name, value)
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

    fn gen_local_internal(
        &mut self,
        state: &mut CodegenState<'ctx>,
        name: Ustr,
        ptr: PointerValue<'ctx>,
    ) {
        ptr.set_name(&name);

        let align = align_of(
            ptr.get_type().get_element_type().try_into().unwrap(),
            self.target_metrics.word_size,
        );

        ptr.as_instruction_value()
            .unwrap()
            .set_alignment(align as u32)
            .unwrap();

        state.env.insert(name, CodegenDecl::Local(ptr));
    }

    pub(super) fn gen_expr(
        &mut self,
        state: &mut CodegenState<'ctx>,
        expr: &Expr,
        deref: bool,
    ) -> BasicValueEnum<'ctx> {
        if self.current_block().get_terminator().is_some() {
            return self.gen_unit();
        }

        let value = match &expr.kind {
            ExprKind::Import(imports) => {
                for import in imports.iter() {
                    let decl = self.resolve_decl_from_import(import);
                    state.env.insert(import.alias, decl);
                }

                self.gen_unit()
            }
            ExprKind::Foreign(bindings) => {
                for binding in bindings.iter() {
                    if binding.ty.is_fn() {
                        self.gen_binding_pattern_from_expr(
                            state,
                            &binding.pattern,
                            &binding.ty,
                            &binding.value,
                        );
                    } else {
                        let symbol = binding.pattern.into_single().symbol;
                        let ty = self.llvm_type(&binding.ty);
                        let global_value = self.add_global_uninit(&symbol, ty, Linkage::External);
                        state.env.insert(symbol, CodegenDecl::Global(global_value));
                    }
                }

                self.gen_unit()
            }
            ExprKind::Binding(binding) => {
                self.gen_binding_pattern_from_expr(
                    state,
                    &binding.pattern,
                    &binding.ty,
                    &binding.value,
                );

                self.gen_unit()
            }
            ExprKind::Defer(_) => self.gen_unit(),
            ExprKind::Assign { lvalue, rvalue } => {
                let left = self.gen_expr(state, lvalue, false).into_pointer_value();
                let right = self.gen_expr(state, rvalue, true);

                // println!("left: {:#?}", left);
                // println!("right: {:#?}", right);

                self.build_store(left, right);

                self.gen_unit()
            }
            ExprKind::Cast(info) => self.gen_cast(state, &info.expr, &info.target_ty),
            ExprKind::Builtin(builtin) => match builtin {
                Builtin::SizeOf(expr) => match &expr.ty {
                    Ty::Type(ty) => self.llvm_type(ty).size_of().unwrap().into(),
                    ty => unreachable!("got {}", ty),
                },
                Builtin::AlignOf(expr) => match &expr.ty {
                    Ty::Type(ty) => self.llvm_type(ty).align_of().into(),
                    ty => unreachable!("got {}", ty),
                },
                Builtin::Panic(msg_expr) => {
                    let message = if let Some(msg_expr) = msg_expr {
                        self.gen_expr(state, msg_expr, true)
                    } else {
                        self.gen_global_str("", "", true)
                    };

                    self.gen_panic(state, message, expr.span);
                    self.gen_unit()
                }
            },
            ExprKind::Fn(func) => {
                let function = self.gen_fn(state.module_info, func, Some(state.clone()));

                self.start_block(state, state.curr_block);

                function.as_global_value().as_pointer_value().into()
            }
            ExprKind::While { cond, block } => {
                let loop_head = self.append_basic_block(state, "loop_head");
                let loop_body = self.append_basic_block(state, "loop_body");
                let loop_exit = self.append_basic_block(state, "loop_exit");

                self.builder.build_unconditional_branch(loop_head);
                self.start_block(state, loop_head);

                let cond = self.gen_expr(state, cond, true).into_int_value();

                self.builder
                    .build_conditional_branch(cond, loop_body, loop_exit);

                self.start_block(state, loop_body);

                state.loop_blocks.push(LoopBlock {
                    head: loop_head,
                    exit: loop_exit,
                });

                self.gen_expr(state, block, true);

                state.loop_blocks.pop();

                if self.current_block().get_terminator().is_none() {
                    self.builder.build_unconditional_branch(loop_head);
                }

                self.start_block(state, loop_exit);

                return self.gen_unit();
            }
            ExprKind::For(for_) => {
                let loop_head = self.append_basic_block(state, "loop_head");
                let loop_body = self.append_basic_block(state, "loop_body");
                let loop_exit = self.append_basic_block(state, "loop_exit");

                let (start, end) = match &for_.iterator {
                    ForIter::Range(start, end) => {
                        let start = self.gen_expr(state, start, true).into_int_value();
                        let end = self.gen_expr(state, end, true).into_int_value();

                        (start, end)
                    }
                    ForIter::Value(value, ..) => {
                        let start = self.ptr_sized_int_type.const_zero();
                        let end = match &value.ty.maybe_deref_once() {
                            Ty::Array(_, len) => {
                                self.ptr_sized_int_type.const_int(*len as u64, false)
                            }
                            Ty::Slice(..) => {
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
                    ForIter::Range(_, _) => {
                        self.gen_local_with_alloca(state, for_.iter_name, start.into())
                    }
                    ForIter::Value(value) => {
                        let by_ref = value.ty.is_pointer();

                        let agg = self.gen_expr(state, value, false).into_pointer_value();
                        let agg = self.maybe_load_double_pointer(agg);

                        let item = self.gen_subscript(agg.into(), &value.ty, start, !by_ref);

                        self.gen_local_with_alloca(state, for_.iter_name, item)
                    }
                };

                let it_index =
                    self.gen_local_with_alloca(state, for_.iter_index_name, start.into());

                self.builder.build_unconditional_branch(loop_head);
                self.start_block(state, loop_head);

                let curr_index = self.build_load(it_index.into()).into_int_value();

                let continue_condition = self.builder.build_int_compare(
                    match &for_.iterator {
                        ForIter::Range(..) => IntPredicate::SLE,
                        ForIter::Value(..) => IntPredicate::SLT,
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

                self.gen_expr(state, expr, true);

                if self.current_block().get_terminator().is_none() {
                    let step = start.get_type().const_int(1, true);

                    let next_index = self
                        .builder
                        .build_int_add(curr_index, step, "for_next_index");

                    self.build_store(it_index, next_index.into());

                    match &for_.iterator {
                        ForIter::Range(_, _) => {
                            let it_value = self.build_load(it.into()).into_int_value();
                            let next_it =
                                self.builder.build_int_add(it_value, step, "for_next_index");

                            self.build_store(it, next_it.into());
                        }
                        ForIter::Value(value) => {
                            let by_ref = value.ty.is_pointer();

                            let agg = self.gen_expr(state, value, false).into_pointer_value();
                            let agg = self.maybe_load_double_pointer(agg);

                            let item =
                                self.gen_subscript(agg.into(), &value.ty, next_index, !by_ref);

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
            ExprKind::Break { deferred } => {
                self.gen_expr_list(state, deferred);

                let exit_block = state.loop_blocks.last().unwrap().exit;
                self.builder.build_unconditional_branch(exit_block);

                self.gen_unit()
            }
            ExprKind::Continue { deferred } => {
                self.gen_expr_list(state, deferred);

                let head_block = state.loop_blocks.last().unwrap().head;
                self.builder.build_unconditional_branch(head_block);

                self.gen_unit()
            }
            ExprKind::Return { expr, deferred } => {
                let value = expr.as_ref().map(|expr| self.gen_expr(state, expr, true));
                self.gen_return(state, value, deferred)
            }
            ExprKind::If {
                cond,
                then_expr,
                else_expr,
            } => self.gen_if_expr(state, cond, then_expr, else_expr),
            ExprKind::Block(block) => {
                let mut value = self.gen_unit();

                state.push_scope();

                for expr in &block.exprs {
                    value = self.gen_expr(state, expr, true);
                }

                self.gen_expr_list(state, &block.deferred);

                state.pop_scope();

                value
            }
            ExprKind::Binary { lhs, op, rhs } => self.gen_binary(state, lhs, op, rhs, expr.span),
            ExprKind::Unary { op, lhs } => self.gen_unary(state, op, lhs, expr.span, deref),
            ExprKind::Subscript {
                expr: accessed_expr,
                index,
            } => {
                let value = self.gen_expr(state, accessed_expr, false);
                let index = self.gen_expr(state, index, true).into_int_value();

                let len = match accessed_expr.ty.maybe_deref_once() {
                    Ty::Array(_, size) => Some(index.get_type().const_int(size as _, false)),
                    Ty::Slice(..) => Some(self.gen_load_slice_len(value)),
                    Ty::MultiPointer(..) => None,
                    ty => unreachable!("got {}", ty),
                };

                if let Some(len) = len {
                    self.gen_runtime_check_index_out_of_bounds(state, index, len, expr.span);
                }

                self.gen_subscript(value, &accessed_expr.ty, index, deref)
            }
            ExprKind::Slice {
                expr: sliced_expr,
                low,
                high,
            } => {
                let ptr = self
                    .gen_expr(state, sliced_expr, false)
                    .into_pointer_value();
                let ty = &sliced_expr.ty;

                let data = match ty {
                    Ty::Slice(..) => {
                        let data = self.gen_load_slice_data(ptr.into());
                        self.build_load(data.into()).into_pointer_value()
                    }
                    Ty::MultiPointer(..) => self.build_load(ptr.into()).into_pointer_value(),
                    Ty::Array(..) => ptr,
                    _ => unreachable!("got {}", ty),
                };

                let low = match low {
                    Some(low) => self.gen_expr(state, low, true).into_int_value(),
                    None => match high {
                        Some(high) => self.llvm_type(&high.ty).into_int_type(),
                        None => self.ptr_sized_int_type,
                    }
                    .const_zero(),
                };

                let high = match high {
                    Some(high) => self.gen_expr(state, high, true).into_int_value(),
                    None => match ty {
                        Ty::Array(_, len) => low.get_type().const_int(*len as u64, false),
                        Ty::Slice(..) => self.gen_load_slice_len(ptr.into()),
                        _ => unreachable!(),
                    },
                };

                self.gen_runtime_check_slice_end_before_start(state, low, high, expr.span);

                let len = match ty {
                    Ty::Slice(..) => Some(self.gen_load_slice_len(ptr.into())),
                    Ty::Array(_, size) => {
                        Some(self.ptr_sized_int_type.const_int(*size as _, false))
                    }
                    Ty::MultiPointer(..) => None,
                    _ => unreachable!("got {}", ty),
                };

                if let Some(len) = len {
                    self.gen_runtime_check_slice_range_out_of_bounds(
                        state, low, high, len, expr.span,
                    );
                }

                let slice_llvm_ty = self.llvm_type(&expr.ty);
                let slice_ptr = self.build_alloca(state, slice_llvm_ty);

                self.gen_slice(
                    slice_ptr,
                    data.into(),
                    low,
                    high,
                    &ty.element_type().unwrap(),
                );

                if deref {
                    self.build_load(slice_ptr.into())
                } else {
                    slice_ptr.into()
                }
            }
            ExprKind::Call(call) => self.gen_fn_call_expr(state, call, &expr.ty),
            ExprKind::MemberAccess {
                expr: accessed_expr,
                member: field,
            } => {
                let value = self.gen_expr(state, accessed_expr, false);

                let value = if accessed_expr.ty.is_pointer() {
                    self.build_load(value)
                } else {
                    value
                };

                let value = match &accessed_expr.ty.maybe_deref_once() {
                    ty @ Ty::Tuple(_) => {
                        let index = field.parse::<usize>().unwrap();
                        let llvm_ty = Some(self.llvm_type(ty));
                        self.gen_struct_access(value, index as u32, llvm_ty)
                    }
                    ty @ Ty::Struct(struct_ty) => {
                        let struct_llvm_ty = Some(self.llvm_type(ty));

                        if struct_ty.is_union() {
                            let field = struct_ty
                                .fields
                                .iter()
                                .find(|f| f.symbol == *field)
                                .unwrap();
                            let field_ty = self.llvm_type(&field.ty);
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
                                .position(|f| f.symbol == *field)
                                .unwrap();
                            self.gen_struct_access(value, index as u32, struct_llvm_ty)
                        }
                    }
                    Ty::Array(_, len) => match field.as_str() {
                        BUILTIN_FIELD_LEN => {
                            self.ptr_sized_int_type.const_int(*len as u64, false).into()
                        }
                        _ => unreachable!("got field `{}`", field),
                    },
                    Ty::Slice(..) => match field.as_str() {
                        BUILTIN_FIELD_LEN => self.gen_load_slice_len(value).into(),
                        BUILTIN_FIELD_DATA => self.gen_load_slice_data(value).into(),
                        _ => unreachable!("got field `{}`", field),
                    },
                    Ty::Module(id) => {
                        todo!()
                        // let decl = self.find_or_gen_top_level_decl(
                        //     ModuleInfo::new(*name, *file_path),
                        //     *field,
                        // );

                        // match decl {
                        //     CodegenDecl::Module { .. } => self.gen_unit(),
                        //     _ => {
                        //         let ptr = decl.into_pointer_value();

                        //         if deref {
                        //             self.build_load(ptr.into())
                        //         } else {
                        //             ptr.into()
                        //         }
                        //     }
                        // }
                    }
                    _ => unreachable!("invalid ty `{}`", accessed_expr.ty),
                };

                if deref {
                    self.build_load(value)
                } else {
                    value
                }
            }
            ExprKind::Id { symbol, .. } => {
                let decl = match state.env.get(&symbol) {
                    Some(decl) => decl.clone(),
                    None => self.find_or_gen_top_level_decl(state.module_info, *symbol),
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
            ExprKind::ArrayLiteral(kind) => {
                let array_ptr = match kind {
                    ArrayLiteralKind::List(elements) => {
                        let elements: Vec<BasicValueEnum> = elements
                            .iter()
                            .map(|e| self.gen_expr(state, e, true))
                            .collect();

                        let size = match &expr.ty {
                            Ty::Array(_, size) => *size,
                            _ => unreachable!("got ty `{}`", expr.ty),
                        };

                        let llvm_ty = self.llvm_type(&expr.ty);
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
                    ArrayLiteralKind::Fill {
                        expr: element_expr,
                        len: _,
                    } => {
                        let value = self.gen_expr(state, element_expr, true);

                        let array_type = self.llvm_type(&expr.ty);
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
            ExprKind::TupleLiteral(elements) => {
                if expr.ty.is_type() {
                    return self.gen_unit();
                }

                let values: Vec<BasicValueEnum> = elements
                    .iter()
                    .map(|e| self.gen_expr(state, e, true))
                    .collect();

                let llvm_ty = self.llvm_type(&expr.ty);
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
            ExprKind::StructLiteral {
                type_expr: _,
                fields,
            } => self.gen_struct_literal_named(state, &expr.ty, fields, deref),

            ExprKind::Literal(value) => self.gen_literal_value(value, &expr.ty, deref),

            ExprKind::Noop
            | ExprKind::PointerType(..)
            | ExprKind::MultiPointerType(..)
            | ExprKind::ArrayType(..)
            | ExprKind::SliceType(..)
            | ExprKind::StructType(..)
            | ExprKind::SelfType
            | ExprKind::UnitType
            | ExprKind::NeverType
            | ExprKind::PlaceholderType => self.gen_unit(),

            ExprKind::FnType(sig) => {
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

        if expr.ty.is_never() {
            self.build_unreachable();
        }

        value
    }

    pub(super) fn gen_expr_list(&mut self, state: &mut CodegenState<'ctx>, expr_list: &Vec<Expr>) {
        for expr in expr_list {
            self.gen_expr(state, expr, true);
        }
    }

    pub(super) fn gen_local_and_store_expr(
        &mut self,
        state: &mut CodegenState<'ctx>,
        symbol: Ustr,
        expr: &Option<Expr>,
        ty: &Ty,
    ) -> PointerValue<'ctx> {
        if let Some(expr) = expr {
            let ptr = self.gen_local_uninit(state, symbol, ty);
            let value = self.gen_expr(state, expr, true);
            self.build_store(ptr, value);
            ptr
        } else {
            self.gen_local_uninit(state, symbol, ty)
        }
    }

    pub(super) fn gen_cast(
        &mut self,
        state: &mut CodegenState<'ctx>,
        expr: &Box<Expr>,
        target_ty: &Ty,
    ) -> BasicValueEnum<'ctx> {
        let value = self.gen_expr(state, expr, true);
        self.gen_cast_internal(state, value, &expr.ty, target_ty)
    }

    fn gen_cast_internal(
        &mut self,
        state: &mut CodegenState<'ctx>,
        value: BasicValueEnum<'ctx>,
        from_ty: &Ty,
        target_ty: &Ty,
    ) -> BasicValueEnum<'ctx> {
        if from_ty == target_ty {
            return value;
        }

        const INST_NAME: &str = "cast";

        let cast_type = self.llvm_type(target_ty);

        match (from_ty, target_ty) {
            (Ty::Bool, Ty::Int(_)) | (Ty::Bool, Ty::UInt(_)) => self
                .builder
                .build_int_z_extend(value.into_int_value(), cast_type.into_int_type(), INST_NAME)
                .into(),
            (Ty::UInt(_), Ty::Int(_))
            | (Ty::Int(_), Ty::UInt(_))
            | (Ty::Int(_), Ty::Int(_))
            | (Ty::UInt(_), Ty::UInt(_)) => self
                .builder
                .build_int_cast(value.into_int_value(), cast_type.into_int_type(), INST_NAME)
                .into(),

            (Ty::Int(_), Ty::Float(_)) => self
                .builder
                .build_signed_int_to_float(
                    value.into_int_value(),
                    cast_type.into_float_type(),
                    INST_NAME,
                )
                .into(),

            (Ty::UInt(_), Ty::Float(_)) => self
                .builder
                .build_unsigned_int_to_float(
                    value.into_int_value(),
                    cast_type.into_float_type(),
                    INST_NAME,
                )
                .into(),

            (Ty::Float(_), Ty::Int(_)) => self
                .builder
                .build_float_to_signed_int(
                    value.into_float_value(),
                    cast_type.into_int_type(),
                    INST_NAME,
                )
                .into(),
            (Ty::Float(_), Ty::UInt(_)) => self
                .builder
                .build_float_to_unsigned_int(
                    value.into_float_value(),
                    cast_type.into_int_type(),
                    INST_NAME,
                )
                .into(),
            (Ty::Float(_), Ty::Float(_)) => self
                .builder
                .build_float_cast(
                    value.into_float_value(),
                    cast_type.into_float_type(),
                    INST_NAME,
                )
                .into(),

            (Ty::Pointer(..), Ty::Pointer(..))
            | (Ty::Pointer(..), Ty::MultiPointer(..))
            | (Ty::MultiPointer(..), Ty::Pointer(..)) => self
                .builder
                .build_pointer_cast(
                    value.into_pointer_value(),
                    cast_type.into_pointer_type(),
                    INST_NAME,
                )
                .into(),

            // pointer <=> int | uint
            (Ty::Pointer(..), Ty::Int(..)) | (Ty::Pointer(..), Ty::UInt(..)) => self
                .builder
                .build_ptr_to_int(
                    value.into_pointer_value(),
                    cast_type.into_int_type(),
                    INST_NAME,
                )
                .into(),

            // int | uint <=> pointer
            (Ty::Int(..), Ty::Pointer(..)) | (Ty::UInt(..), Ty::Pointer(..)) => self
                .builder
                .build_int_to_ptr(
                    value.into_int_value(),
                    cast_type.into_pointer_type(),
                    INST_NAME,
                )
                .into(),

            (Ty::Pointer(t, _), Ty::Slice(t_slice, ..))
            | (Ty::Slice(t_slice, ..), Ty::Pointer(t, _)) => match t.as_ref() {
                Ty::Array(_, size) => {
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
        deferred: &Vec<Expr>,
    ) -> BasicValueEnum<'ctx> {
        let abi_fn = self.fn_type_map.get(&state.fn_type).unwrap().clone();

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
