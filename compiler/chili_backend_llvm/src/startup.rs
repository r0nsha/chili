use crate::{
    codegen::{Codegen, CodegenState},
    ty::IntoLlvmType,
};
use chili_ast::{ast, pattern::Pattern, ty::*};
use chili_infer::normalize::NormalizeTy;
use inkwell::{
    module::Linkage,
    values::{BasicValue, FunctionValue},
    AddressSpace,
};

impl<'w, 'cg, 'ctx> Codegen<'cg, 'ctx> {
    pub(crate) fn gen_entry_point_function(&mut self) -> FunctionValue<'ctx> {
        let entry_point_func_id = self.workspace.entry_point_function_id.unwrap();

        let entry_point_func_info = self
            .workspace
            .get_binding_info(entry_point_func_id)
            .unwrap();

        let entry_point_func = self
            .global_decls
            .get(&entry_point_func_id)
            .unwrap()
            .into_function_value();

        let fn_ty = entry_point_func_info.ty.normalize(self.tycx).into_fn();

        let name = "main";

        let linkage = Some(Linkage::External);

        // let function = if os == Windows && BuildMode == DLL {
        //     self.module.add_function(
        //         "DllMain",
        //         ret_type.fn_type(&[
        //             "hinstDLL" -> rawptr,
        //             "fdwReason" -> u32,
        //             "lpReserved" -> rawptr,
        //         ], false),
        //         linkage,
        //     )
        // } else if (build_context.metrics.os == TargetOs_windows &&
        // (build_context.metrics.arch == TargetArch_386 ||
        // build_context.no_crt)) {     self.module.add_function(
        //         "mainCRTStartup",
        //         ret_type.fn_type(&[], false),
        //         linkage,
        //     )
        // } else if (is_arch_wasm()) {
        //     self.module.add_function(
        //         "_start",
        //         ret_type.fn_type(&[], false),
        //         linkage,
        //     )
        // } else {
        //     self.module.add_function(
        //         "main",
        //         ret_type.fn_type(&[
        //             self.context.i32_type(),
        //
        // self.context.i8_type().ptr_type(AddressSpace::Generic).
        // ptr_type(AddressSpace::Generic)         ], false),
        //         linkage,
        //     )
        // };
        let startup_fn_type = FunctionTy {
            params: vec![
                TyKind::Uint(UintTy::U32),
                TyKind::Uint(UintTy::U8)
                    .pointer_type(false)
                    .pointer_type(false),
            ],
            ret: Box::new(TyKind::Uint(UintTy::U32)),
            variadic: false,
            lib_name: None,
        };

        let function = self.module.add_function(
            name,
            self.context.i32_type().fn_type(
                &[
                    self.context.i32_type().into(),
                    self.context
                        .i8_type()
                        .ptr_type(AddressSpace::Generic)
                        .ptr_type(AddressSpace::Generic)
                        .into(),
                ],
                false,
            ),
            linkage,
        );

        function.get_nth_param(0).unwrap().set_name("argc");
        function.get_nth_param(1).unwrap().set_name("argv");

        let decl_block = self.context.append_basic_block(function, "decls");
        let entry_block = self.context.append_basic_block(function, "entry");

        let root_module_info = self.workspace.get_root_module_info();

        let mut state = CodegenState::new(
            *root_module_info,
            function,
            startup_fn_type,
            None,
            decl_block,
            entry_block,
        );

        state.push_scope();

        self.start_block(&mut state, entry_block);

        // we initialize the runtime known global bindings at the start of the program
        self.initialize_globals(&mut state);

        self.gen_fn_call(&mut state, entry_point_func, &fn_ty, vec![], &fn_ty.ret);

        // TODO: if this is DLL Main, return 1 instead of 0

        if self.current_block().get_terminator().is_none() {
            self.builder
                .build_return(Some(&self.context.i32_type().const_zero()));
        }

        self.start_block(&mut state, decl_block);

        state.pop_scope();

        self.builder.build_unconditional_branch(entry_block);

        self.verify_and_optimize_function(function, name)
    }

    fn initialize_globals(&mut self, state: &mut CodegenState<'ctx>) {
        for binding in self.typed_ast.bindings.iter() {
            // if all patterns are const, then there is no value to generate at runtime - so we skip
            if binding.pattern.iter().all(|p| !p.is_mutable) {
                continue;
            }

            let ty = binding.ty.llvm_type(self);

            // TODO: when hybrid patterns arrive, we can get the ptr to the hybrid pattern that has been generated
            let global_value = match &binding.pattern {
                Pattern::Symbol(pat) => self.global_decls.get(&pat.id).unwrap().into_global_value(),
                Pattern::StructUnpack(_) | Pattern::TupleUnpack(_) => {
                    let global_value = self.module.add_global(ty, None, "");
                    global_value.set_linkage(Linkage::Private);
                    global_value
                }
                Pattern::Hybrid(_) => todo!(),
            };

            let value = if let Some(expr) = &binding.expr {
                let old_module_info = state.module_info;
                state.module_info = *self.workspace.get_module_info(binding.module_id).unwrap();

                let value = self.gen_expr(state, expr, true);

                state.module_info = old_module_info;

                value
            } else {
                ty.const_zero()
            };

            let initializer = if binding.expr.as_ref().map_or(false, |expr| {
                matches!(&expr.kind, ast::ExprKind::ConstValue(..))
            }) {
                value
            } else {
                ty.const_zero()
            };

            global_value.set_initializer(&initializer);

            let global_ptr = global_value.as_pointer_value();
            match &binding.pattern {
                Pattern::Symbol(_) => {
                    self.build_store(global_ptr, value);
                }
                Pattern::StructUnpack(pat) => {
                    let ty = binding.ty.normalize(self.tycx);
                    let struct_ty = ty.maybe_deref_once().as_struct().clone();
                    let struct_llvm_type = Some(ty.llvm_type(self));

                    for pat in pat.symbols.iter() {
                        if pat.ignore {
                            continue;
                        }

                        let binding_info = self.workspace.get_binding_info(pat.id).unwrap();

                        if binding_info.const_value.is_some() {
                            continue;
                        }

                        if let Some(ptr) = self
                            .global_decls
                            .get(&pat.id)
                            .map(|d| d.into_pointer_value())
                        {
                            let field_index = struct_ty.find_field_position(pat.symbol).unwrap();

                            let field_value = self.gen_struct_access(
                                global_ptr.into(),
                                field_index as u32,
                                struct_llvm_type,
                            );

                            self.build_store(ptr, self.build_load(field_value));
                        }
                    }
                }
                Pattern::TupleUnpack(pat) => {
                    let ty = binding.ty.normalize(self.tycx);
                    let llvm_type = Some(ty.llvm_type(self));

                    for (i, pat) in pat.symbols.iter().enumerate() {
                        if pat.ignore {
                            continue;
                        }

                        let binding_info = self.workspace.get_binding_info(pat.id).unwrap();

                        if binding_info.const_value.is_some() {
                            continue;
                        }

                        if let Some(ptr) = self
                            .global_decls
                            .get(&pat.id)
                            .map(|d| d.into_pointer_value())
                        {
                            let field_value =
                                self.gen_struct_access(global_ptr.into(), i as u32, llvm_type);

                            self.build_store(ptr, self.build_load(field_value));
                        }
                    }
                }
                Pattern::Hybrid(_) => todo!(),
            }
        }
    }
}
