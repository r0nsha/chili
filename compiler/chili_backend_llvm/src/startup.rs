use crate::codegen::{Codegen, CodegenState};
use chili_ast::{ast, ty::*};
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
        let startup_fn_type = FnTy {
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

        for (id, binding) in self.ast.bindings.iter() {
            let binding_info = self.workspace.get_binding_info(*id).unwrap();

            // if the binding has a constant value, its value is already initialized at compile time
            if binding_info.const_value.is_some() {
                continue;
            }

            match binding.expr.as_ref() {
                Some(expr) => match &expr.kind {
                    // if the binding is a fn or fn-type, don't initialize its value
                    // Note (Ron): i can probably come up with a cleaner solution here
                    ast::ExprKind::Fn(_) | ast::ExprKind::FnType(_) => continue,
                    _ => (),
                },
                None => (),
            }

            if let Some(ptr) = self.global_decls.get(id).map(|d| d.into_pointer_value()) {
                state.module_info = *self.workspace.get_module_info(binding.module_id).unwrap();
                let value = self.gen_expr(&mut state, binding.expr.as_ref().unwrap(), true);
                self.build_store(ptr, value);
            }
        }

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
}
