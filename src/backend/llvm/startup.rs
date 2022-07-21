use super::codegen::{FunctionState, Generator};
use crate::{infer::normalize::Normalize, types::*};
use inkwell::{module::Linkage, values::BasicValue, AddressSpace};
use ustr::ustr;

impl<'g, 'ctx> Generator<'g, 'ctx> {
    pub(super) fn gen_entry_point_function(&mut self) {
        let name = self
            .workspace
            .build_options
            .entry_point_function_name()
            .unwrap();

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
        let startup_fn_type = FunctionType {
            params: vec![
                FunctionTypeParam {
                    name: ustr("argc"),
                    ty: Type::u32(),
                },
                FunctionTypeParam {
                    name: ustr("argv"),
                    ty: Type::u8().pointer_type(false).pointer_type(false),
                },
            ],
            return_type: Box::new(Type::u32()),
            varargs: None,
            kind: FunctionTypeKind::Orphan,
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

        let mut state = FunctionState::new(
            *root_module_info,
            function,
            startup_fn_type,
            None,
            decl_block,
            entry_block,
        );

        state.push_scope();

        self.start_block(&mut state, entry_block);

        self.startup_function_state = Some(state.clone());

        // Codegen the entry point function
        self.gen_top_level_binding(self.workspace.entry_point_function_id.unwrap());

        // Call the entry point function
        let entry_point_func_id = self.workspace.entry_point_function_id.unwrap();

        let entry_point_func_info = self
            .workspace
            .binding_infos
            .get(entry_point_func_id)
            .unwrap();

        let entry_point_func = self
            .global_decls
            .get(&entry_point_func_id)
            .unwrap()
            .into_function_value();

        let fn_ty = entry_point_func_info.ty.normalize(self.tcx).into_function();

        self.gen_function_call(
            &mut state,
            entry_point_func,
            &fn_ty,
            vec![],
            &fn_ty.return_type,
        );

        // TODO: if this is DLL Main, return 1 instead of 0

        if self.current_block().get_terminator().is_none() {
            self.builder
                .build_return(Some(&self.context.i32_type().const_zero()));
        }

        self.start_block(&mut state, decl_block);

        state.pop_scope();

        self.builder.build_unconditional_branch(entry_block);
    }
}
