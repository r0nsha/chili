use crate::ty::IntoLlvmType;

use super::codegen::{Codegen, CodegenState};
use chili_infer::normalize::Normalize;
use chili_span::Span;
use inkwell::values::{BasicValueEnum, IntValue};

impl<'w, 'cg, 'ctx> Codegen<'cg, 'ctx> {
    #[allow(unused)]
    pub(super) fn gen_panic_with_message(
        &mut self,
        state: &mut CodegenState<'ctx>,
        msg: impl AsRef<str>,
        span: Span,
    ) {
        let message = self.gen_global_str("", msg.as_ref(), true);
        self.gen_panic(state, message, span)
    }

    pub(super) fn gen_panic(
        &mut self,
        state: &mut CodegenState<'ctx>,
        message: BasicValueEnum<'ctx>,
        span: Span,
    ) {
        let panic_fn = self
            .find_decl_by_name("std.panicking", "default_panic_handler")
            .into_function_value();

        let default_panic_handler_info =
            self.find_binding_info_by_name("std.panicking", "default_panic_handler");

        let panic_type = default_panic_handler_info
            .ty
            .clone()
            .normalize(self.tycx)
            .into_fn();

        let panic_info_llvm_type = panic_type.params.first().unwrap().llvm_type(self);

        let program = self.gen_global_str("panic_program", self.workspace.name.as_ref(), true);

        let file_path = self.gen_global_str("panic_file_path", state.module_info.file_path, true);

        let line = self
            .ptr_sized_int_type
            .const_int(span.start.line as _, false);

        let column = self
            .ptr_sized_int_type
            .const_int(span.start.column as _, false);

        let panic_info = self.gen_struct(
            state,
            panic_info_llvm_type.into(),
            &[program, message, file_path, line.into(), column.into()],
        );

        let panic_info = self.build_load(panic_info.into());

        self.gen_fn_call(
            state,
            panic_fn,
            &panic_type,
            vec![panic_info],
            &panic_type.ret,
        );

        self.build_unreachable();
    }

    pub(super) fn gen_conditional_panic(
        &mut self,
        state: &mut CodegenState<'ctx>,
        name: impl AsRef<str>,
        cond: IntValue<'ctx>,
        message: BasicValueEnum<'ctx>,
        span: Span,
    ) {
        let name = name.as_ref();

        let panic_block = self.append_basic_block(state, name);
        let no_panic_block = self.append_basic_block(state, "__no_panic");

        self.builder
            .build_conditional_branch(cond, panic_block, no_panic_block);

        self.start_block(state, panic_block);
        self.gen_panic(state, message, span);

        self.start_block(state, no_panic_block);
    }
}
