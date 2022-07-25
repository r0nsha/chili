use super::codegen::{FunctionState, Generator};
use super::ty::IntoLlvmType;
use crate::infer::normalize::Normalize;
use crate::span::Span;
use inkwell::values::{BasicValue, BasicValueEnum, IntValue};

impl<'g, 'ctx> Generator<'g, 'ctx> {
    #[allow(unused)]
    pub(super) fn gen_panic_with_message(&mut self, state: &mut FunctionState<'ctx>, msg: impl AsRef<str>, span: Span) {
        let message = self.const_str_slice("", msg.as_ref()).into();
        self.gen_panic(state, message, span)
    }

    pub(super) fn gen_panic(&mut self, state: &mut FunctionState<'ctx>, message: BasicValueEnum<'ctx>, span: Span) {
        let panic_fn = self
            .find_decl_by_name("std.panicking", "default_panic_handler")
            .into_function_value();

        let default_panic_handler_info = self.find_binding_info_by_name("std.panicking", "default_panic_handler");

        let panic_type = default_panic_handler_info
            .ty
            .clone()
            .normalize(self.tcx)
            .into_function();

        let panic_info_llvm_type = panic_type.params.first().unwrap().ty.llvm_type(self);

        let file_path = self
            .const_str_slice("panic_file_path", state.module_info.file_path)
            .as_basic_value_enum();

        let line = self.ptr_sized_int_type.const_int(span.start.line as _, false);

        let column = self.ptr_sized_int_type.const_int(span.start.column as _, false);

        let panic_info = self.build_struct(
            state,
            panic_info_llvm_type.into(),
            &[message, file_path, line.into(), column.into()],
        );

        let panic_info = self.build_load(panic_info.into());

        self.gen_function_call(state, panic_fn, &panic_type, vec![panic_info], &panic_type.return_type);

        self.build_unreachable();
    }

    pub(super) fn gen_conditional_panic(
        &mut self,
        state: &mut FunctionState<'ctx>,
        name: impl AsRef<str>,
        condition: IntValue<'ctx>,
        message: BasicValueEnum<'ctx>,
        span: Span,
    ) {
        let name = name.as_ref();

        let panic_block = self.append_basic_block(state, name);
        let no_panic_block = self.append_basic_block(state, "__no_panic");

        self.builder
            .build_conditional_branch(condition, panic_block, no_panic_block);

        self.start_block(state, panic_block);
        self.gen_panic(state, message, span);

        self.start_block(state, no_panic_block);
    }
}
