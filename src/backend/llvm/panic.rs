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
        let panic_fn = self.find_decl_by_name("std.panicking", "panic").into_function_value();
        let panic_binding_info = self.find_binding_info_by_name("std.panicking", "panic");
        let panic_fn_type = panic_binding_info.ty.clone().normalize(self.tcx).into_function();

        let location_llvm_type = panic_fn_type.params[0].ty.llvm_type(self);

        let file_path = self
            .const_str_slice("panic_file_path", state.module_info.file_path)
            .as_basic_value_enum();

        let line = self.context.i32_type().const_int(span.start.line as _, false);
        let column = self.context.i32_type().const_int(span.start.column as _, false);

        let location = self.build_struct(
            state,
            location_llvm_type.into(),
            &[file_path, line.into(), column.into()],
        );

        let location = self.build_load(location.into(), "");

        self.gen_function_call(
            state,
            panic_fn,
            &panic_fn_type,
            vec![location, message],
            &panic_fn_type.return_type,
        );

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
