use super::codegen::Generator;
use crate::{hir, types::FunctionType};
use inkwell::{
    module::Linkage,
    values::{BasicValue, FunctionValue},
};

impl<'g, 'ctx> Generator<'g, 'ctx> {
    pub(super) fn gen_intrinsic(
        &mut self,
        intrinsic: &hir::Intrinsic,
        function_type: &FunctionType,
    ) -> FunctionValue<'ctx> {
        match intrinsic {
            hir::Intrinsic::StartWorkspace => self.get_or_create_intrinsic(intrinsic, |generator| {
                const NAME: &str = "intrinsic#start_workspace";

                let function = generator.declare_fn_sig(function_type, NAME, Some(Linkage::Private));

                let entry_block = generator.context.append_basic_block(function, "entry");

                generator.builder.position_at_end(entry_block);

                let return_ptr = function.get_first_param().unwrap().into_pointer_value();

                let str = generator.const_str_slice("", "");

                let return_value = generator
                    .const_struct(&[str.into(), generator.const_bool(false).into()])
                    .as_basic_value_enum();

                generator.build_store(return_ptr, return_value);

                generator.builder.build_return(None);

                function
            }),
            hir::Intrinsic::Location | hir::Intrinsic::CallerLocation => panic!(
                "intrinsic function '{}' should have been evaluated at compile-time",
                intrinsic
            ),
        }
    }

    fn get_or_create_intrinsic<F>(&mut self, intrinsic: &hir::Intrinsic, create_fn: F) -> FunctionValue<'ctx>
    where
        F: FnOnce(&mut Generator<'g, 'ctx>) -> FunctionValue<'ctx>,
    {
        self.intrinsics.get(intrinsic).cloned().unwrap_or_else(|| {
            let function = create_fn(self);
            self.intrinsics.insert(*intrinsic, function);
            function
        })
    }
}
