use super::codegen::Generator;
use crate::{hir, types::FunctionType};
use inkwell::{module::Linkage, values::FunctionValue};

impl<'g, 'ctx> Generator<'g, 'ctx> {
    pub(super) fn gen_intrinsic(
        &mut self,
        intrinsic: &hir::Intrinsic,
        function_type: &FunctionType,
    ) -> FunctionValue<'ctx> {
        match intrinsic {
            hir::Intrinsic::StartWorkspace => self.get_or_create_intrinsic(intrinsic, |cg| {
                const NAME: &str = "intrinsic#start_workspace";
                let function = cg.declare_fn_sig(function_type, NAME, Some(Linkage::Private));

                let entry_block = cg.context.append_basic_block(function, "entry");

                cg.builder.position_at_end(entry_block);
                cg.builder.build_return(Some(&cg.unit_value()));

                function
            }),
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
