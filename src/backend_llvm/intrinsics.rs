use inkwell::values::FunctionValue;

use super::codegen::Codegen;
use crate::{ast::ast, infer::normalize::Normalize};

impl<'cg, 'ctx> Codegen<'cg, 'ctx> {
    pub(super) fn gen_intrinsic(
        &mut self,
        binding: &ast::Binding,
        intrinsic: &ast::Intrinsic,
    ) -> FunctionValue<'ctx> {
        match intrinsic {
            ast::Intrinsic::StartWorkspace => self.get_or_create_intrinsic(intrinsic, |cg| {
                const NAME: &str = "intrinsic#start_workspace";
                let function = cg.declare_fn_sig(binding.ty.normalize(cg.tycx).as_fn(), NAME);

                let entry_block = cg.context.append_basic_block(function, "entry");

                cg.builder.position_at_end(entry_block);
                cg.builder.build_return(Some(&cg.gen_unit()));

                function
            }),
        }
    }

    fn get_or_create_intrinsic<F>(
        &mut self,
        intrinsic: &ast::Intrinsic,
        create_fn: F,
    ) -> FunctionValue<'ctx>
    where
        F: FnOnce(&mut Codegen<'cg, 'ctx>) -> FunctionValue<'ctx>,
    {
        self.intrinsics.get(intrinsic).cloned().unwrap_or_else(|| {
            let function = create_fn(self);
            self.intrinsics.insert(*intrinsic, function);
            function
        })
    }
}
