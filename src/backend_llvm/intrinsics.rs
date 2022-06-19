use super::codegen::Codegen;
use crate::ast::{ast, ty::FunctionType};
use inkwell::{module::Linkage, values::FunctionValue};

impl<'cg, 'ctx> Codegen<'cg, 'ctx> {
    pub(super) fn gen_intrinsic(
        &mut self,
        intrinsic: &ast::Intrinsic,
        function_type: &FunctionType,
    ) -> FunctionValue<'ctx> {
        match intrinsic {
            ast::Intrinsic::StartWorkspace => self.get_or_create_intrinsic(intrinsic, |cg| {
                const NAME: &str = "intrinsic#start_workspace";
                let function = cg.declare_fn_sig(function_type, NAME, Some(Linkage::Private));

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
