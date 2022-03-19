use chili_ast::{ast, workspace::Workspace};
use chili_error::DiagnosticResult;
use chili_infer::tycx::TyCtx;

pub(crate) trait TypeCk {
    fn typeck(&mut self, tycx: &mut TyCtx, workspace: &mut Workspace) -> DiagnosticResult<()>;
}

impl TypeCk for ast::Ast {
    fn typeck(&mut self, tycx: &mut TyCtx, workspace: &mut Workspace) -> DiagnosticResult<()> {
        for import in self.imports.iter_mut() {
            import.typeck(tycx, workspace)?;
        }

        for binding in self.bindings.iter_mut() {
            binding.typeck(tycx, workspace)?;
        }

        Ok(())
    }
}

impl TypeCk for ast::Import {
    fn typeck(&mut self, tycx: &mut TyCtx, workspace: &mut Workspace) -> DiagnosticResult<()> {
        Ok(())
    }
}

impl TypeCk for ast::Binding {
    fn typeck(&mut self, tycx: &mut TyCtx, workspace: &mut Workspace) -> DiagnosticResult<()> {
        Ok(())
    }
}
