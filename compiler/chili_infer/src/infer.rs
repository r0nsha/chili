use chili_ast::workspace::Workspace;

use crate::sess::TyVar;

pub(crate) trait Infer<'w> {
    fn infer(&mut self, workspace: Workspace<'w>) -> TyVar;
}
