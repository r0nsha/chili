use chili_ast::workspace::Workspace;

use crate::ty::TyVar;

pub(crate) trait Infer<'w> {
    fn infer(&mut self, workspace: Workspace<'w>) -> TyVar;
}
