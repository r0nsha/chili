use chili_ast::workspace::Workspace;

use crate::ty::Ty;

pub(crate) trait Infer<'w> {
    fn infer(&mut self, workspace: Workspace<'w>) -> Ty;
}
