use chili_ast::workspace::Workspace;

use crate::sess::Ty;

pub(crate) trait Infer<'w> {
    fn infer(&mut self, workspace: Workspace<'w>) -> Ty;
}
