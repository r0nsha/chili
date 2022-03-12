use chili_ast::{ty::TyVar, workspace::Workspace};

pub(crate) trait Infer<'w> {
    fn infer(&mut self, workspace: Workspace<'w>) -> TyVar;
}
