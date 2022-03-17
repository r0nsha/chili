use crate::tycx::{TyBinding, TyContext};
use chili_ast::{ty::Ty, workspace::Workspace};

pub trait NormalizeTy {
    fn normalize(&self, tycx: &TyContext, workspace: &Workspace) -> Self;
}

impl NormalizeTy for Ty {
    fn normalize(&self, tycx: &TyContext, workspace: &Workspace) -> Self {
        match self {
            Ty::Var(var) => match tycx.find_type_binding(*var) {
                TyBinding::Bound(ty) => ty.normalize(tycx, workspace),
                TyBinding::Unbound => self.clone(),
            },
            _ => self.clone(),
        }
    }
}
