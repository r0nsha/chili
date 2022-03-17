use crate::tycx::{TyBinding, TyContext};
use chili_ast::ty::*;

pub trait NormalizeTy {
    fn normalize(&self, tycx: &TyContext) -> TyKind;
}

impl NormalizeTy for Ty {
    fn normalize(&self, tycx: &TyContext) -> TyKind {
        match tycx.find_type_binding(*self) {
            TyBinding::Bound(ty) => ty.normalize(tycx),
            TyBinding::Unbound => TyKind::Var(*self),
        }
    }
}

impl NormalizeTy for TyKind {
    fn normalize(&self, tycx: &TyContext) -> TyKind {
        match self {
            TyKind::Var(var) => var.normalize(tycx),
            _ => self.clone(),
        }
    }
}
