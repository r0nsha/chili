use crate::tyctx::{TyBinding, TyContext};
use chili_ast::{ty::Ty, workspace::Workspace};

pub trait NormalizeTy {
    fn normalize(&self, ctx: &TyContext, workspace: &Workspace) -> Self;
}

impl NormalizeTy for Ty {
    fn normalize(&self, ctx: &TyContext, workspace: &Workspace) -> Self {
        match self {
            Ty::Var(var) => match ctx.find_type_binding(*var) {
                TyBinding::Bound(ty) => ty.normalize(ctx, workspace),
                TyBinding::Unbound => self.clone(),
            },
            _ => self.clone(),
        }
    }
}
