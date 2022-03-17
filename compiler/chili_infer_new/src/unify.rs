use chili_ast::{ty::*, workspace::Workspace};
use chili_span::Span;

use crate::{
    normalize::NormalizeTy,
    tyctx::{TyBinding, TyContext},
};

pub(crate) type TyUnifyResult = Result<(), TyUnifyErr>;

#[derive(Debug)]
pub(crate) enum TyUnifyErr {
    Mismatch(Ty, Ty),
    Occurs(Ty, Ty),
}

pub(crate) trait Unify
where
    Self: Sized,
{
    fn unify(
        &self,
        other: &Self,
        ctx: &mut TyContext,
        workspace: &Workspace,
        span: Span,
    ) -> TyUnifyResult;
}

impl Unify for Ty {
    fn unify(
        &self,
        other: &Self,
        ctx: &mut TyContext,
        workspace: &Workspace,
        span: Span,
    ) -> TyUnifyResult {
        match (self, other) {
            (Ty::Unit, Ty::Unit) => Ok(()),
            (Ty::Bool, Ty::Bool) => Ok(()),
            (Ty::Int(t1), Ty::Int(t2)) if t1 == t2 => Ok(()),
            (Ty::UInt(t1), Ty::UInt(t2)) if t1 == t2 => Ok(()),
            (Ty::Float(t1), Ty::Float(t2)) if t1 == t2 => Ok(()),

            (Ty::AnyInt(var), ty @ Ty::Int(_))
            | (ty @ Ty::Int(_), Ty::AnyInt(var))
            | (Ty::AnyInt(var), ty @ Ty::UInt(_))
            | (ty @ Ty::UInt(_), Ty::AnyInt(var))
            | (Ty::AnyInt(var), ty @ Ty::Float(_))
            | (ty @ Ty::Float(_), Ty::AnyInt(var))
            | (Ty::AnyFloat(var), ty @ Ty::Float(_))
            | (ty @ Ty::Float(_), Ty::AnyFloat(var)) => {
                ctx.bind(*var, ty.clone());
                Ok(())
            }

            (Ty::Var(var), _) => unify_var_type(*var, self, other, ctx, workspace, span),
            (_, Ty::Var(var)) => unify_var_type(*var, other, self, ctx, workspace, span),

            (Ty::Never, _) | (_, Ty::Never) => Ok(()),

            _ => Err(TyUnifyErr::Mismatch(self.clone(), other.clone())),
        }
    }
}

fn unify_var_type(
    var: TyVar,
    t1: &Ty,
    t2: &Ty,
    ctx: &mut TyContext,
    workspace: &Workspace,
    span: Span,
) -> TyUnifyResult {
    match ctx.find_type_binding(var) {
        TyBinding::Bound(t) => t.unify(t2, ctx, workspace, span),
        TyBinding::Unbound => {
            let normalized = t2.normalize(ctx, workspace);

            if *t1 != normalized {
                if occurs(var, &normalized, ctx, workspace) {
                    Err(TyUnifyErr::Occurs(t1.clone(), t2.clone()))
                } else {
                    ctx.bind(var, normalized);
                    Ok(())
                }
            } else {
                Ok(())
            }
        }
    }
}

fn occurs(var: TyVar, ty: &Ty, ctx: &TyContext, workspace: &Workspace) -> bool {
    match ty {
        Ty::Var(other) => match ctx.find_type_binding(*other) {
            TyBinding::Bound(ty) => occurs(var, &ty, ctx, workspace),
            TyBinding::Unbound => var == *other,
        },
        Ty::Fn(f) => {
            f.params.iter().any(|p| occurs(var, &p.ty, ctx, workspace))
                || occurs(var, &f.ret, ctx, workspace)
        }
        Ty::Pointer(ty, _) | Ty::MultiPointer(ty, _) | Ty::Array(ty, _) | Ty::Slice(ty, _) => {
            occurs(var, ty, ctx, workspace)
        }
        Ty::Tuple(tys) => tys.iter().any(|ty| occurs(var, ty, ctx, workspace)),
        Ty::Struct(st) => st.fields.iter().any(|f| occurs(var, &f.ty, ctx, workspace)),
        _ => false,
    }
}
