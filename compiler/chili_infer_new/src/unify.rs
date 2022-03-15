use chili_ast::{ty::*, workspace::Workspace};
use chili_span::Span;

use crate::sess::{InferSess, TyBinding, TyVar};

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
        sess: &mut InferSess,
        workspace: &Workspace,
        span: Span,
    ) -> TyUnifyResult;
}

impl Unify for Ty {
    fn unify(
        &self,
        other: &Self,
        sess: &mut InferSess,
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
                sess.bind(TyVar(*var), ty.clone());
                Ok(())
            }

            (Ty::Var(var), _) | (Ty::AnyInt(var), _) | (Ty::AnyFloat(var), _) => {
                unify_var_type(TyVar(*var), self, other, sess, workspace, span)
            }

            (_, Ty::Var(var)) | (_, Ty::AnyInt(var)) | (_, Ty::AnyFloat(var)) => {
                unify_var_type(TyVar(*var), other, self, sess, workspace, span)
            }

            (Ty::Never, _) | (_, Ty::Never) => Ok(()),

            _ => Err(TyUnifyErr::Mismatch(self.clone(), other.clone())),
        }
    }
}

fn unify_var_type(
    var: TyVar,
    t1: &Ty,
    t2: &Ty,
    sess: &mut InferSess,
    workspace: &Workspace,
    span: Span,
) -> TyUnifyResult {
    match sess.find_type_binding(var) {
        TyBinding::Bound(t) => t.unify(t2, sess, workspace, span),
        TyBinding::Unbound => {
            let normalized = normalize_ty(t2, sess, workspace);

            if *t1 != normalized {
                if occurs(var, &normalized, sess, workspace) {
                    Err(TyUnifyErr::Occurs(t1.clone(), t2.clone()))
                } else {
                    sess.bind(var, normalized);
                    Ok(())
                }
            } else {
                Ok(())
            }
        }
    }
}

fn normalize_ty(ty: &Ty, sess: &mut InferSess, workspace: &Workspace) -> Ty {
    match ty {
        Ty::Var(var) => match sess.find_type_binding(TyVar(*var)) {
            TyBinding::Bound(ty) => normalize_ty(&ty, sess, workspace),
            TyBinding::Unbound => ty.clone(),
        },
        _ => ty.clone(),
    }
}

fn occurs(var: TyVar, ty: &Ty, sess: &InferSess, workspace: &Workspace) -> bool {
    match ty {
        Ty::Var(other_var) => tyvars_match(var, TyVar(*other_var), sess, workspace),
        Ty::Fn(f) => {
            f.params.iter().any(|p| occurs(var, &p.ty, sess, workspace))
                || occurs(var, &f.ret, sess, workspace)
        }
        Ty::Pointer(ty, _) | Ty::MultiPointer(ty, _) | Ty::Array(ty, _) | Ty::Slice(ty, _) => {
            occurs(var, ty, sess, workspace)
        }
        Ty::Tuple(tys) => tys.iter().any(|ty| occurs(var, ty, sess, workspace)),
        Ty::Struct(st) => st
            .fields
            .iter()
            .any(|f| occurs(var, &f.ty, sess, workspace)),
        _ => false,
    }
}

fn tyvars_match(var: TyVar, other: TyVar, sess: &InferSess, workspace: &Workspace) -> bool {
    match sess.find_type_binding(other) {
        TyBinding::Bound(ty) => occurs(var, &ty, sess, workspace),
        TyBinding::Unbound => var == other,
    }
}
