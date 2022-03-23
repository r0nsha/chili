use crate::{
    normalize::NormalizeTy,
    tycx::{InferenceValue, }, new_infer::InferSess,
};
use chili_ast::{ty::*, };

pub type UnifyTyResult = Result<(), UnifyTyErr>;

#[derive(Debug)]
pub enum UnifyTyErr {
    Mismatch,
    Occurs,
}

pub trait UnifyTy<T>
where
    Self: Sized,
    T: Sized,
{
    fn unify(&self, other: &T, sess:&mut InferSess ) -> UnifyTyResult;
}

impl UnifyTy<Ty> for Ty {
    fn unify(&self, other: &Ty, sess:&mut InferSess) -> UnifyTyResult {
        let t1 = TyKind::Var(*self);
        let t2 = TyKind::Var(*other);
        t1.unify(&t2, sess)
    }
}

impl UnifyTy<TyKind> for Ty {
    fn unify(&self, other: &TyKind, sess:&mut InferSess) -> UnifyTyResult {
        let ty = TyKind::Var(*self);
        ty.unify(other, sess)
    }
}

impl UnifyTy<Ty> for TyKind {
    fn unify(&self, other: &Ty, sess:&mut InferSess) -> UnifyTyResult {
        let other = TyKind::Var(*other);
        self.unify(&other, sess)
    }
}

impl UnifyTy<TyKind> for TyKind {
    fn unify(&self, other: &TyKind, sess:&mut InferSess) -> UnifyTyResult {
        match (self, other) {
            (TyKind::Unit, TyKind::Unit) => Ok(()),
            (TyKind::Bool, TyKind::Bool) => Ok(()),

            (TyKind::Int(t1), TyKind::Int(t2)) if t1 == t2 => Ok(()),
            (TyKind::UInt(t1), TyKind::UInt(t2)) if t1 == t2 => Ok(()),
            (TyKind::Float(t1), TyKind::Float(t2)) if t1 == t2 => Ok(()),

            // int/int
            (TyKind::AnyInt(var), ty @ TyKind::AnyInt(_))
            | (TyKind::AnyInt(var), ty @ TyKind::Int(_))
            | (ty @ TyKind::Int(_), TyKind::AnyInt(var))
            | (TyKind::AnyInt(var), ty @ TyKind::UInt(_))
            | (ty @ TyKind::UInt(_), TyKind::AnyInt(var))
            // int/float
            | (TyKind::AnyInt(var), ty @ TyKind::AnyFloat(_))
            | (ty @ TyKind::AnyFloat(_), TyKind::AnyInt(var))
            | (TyKind::AnyInt(var), ty @ TyKind::Float(_))
            | (ty @ TyKind::Float(_), TyKind::AnyInt(var))
            // float/float
            | (TyKind::AnyFloat(var), ty @ TyKind::AnyFloat(_))
            | (TyKind::AnyFloat(var), ty @ TyKind::Float(_))
            | (ty @ TyKind::Float(_), TyKind::AnyFloat(var)) => {
                sess.tycx.bind(*var, ty.clone());
                Ok(())
            }

            (TyKind::Pointer(t1, a1),TyKind::Pointer(t2, a2))
            | (TyKind::MultiPointer(t1, a1),TyKind::MultiPointer(t2, a2)) 
            | (TyKind::Slice(t1, a1),TyKind::Slice(t2, a2)) => {
                if !can_coerce_mut(*a1,*a2) {
                    Err(UnifyTyErr::Mismatch)
                } else {
                    t1.unify(t2.as_ref(), sess)?;
                    Ok(())
                }
            }

            (TyKind::Fn(f1),TyKind::Fn(f2)) => {
                if f1.params.len() != f2.params.len() || f1.variadic != f2.variadic {
                    Err(UnifyTyErr::Mismatch)
                } else {
                    for (p1, p2) in f1.params.iter().zip(f2.params.iter()) {
                        p1.ty.unify(&p2.ty, sess)?;
                    }
                    f1.ret.unify(f2.ret.as_ref(), sess)?;
                    Ok(())
                }
            }

            (TyKind::Array(t1, s1),TyKind::Array(t2, s2)) => {
                if *s1 != *s2 {
                    Err(UnifyTyErr::Mismatch)
                } else {
                    t1.unify(t2.as_ref(), sess)?;
                    Ok(())
                }
            }

            (TyKind::Tuple(t1),TyKind::Tuple(t2)) => {
                if t1.len() != t2.len() {
                    Err(UnifyTyErr::Mismatch)
                } else {
                    for (t1, t2) in t1.iter().zip(t2.iter()) {
                        t1.unify(t2, sess)?;
                    }
                    Ok(())
                }
            }

            (TyKind::Struct(t1),TyKind::Struct(t2)) => {
                if t1.binding_info_id == t2.binding_info_id {
                    Ok(())
                } else if t1.fields.len() != t2.fields.len() || t1.kind != t2.kind {
                    Err(UnifyTyErr::Mismatch)
                } else {
                    for (f1, f2) in t1.fields.iter().zip(t2.fields.iter()) {
                        f1.ty.unify(&f2.ty, sess)?;
                    }
                    Ok(())
                }
            }
            
            (TyKind::Type(t1), TyKind::Type(t2)) => t1.unify(t2.as_ref(), sess),
            (TyKind::Type(t1), t2) => t1.unify(t2, sess),
            (t1, TyKind::Type(t2)) => t1.unify(t2.as_ref(), sess),

            (TyKind::Var(var), _) => unify_var_ty(*var, other, sess),
            (_, TyKind::Var(var)) => unify_var_ty(*var, self, sess),

            (TyKind::Never, _) | (_, TyKind::Never) => Ok(()),

            _ => {
                // println!("{} <=> {}", self, other);
                Err(UnifyTyErr::Mismatch)},
        }
    }
}

fn unify_var_ty(var: Ty, other: &TyKind, sess:&mut InferSess) -> UnifyTyResult {
    match sess.tycx.value_of(var) {
        InferenceValue::Bound(kind) => kind.clone().unify(other, sess),
        InferenceValue::Unbound => {
            let other_norm = other.normalize(&sess.tycx);

            if TyKind::Var(var) != other_norm {
                if occurs(var, &other_norm, sess) {
                    Err(UnifyTyErr::Occurs)
                } else {
                    sess. tycx.bind(var, other_norm);
                    Ok(())
                }
            } else {
                Ok(())
            }
        }
    }
}

fn occurs(var: Ty, kind: &TyKind, sess:& InferSess) -> bool {
    match kind {
        TyKind::Var(other) => match sess.tycx.value_of(*other) {
            InferenceValue::Bound(ty) => occurs(var, &ty, sess),
            InferenceValue::Unbound => var == *other,
        },
        TyKind::Fn(f) => {
            f.params.iter().any(|p| occurs(var, &p.ty, sess))
                || occurs(var, &f.ret, sess)
        }
        TyKind::Pointer(ty, _)
        | TyKind::MultiPointer(ty, _)
        | TyKind::Array(ty, _)
        | TyKind::Slice(ty, _) => occurs(var, ty, sess),
        TyKind::Tuple(tys) => tys.iter().any(|ty| occurs(var, ty, sess)),
        TyKind::Struct(st) => st
            .fields
            .iter()
            .any(|f| occurs(var, &f.ty, sess)),
        _ => false,
    }
}

// NOTE (Ron): checks that mutability rules are equal
pub fn can_coerce_mut(from: bool, to: bool) -> bool {
    from == to || (!from && to)
}
