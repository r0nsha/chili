use crate::{
    display::DisplayTy, inference_value::InferenceValue, normalize::NormalizeTy, ty_ctx::TyCtx,
};
use chili_ast::ty::*;
use chili_error::diagnostic::{Diagnostic, Label};
use chili_span::Span;
use common::builtin::{BUILTIN_FIELD_DATA, BUILTIN_FIELD_LEN};
use ustr::ustr;

pub trait UnifyTy<T>
where
    Self: Sized,
    T: Sized,
{
    fn unify(&self, other: &T, tycx: &mut TyCtx) -> UnifyTyResult;
}

impl UnifyTy<Ty> for Ty {
    fn unify(&self, other: &Ty, tycx: &mut TyCtx) -> UnifyTyResult {
        let t1 = self.as_kind();
        let t2 = other.as_kind();
        t1.unify(&t2, tycx)
    }
}

impl UnifyTy<TyKind> for Ty {
    fn unify(&self, other: &TyKind, tycx: &mut TyCtx) -> UnifyTyResult {
        let ty = self.as_kind();
        ty.unify(other, tycx)
    }
}

impl UnifyTy<Ty> for TyKind {
    fn unify(&self, other: &Ty, tycx: &mut TyCtx) -> UnifyTyResult {
        let other = other.as_kind();
        self.unify(&other, tycx)
    }
}

impl UnifyTy<TyKind> for TyKind {
    fn unify(&self, other: &TyKind, tycx: &mut TyCtx) -> UnifyTyResult {
        match (self, other) {
            (TyKind::Unit, TyKind::Unit) => Ok(()),
            (TyKind::Bool, TyKind::Bool) => Ok(()),

            (TyKind::Int(t1), TyKind::Int(t2)) if t1 == t2 => Ok(()),
            (TyKind::Uint(t1), TyKind::Uint(t2)) if t1 == t2 => Ok(()),
            (TyKind::Float(t1), TyKind::Float(t2)) if t1 == t2 => Ok(()),

            (TyKind::Pointer(t1, a1), TyKind::Pointer(t2, a2))
            | (TyKind::MultiPointer(t1, a1), TyKind::MultiPointer(t2, a2))
            | (TyKind::Slice(t1, a1), TyKind::Slice(t2, a2)) => {
                if !can_coerce_mut(*a1, *a2) {
                    Err(UnifyTyErr::Mismatch)
                } else {
                    t1.unify(t2.as_ref(), tycx)?;
                    Ok(())
                }
            }

            (TyKind::Function(f1), TyKind::Function(f2)) => {
                f1.ret.unify(f2.ret.as_ref(), tycx)?;

                if f1.params.len() != f2.params.len() && !f1.variadic && !f2.variadic {
                    Err(UnifyTyErr::Mismatch)
                } else {
                    for (p1, p2) in f1.params.iter().zip(f2.params.iter()) {
                        p1.unify(p2, tycx)?;
                    }
                    Ok(())
                }
            }

            (TyKind::Array(t1, s1), TyKind::Array(t2, s2)) => {
                if *s1 != *s2 {
                    Err(UnifyTyErr::Mismatch)
                } else {
                    t1.unify(t2.as_ref(), tycx)?;
                    Ok(())
                }
            }

            (TyKind::Tuple(t1), TyKind::Tuple(t2)) => {
                if t1.len() != t2.len() {
                    Err(UnifyTyErr::Mismatch)
                } else {
                    for (t1, t2) in t1.iter().zip(t2.iter()) {
                        t1.unify(t2, tycx)?;
                    }
                    Ok(())
                }
            }

            (TyKind::Struct(t1), TyKind::Struct(t2)) => {
                if t1.binding_info_id == t2.binding_info_id {
                    Ok(())
                } else if t1.fields.len() != t2.fields.len() || t1.kind != t2.kind {
                    Err(UnifyTyErr::Mismatch)
                } else {
                    for f1 in t1.fields.iter() {
                        if let Some(f2) = t2.fields.iter().find(|f| f.symbol == f1.symbol) {
                            f1.ty.unify(&f2.ty, tycx)?;
                        } else {
                            return Err(UnifyTyErr::Mismatch);
                        }
                    }
                    Ok(())
                }
            }

            (TyKind::Type(t1), TyKind::Type(t2)) => t1.unify(t2.as_ref(), tycx),
            (TyKind::AnyType, TyKind::Type(_)) | (TyKind::Type(_), TyKind::AnyType) => Ok(()),

            (TyKind::Var(var) | TyKind::Infer(var, _), _) => unify_var_ty(*var, other, tycx),
            (_, TyKind::Var(var) | TyKind::Infer(var, _)) => unify_var_ty(*var, self, tycx),

            (TyKind::Never, _) | (_, TyKind::Never) => Ok(()),

            _ => Err(UnifyTyErr::Mismatch),
        }
    }
}

fn unify_var_ty(var: Ty, other: &TyKind, tycx: &mut TyCtx) -> UnifyTyResult {
    match tycx.value_of(var).clone() {
        InferenceValue::Bound(kind) => kind.unify(other, tycx),
        InferenceValue::AnyInt => {
            let other_kind = other.normalize(tycx);
            match other_kind {
                TyKind::Int(_) | TyKind::Uint(_) | TyKind::Float(_) => {
                    tycx.bind_ty(var, other_kind);
                    Ok(())
                }
                TyKind::Infer(other, InferTy::AnyInt | InferTy::AnyFloat) | TyKind::Var(other) => {
                    if other != var {
                        tycx.bind_ty(other, var.into());
                    }
                    Ok(())
                }
                _ => Err(UnifyTyErr::Mismatch),
            }
        }
        InferenceValue::AnyFloat => {
            let other_kind = other.normalize(tycx);
            match other_kind {
                TyKind::Float(_) => {
                    tycx.bind_ty(var, other_kind);
                    Ok(())
                }
                TyKind::Infer(other, InferTy::AnyInt | InferTy::AnyFloat) | TyKind::Var(other) => {
                    if other != var {
                        tycx.bind_ty(other, var.into());
                    }
                    Ok(())
                }
                _ => Err(UnifyTyErr::Mismatch),
            }
        }
        InferenceValue::PartialStruct(mut partial_struct) => {
            let other_kind = other.normalize(tycx);
            match other_kind.maybe_deref_once() {
                TyKind::Array(..) if partial_struct.contains_key(&ustr(BUILTIN_FIELD_LEN)) => {
                    tycx.bind_ty(var, other_kind);
                    Ok(())
                }
                TyKind::Slice(..)
                    if partial_struct.contains_key(&ustr(BUILTIN_FIELD_LEN))
                        || partial_struct.contains_key(&ustr(BUILTIN_FIELD_DATA)) =>
                {
                    tycx.bind_ty(var, other_kind);
                    Ok(())
                }
                TyKind::Struct(ref other_struct) => {
                    for (symbol, ty) in partial_struct.iter() {
                        // if both the partial struct and the struct have this field, unify their types
                        if let Some(other_ty) =
                            other_struct.fields.iter().find(|f| f.symbol == *symbol)
                        {
                            ty.unify(&other_ty.ty, tycx)?;
                        } else {
                            // any field that exists in the partial struct, but doesn't exist in struct, is an error
                            return Err(UnifyTyErr::Mismatch);
                        }
                    }

                    tycx.bind_ty(var, other_kind);

                    Ok(())
                }
                TyKind::Module(module_id) => {
                    // TODO: check that the symbols in this PartialStruct actually exist in this module
                    tycx.bind_ty(var, TyKind::Module(module_id));
                    Ok(())
                }
                TyKind::Infer(other, InferTy::PartialStruct(ref other_partial)) => {
                    for (symbol, ty) in partial_struct.iter() {
                        // if both partial structs have this field, unify their types
                        if let Some(other_ty) = other_partial.get(symbol) {
                            ty.unify(other_ty, tycx)?;
                        }
                    }

                    for (symbol, ty) in other_partial.iter() {
                        // if the other partial struct has fields that this struct doesn't, add them
                        if !partial_struct.contains_key(symbol) {
                            partial_struct.insert(*symbol, ty.clone());
                        }
                    }

                    // bind both vars to the new partial struct
                    let value = InferenceValue::PartialStruct(partial_struct);

                    tycx.bind_value(var, value.clone());
                    tycx.bind_value(other, value);

                    Ok(())
                }
                TyKind::Var(other) => {
                    if other != var {
                        tycx.bind_ty(other, var.into());
                    }

                    Ok(())
                }
                _ => Err(UnifyTyErr::Mismatch),
            }
        }
        InferenceValue::PartialTuple(partial_tuple) => {
            let other_kind = other.normalize(tycx);
            match other_kind {
                TyKind::Tuple(ref other_tuple)
                | TyKind::Infer(_, InferTy::PartialTuple(ref other_tuple)) => {
                    let mut any_err = false;

                    if other_tuple.len() < partial_tuple.len() {
                        any_err = true;
                    }

                    for (index, ty) in partial_tuple.iter().enumerate() {
                        if let Some(other) = other_tuple.get(index) {
                            any_err |= ty.unify(other, tycx).is_err();
                        }
                    }

                    tycx.bind_ty(var, other_kind);

                    if any_err {
                        Err(UnifyTyErr::Mismatch)
                    } else {
                        Ok(())
                    }
                }
                TyKind::Var(other) => {
                    if other != var {
                        tycx.bind_ty(other, var.into());
                    }

                    Ok(())
                }
                _ => Err(UnifyTyErr::Mismatch),
            }
        }
        InferenceValue::Unbound => {
            let other_kind = other.normalize(tycx);

            if occurs(var, &other_kind, tycx) {
                Err(UnifyTyErr::Occurs)
            } else {
                tycx.bind_ty(var, other.clone());
                Ok(())
            }
        }
    }
}

pub fn occurs(var: Ty, kind: &TyKind, tycx: &TyCtx) -> bool {
    match kind {
        &TyKind::Var(other) => {
            use InferenceValue::*;
            match tycx.value_of(other) {
                Bound(ty) => occurs(var, ty, tycx) || var == other,
                PartialStruct(partial) => {
                    partial.values().any(|ty| occurs(var, ty, tycx)) || var == other
                }
                PartialTuple(partial) => {
                    partial.iter().any(|ty| occurs(var, ty, tycx)) || var == other
                }
                AnyInt | AnyFloat | Unbound => var == other,
            }
        }
        TyKind::Function(f) => {
            f.params.iter().any(|p| occurs(var, p, tycx)) || occurs(var, &f.ret, tycx)
        }
        TyKind::Array(ty, _) => occurs(var, ty, tycx),
        TyKind::Tuple(tys) => tys.iter().any(|ty| occurs(var, ty, tycx)),
        TyKind::Struct(st) => st.fields.iter().any(|f| occurs(var, &f.ty, tycx)),
        TyKind::Infer(other, InferTy::PartialStruct(partial)) => {
            partial.values().any(|ty| occurs(var, ty, tycx)) || var == *other
        }
        TyKind::Infer(other, InferTy::PartialTuple(partial)) => {
            partial.iter().any(|ty| occurs(var, ty, tycx)) || var == *other
        }
        _ => false,
    }
}

pub type UnifyTyResult = Result<(), UnifyTyErr>;

#[derive(Debug)]
pub enum UnifyTyErr {
    Mismatch,
    Occurs,
}

impl UnifyTyErr {
    pub fn into_diagnostic(
        self,
        tycx: &TyCtx,
        expected: impl DisplayTy,
        expected_span: Option<Span>,
        found: impl DisplayTy,
        found_span: Span,
    ) -> Diagnostic {
        let expected = expected.display(tycx);
        let found = found.display(tycx);

        match self {
            UnifyTyErr::Mismatch => Diagnostic::error()
                .with_message(format!(
                    "mismatched types - expected {}, found {}",
                    expected, found
                ))
                .with_label(Label::primary(found_span, format!("expected {}", expected)))
                .maybe_with_label(
                    expected_span.map(|span| Label::secondary(span, "expected due to this")),
                ),
            UnifyTyErr::Occurs => Diagnostic::error()
                .with_message(format!("recursive type `{}` has infinite size", expected,))
                .with_label(Label::primary(found_span, "type is recursive")),
        }
    }
}

// NOTE (Ron): checks that mutability rules are equal
pub fn can_coerce_mut(from: bool, to: bool) -> bool {
    from == to || (!from && to)
}
