use super::{
    display::DisplayTy, inference_value::InferenceValue, normalize::Normalize, ty_ctx::TyCtx,
};
use crate::{
    common::builtin::{BUILTIN_FIELD_DATA, BUILTIN_FIELD_LEN},
    error::diagnostic::{Diagnostic, Label},
    span::Span,
    types::*,
};
use ustr::ustr;

pub trait UnifyTy<T>
where
    Self: Sized,
    T: Sized,
{
    fn unify(&self, other: &T, tycx: &mut TyCtx) -> UnifyTyResult;
}

impl UnifyTy<TypeId> for TypeId {
    fn unify(&self, other: &TypeId, tycx: &mut TyCtx) -> UnifyTyResult {
        let t1 = self.as_kind();
        let t2 = other.as_kind();
        t1.unify(&t2, tycx)
    }
}

impl UnifyTy<Type> for TypeId {
    fn unify(&self, other: &Type, tycx: &mut TyCtx) -> UnifyTyResult {
        let ty = self.as_kind();
        ty.unify(other, tycx)
    }
}

impl UnifyTy<TypeId> for Type {
    fn unify(&self, other: &TypeId, tycx: &mut TyCtx) -> UnifyTyResult {
        let other = other.as_kind();
        self.unify(&other, tycx)
    }
}

impl UnifyTy<Type> for Type {
    fn unify(&self, other: &Type, tycx: &mut TyCtx) -> UnifyTyResult {
        match (self, other) {
            (Type::Unit, Type::Unit) => Ok(()),
            (Type::Bool, Type::Bool) => Ok(()),

            (Type::Int(t1), Type::Int(t2)) if t1 == t2 => Ok(()),
            (Type::Uint(t1), Type::Uint(t2)) if t1 == t2 => Ok(()),
            (Type::Float(t1), Type::Float(t2)) if t1 == t2 => Ok(()),

            (Type::Pointer(t1, a1), Type::Pointer(t2, a2))
            | (Type::MultiPointer(t1, a1), Type::MultiPointer(t2, a2))
            | (Type::Slice(t1, a1), Type::Slice(t2, a2)) => {
                if !can_coerce_mut(*a1, *a2) {
                    Err(UnifyTyErr::Mismatch)
                } else {
                    t1.unify(t2.as_ref(), tycx)?;
                    Ok(())
                }
            }

            (Type::Function(f1), Type::Function(f2)) => {
                for (p1, p2) in f1.params.iter().zip(f2.params.iter()) {
                    p1.unify(p2, tycx)?;
                }

                f1.return_type.unify(f2.return_type.as_ref(), tycx)?;

                match (&f1.varargs, &f2.varargs) {
                    (Some(v1), Some(v2)) => match (&v1.ty, &v2.ty) {
                        (Some(vt1), Some(vt2)) => vt1.unify(vt2, tycx)?,
                        (None, None) => (),
                        _ => return Err(UnifyTyErr::Mismatch),
                    },
                    (None, None) => {
                        if f1.params.len() != f2.params.len() {
                            return Err(UnifyTyErr::Mismatch);
                        }
                    }
                    _ => return Err(UnifyTyErr::Mismatch),
                }

                Ok(())
            }

            (Type::Array(t1, s1), Type::Array(t2, s2)) => {
                if *s1 != *s2 {
                    Err(UnifyTyErr::Mismatch)
                } else {
                    t1.unify(t2.as_ref(), tycx)?;
                    Ok(())
                }
            }

            (Type::Tuple(t1), Type::Tuple(t2)) => {
                if t1.len() != t2.len() {
                    Err(UnifyTyErr::Mismatch)
                } else {
                    for (t1, t2) in t1.iter().zip(t2.iter()) {
                        t1.unify(t2, tycx)?;
                    }
                    Ok(())
                }
            }

            (Type::Struct(t1), Type::Struct(t2)) => {
                if t1.binding_id == t2.binding_id {
                    Ok(())
                } else if t1.fields.len() != t2.fields.len() || t1.kind != t2.kind {
                    Err(UnifyTyErr::Mismatch)
                } else {
                    for f1 in t1.fields.iter() {
                        if let Some(f2) = t2.find_field(f1.name) {
                            f1.ty.unify(&f2.ty, tycx)?;
                        } else {
                            return Err(UnifyTyErr::Mismatch);
                        }
                    }
                    Ok(())
                }
            }

            (Type::Type(t1), Type::Type(t2)) => t1.unify(t2.as_ref(), tycx),
            (Type::AnyType, Type::Type(_)) | (Type::Type(_), Type::AnyType) => Ok(()),

            (Type::Var(var) | Type::Infer(var, _), _) => unify_var_ty(*var, other, tycx),
            (_, Type::Var(var) | Type::Infer(var, _)) => unify_var_ty(*var, self, tycx),

            (Type::Never, _) | (_, Type::Never) => Ok(()),

            _ => Err(UnifyTyErr::Mismatch),
        }
    }
}

fn unify_var_ty(var: TypeId, other: &Type, tycx: &mut TyCtx) -> UnifyTyResult {
    match tycx.value_of(var).clone() {
        InferenceValue::Bound(kind) => kind.unify(other, tycx),
        InferenceValue::AnyInt => {
            let other_kind = other.normalize(tycx);
            match other_kind {
                Type::Int(_) | Type::Uint(_) | Type::Float(_) => {
                    tycx.bind_ty(var, other_kind);
                    Ok(())
                }
                Type::Infer(other, InferTy::AnyInt | InferTy::AnyFloat) | Type::Var(other) => {
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
                Type::Float(_) => {
                    tycx.bind_ty(var, other_kind);
                    Ok(())
                }
                Type::Infer(other, InferTy::AnyInt | InferTy::AnyFloat) | Type::Var(other) => {
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
                Type::Array(..) if partial_struct.contains_key(&ustr(BUILTIN_FIELD_LEN)) => {
                    tycx.bind_ty(var, other_kind);
                    Ok(())
                }
                Type::Slice(..)
                    if partial_struct.contains_key(&ustr(BUILTIN_FIELD_LEN))
                        || partial_struct.contains_key(&ustr(BUILTIN_FIELD_DATA)) =>
                {
                    tycx.bind_ty(var, other_kind);
                    Ok(())
                }
                Type::Struct(ref other_struct) => {
                    for (symbol, ty) in partial_struct.iter() {
                        // if both the partial struct and the struct have this field, unify their types
                        if let Some(other_ty) = other_struct.find_field(*symbol) {
                            ty.unify(&other_ty.ty, tycx)?;
                        } else {
                            // any field that exists in the partial struct, but doesn't exist in struct, is an error
                            return Err(UnifyTyErr::Mismatch);
                        }
                    }

                    tycx.bind_ty(var, other_kind);

                    Ok(())
                }
                Type::Module(module_id) => {
                    // TODO: check that the symbols in this PartialStruct actually exist in this module
                    tycx.bind_ty(var, Type::Module(module_id));
                    Ok(())
                }
                Type::Infer(other, InferTy::PartialStruct(ref other_partial)) => {
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
                Type::Var(other) => {
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
                Type::Tuple(ref other_tuple)
                | Type::Infer(_, InferTy::PartialTuple(ref other_tuple)) => {
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
                Type::Var(other) => {
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

pub fn occurs(var: TypeId, kind: &Type, tycx: &TyCtx) -> bool {
    match kind {
        &Type::Var(other) => {
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
        Type::Function(f) => {
            f.params.iter().any(|p| occurs(var, p, tycx)) || occurs(var, &f.return_type, tycx)
        }
        Type::Array(ty, _) => occurs(var, ty, tycx),
        Type::Tuple(tys) => tys.iter().any(|ty| occurs(var, ty, tycx)),
        Type::Struct(st) => st.fields.iter().any(|f| occurs(var, &f.ty, tycx)),
        Type::Infer(other, InferTy::PartialStruct(partial)) => {
            partial.values().any(|ty| occurs(var, ty, tycx)) || var == *other
        }
        Type::Infer(other, InferTy::PartialTuple(partial)) => {
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
