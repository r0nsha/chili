use super::{display::DisplayType, inference_value::InferenceValue, normalize::Normalize, type_ctx::TypeCtx};
use crate::{
    common::builtin::{BUILTIN_FIELD_DATA, BUILTIN_FIELD_LEN},
    error::diagnostic::{Diagnostic, Label},
    span::Span,
    types::*,
};
use ustr::ustr;

pub trait UnifyType<T>
where
    Self: Sized,
    T: Sized,
{
    fn unify(&self, other: &T, tcx: &mut TypeCtx) -> UnifyTypeResult;
}

impl UnifyType<TypeId> for TypeId {
    fn unify(&self, other: &TypeId, tcx: &mut TypeCtx) -> UnifyTypeResult {
        let t1 = self.as_kind();
        let t2 = other.as_kind();
        t1.unify(&t2, tcx)
    }
}

impl UnifyType<Type> for TypeId {
    fn unify(&self, other: &Type, tcx: &mut TypeCtx) -> UnifyTypeResult {
        let ty = self.as_kind();
        ty.unify(other, tcx)
    }
}

impl UnifyType<TypeId> for Type {
    fn unify(&self, other: &TypeId, tcx: &mut TypeCtx) -> UnifyTypeResult {
        let other = other.as_kind();
        self.unify(&other, tcx)
    }
}

impl UnifyType<Type> for Type {
    fn unify(&self, other: &Type, tcx: &mut TypeCtx) -> UnifyTypeResult {
        match (self, other) {
            (Type::Unit, Type::Unit) => Ok(()),
            (Type::Bool, Type::Bool) => Ok(()),

            (Type::Int(t1), Type::Int(t2)) if t1 == t2 => Ok(()),
            (Type::Uint(t1), Type::Uint(t2)) if t1 == t2 => Ok(()),
            (Type::Float(t1), Type::Float(t2)) if t1 == t2 => Ok(()),

            (Type::Pointer(t1, m1), Type::Pointer(t2, m2)) => {
                if *m1 == *m2 {
                    t1.unify(t2.as_ref(), tcx)?;
                    Ok(())
                } else {
                    Err(UnifyTypeErr::Mismatch)
                }
            }

            (Type::Slice(t1), Type::Slice(t2)) => {
                t1.unify(t2.as_ref(), tcx)?;
                Ok(())
            }

            (Type::Str(t1), Type::Str(t2)) => {
                t1.unify(t2.as_ref(), tcx)?;
                Ok(())
            }

            (Type::Function(f1), Type::Function(f2)) => {
                for (p1, p2) in f1.params.iter().zip(f2.params.iter()) {
                    p1.ty.unify(&p2.ty, tcx)?;
                }

                f1.return_type.unify(f2.return_type.as_ref(), tcx)?;

                match (&f1.varargs, &f2.varargs) {
                    (Some(v1), Some(v2)) => match (&v1.ty, &v2.ty) {
                        (Some(vt1), Some(vt2)) => vt1.unify(vt2, tcx)?,
                        (None, None) => (),
                        _ => return Err(UnifyTypeErr::Mismatch),
                    },
                    (None, None) => {
                        if f1.params.len() != f2.params.len() {
                            return Err(UnifyTypeErr::Mismatch);
                        }
                    }
                    _ => return Err(UnifyTypeErr::Mismatch),
                }

                Ok(())
            }

            (Type::Array(t1, s1), Type::Array(t2, s2)) => {
                if *s1 != *s2 {
                    Err(UnifyTypeErr::Mismatch)
                } else {
                    t1.unify(t2.as_ref(), tcx)?;
                    Ok(())
                }
            }

            (Type::Tuple(t1), Type::Tuple(t2)) => {
                if t1.len() != t2.len() {
                    Err(UnifyTypeErr::Mismatch)
                } else {
                    for (t1, t2) in t1.iter().zip(t2.iter()) {
                        t1.unify(t2, tcx)?;
                    }
                    Ok(())
                }
            }

            (Type::Struct(t1), Type::Struct(t2)) => {
                if t1.binding_id == t2.binding_id {
                    Ok(())
                } else if t1.fields.len() != t2.fields.len() || t1.kind != t2.kind {
                    Err(UnifyTypeErr::Mismatch)
                } else {
                    for f1 in t1.fields.iter() {
                        if let Some(f2) = t2.field(f1.name) {
                            f1.ty.unify(&f2.ty, tcx)?;
                        } else {
                            return Err(UnifyTypeErr::Mismatch);
                        }
                    }
                    Ok(())
                }
            }

            (Type::Type(t1), Type::Type(t2)) => t1.unify(t2.as_ref(), tcx),
            (Type::AnyType, Type::Type(_)) | (Type::Type(_), Type::AnyType) => Ok(()),

            (Type::Var(var) | Type::Infer(var, _), _) => unify_var_ty(*var, other, tcx),
            (_, Type::Var(var) | Type::Infer(var, _)) => unify_var_ty(*var, self, tcx),

            (Type::Never, _) | (_, Type::Never) => Ok(()),

            _ => Err(UnifyTypeErr::Mismatch),
        }
    }
}

fn unify_var_ty(var: TypeId, other: &Type, tcx: &mut TypeCtx) -> UnifyTypeResult {
    match tcx.value_of(var).clone() {
        InferenceValue::Bound(kind) => kind.unify(other, tcx),
        InferenceValue::AnyInt => {
            let other_kind = other.normalize(tcx);
            match other_kind {
                Type::Int(_) | Type::Uint(_) | Type::Float(_) => {
                    tcx.bind_ty(var, other_kind);
                    Ok(())
                }
                Type::Infer(other, InferType::AnyInt | InferType::AnyFloat) | Type::Var(other) => {
                    if other != var {
                        tcx.bind_ty(other, var.into());
                    }
                    Ok(())
                }
                _ => Err(UnifyTypeErr::Mismatch),
            }
        }
        InferenceValue::AnyFloat => {
            let other_kind = other.normalize(tcx);
            match other_kind {
                Type::Float(_) => {
                    tcx.bind_ty(var, other_kind);
                    Ok(())
                }
                Type::Infer(other, InferType::AnyInt | InferType::AnyFloat) | Type::Var(other) => {
                    if other != var {
                        tcx.bind_ty(other, var.into());
                    }
                    Ok(())
                }
                _ => Err(UnifyTypeErr::Mismatch),
            }
        }
        InferenceValue::PartialStruct(mut partial_struct) => {
            let other_kind = other.normalize(tcx);
            match other_kind.maybe_deref_once() {
                Type::Array(..) if partial_struct.contains_key(&ustr(BUILTIN_FIELD_LEN)) => {
                    tcx.bind_ty(var, other_kind);
                    Ok(())
                }
                Type::Slice(_) | Type::Str(_)
                    if partial_struct.contains_key(&ustr(BUILTIN_FIELD_LEN))
                        || partial_struct.contains_key(&ustr(BUILTIN_FIELD_DATA)) =>
                {
                    tcx.bind_ty(var, other_kind);
                    Ok(())
                }
                Type::Struct(ref other_struct) => {
                    for (name, ty) in partial_struct.iter() {
                        // if both the partial struct and the struct have this field, unify their types
                        if let Some(other_ty) = other_struct.field(*name) {
                            ty.unify(&other_ty.ty, tcx)?;
                        } else {
                            // any field that exists in the partial struct, but doesn't exist in struct, is an error
                            return Err(UnifyTypeErr::Mismatch);
                        }
                    }

                    tcx.bind_ty(var, other_kind);

                    Ok(())
                }
                Type::Module(module_id) => {
                    // TODO: check that the names in this PartialStruct actually exist in this module
                    tcx.bind_ty(var, Type::Module(module_id));
                    Ok(())
                }
                Type::Infer(other, InferType::PartialStruct(ref other_partial)) => {
                    for (name, ty) in partial_struct.iter() {
                        // if both partial structs have this field, unify their types
                        if let Some(other_ty) = other_partial.get(name) {
                            ty.unify(other_ty, tcx)?;
                        }
                    }

                    for (name, ty) in other_partial.iter() {
                        // if the other partial struct has fields that this struct doesn't, add them
                        if !partial_struct.contains_key(name) {
                            partial_struct.insert(*name, ty.clone());
                        }
                    }

                    // bind both vars to the new partial struct
                    let value = InferenceValue::PartialStruct(partial_struct);

                    tcx.bind_value(var, value.clone());
                    tcx.bind_value(other, value);

                    Ok(())
                }
                Type::Var(other) => {
                    if other != var {
                        tcx.bind_ty(other, var.into());
                    }

                    Ok(())
                }
                _ => Err(UnifyTypeErr::Mismatch),
            }
        }
        InferenceValue::PartialTuple(partial_tuple) => {
            let other_kind = other.normalize(tcx);
            match other_kind {
                Type::Tuple(ref other_tuple) | Type::Infer(_, InferType::PartialTuple(ref other_tuple)) => {
                    let mut any_err = false;

                    if other_tuple.len() < partial_tuple.len() {
                        any_err = true;
                    }

                    for (index, ty) in partial_tuple.iter().enumerate() {
                        if let Some(other) = other_tuple.get(index) {
                            any_err |= ty.unify(other, tcx).is_err();
                        }
                    }

                    tcx.bind_ty(var, other_kind);

                    if any_err {
                        Err(UnifyTypeErr::Mismatch)
                    } else {
                        Ok(())
                    }
                }
                Type::Var(other) => {
                    if other != var {
                        tcx.bind_ty(other, var.into());
                    }

                    Ok(())
                }
                _ => Err(UnifyTypeErr::Mismatch),
            }
        }
        InferenceValue::Unbound => {
            let other_kind = other.normalize(tcx);

            if occurs(var, &other_kind, tcx) {
                Err(UnifyTypeErr::Occurs)
            } else {
                tcx.bind_ty(var, other.clone());
                Ok(())
            }
        }
    }
}

pub fn occurs(var: TypeId, kind: &Type, tcx: &TypeCtx) -> bool {
    match kind {
        &Type::Var(other) => {
            use InferenceValue::*;
            match tcx.value_of(other) {
                Bound(ty) => occurs(var, ty, tcx) || var == other,
                PartialStruct(partial) => partial.values().any(|ty| occurs(var, ty, tcx)) || var == other,
                PartialTuple(partial) => partial.iter().any(|ty| occurs(var, ty, tcx)) || var == other,
                AnyInt | AnyFloat | Unbound => var == other,
            }
        }
        Type::Function(f) => f.params.iter().any(|p| occurs(var, &p.ty, tcx)) || occurs(var, &f.return_type, tcx),
        Type::Array(ty, _) => occurs(var, ty, tcx),
        Type::Tuple(tys) => tys.iter().any(|ty| occurs(var, ty, tcx)),
        Type::Struct(st) => st.fields.iter().any(|f| occurs(var, &f.ty, tcx)),
        Type::Infer(other, InferType::PartialStruct(partial)) => {
            partial.values().any(|ty| occurs(var, ty, tcx)) || var == *other
        }
        Type::Infer(other, InferType::PartialTuple(partial)) => {
            partial.iter().any(|ty| occurs(var, ty, tcx)) || var == *other
        }
        _ => false,
    }
}

pub type UnifyTypeResult = Result<(), UnifyTypeErr>;

#[derive(Debug)]
pub enum UnifyTypeErr {
    Mismatch,
    Occurs,
}

impl UnifyTypeErr {
    pub fn into_diagnostic(
        self,
        tcx: &TypeCtx,
        expected: &impl DisplayType,
        expected_span: Option<Span>,
        found: &impl DisplayType,
        found_span: Span,
    ) -> Diagnostic {
        let expected = expected.display(tcx);
        let found = found.display(tcx);

        match self {
            UnifyTypeErr::Mismatch => Diagnostic::error()
                .with_message(format!("mismatched types - expected {}, found {}", expected, found))
                .with_label(Label::primary(found_span, format!("expected {}", expected)))
                .maybe_with_label(expected_span.map(|span| Label::secondary(span, "expected due to this"))),
            UnifyTypeErr::Occurs => Diagnostic::error()
                .with_message(format!("recursive type `{}` has infinite size", expected,))
                .with_label(Label::primary(found_span, "type is recursive")),
        }
    }
}

// NOTE (Ron): checks that mutability rules are equal
pub fn can_coerce_mut(from_mut: bool, to_mut: bool) -> bool {
    from_mut == to_mut || (from_mut && !to_mut)
}
