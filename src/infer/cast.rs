use super::unify::can_coerce_mut;
use crate::{hir::const_value::ConstValue, types::*};

pub fn can_cast_type(from: &Type, to: &Type) -> bool {
    from == to
        || match (from, to) {
            (Type::Bool, Type::Int(_)) | (Type::Bool, Type::Uint(_)) => true,

            (Type::Infer(_, InferType::AnyInt), Type::Infer(_, InferType::AnyInt))
            | (Type::Infer(_, InferType::AnyInt), Type::Int(_))
            | (Type::Infer(_, InferType::AnyInt), Type::Uint(_))
            | (Type::Infer(_, InferType::AnyInt), Type::Infer(_, InferType::AnyFloat))
            | (Type::Infer(_, InferType::AnyInt), Type::Float(_)) => true,

            (Type::Int(_), Type::Int(_))
            | (Type::Int(_), Type::Uint(_))
            | (Type::Int(_), Type::Infer(_, InferType::AnyFloat))
            | (Type::Int(_), Type::Float(_)) => true,

            (Type::Uint(_), Type::Int(_))
            | (Type::Uint(_), Type::Uint(_))
            | (Type::Uint(_), Type::Float(_)) => true,

            (Type::Infer(_, InferType::AnyFloat), Type::Infer(_, InferType::AnyInt))
            | (Type::Infer(_, InferType::AnyFloat), Type::Int(_))
            | (Type::Infer(_, InferType::AnyFloat), Type::Uint(_))
            | (Type::Infer(_, InferType::AnyFloat), Type::Infer(_, InferType::AnyFloat))
            | (Type::Infer(_, InferType::AnyFloat), Type::Float(_)) => true,

            (Type::Float(_), Type::Int(_))
            | (Type::Float(_), Type::Uint(_))
            | (Type::Float(_), Type::Float(_)) => true,

            (Type::Pointer(..), Type::Int(..)) | (Type::Pointer(..), Type::Uint(..)) => true,

            (Type::Int(..), Type::Pointer(..)) | (Type::Uint(..), Type::Pointer(..)) => true,

            (Type::Pointer(left, from_mutable), Type::Pointer(right, to_mutable)) => {
                if can_coerce_mut(*from_mutable, *to_mutable) {
                    match (left.as_ref(), right.as_ref()) {
                        (Type::Array(t_array, ..), Type::Slice(right, _)) => t_array == right,
                        (Type::Array(t_array, ..), right) => t_array.as_ref() == right,
                        (_, _) => true,
                    }
                } else {
                    false
                }
            }

            (Type::Var(_), _) | (_, Type::Var(_)) => true,

            _ => false,
        }
}

pub fn try_cast_const_value(const_value: &ConstValue, to: &Type) -> Option<ConstValue> {
    match (const_value, to) {
        (ConstValue::Bool(_), Type::Bool) => Some(const_value.clone()),
        (ConstValue::Bool(v), Type::Int(_)) => Some(ConstValue::Int(*v as i64)),
        (ConstValue::Bool(v), Type::Uint(_)) => Some(ConstValue::Uint(*v as u64)),

        (ConstValue::Int(_), Type::Int(_)) => Some(const_value.clone()),
        (ConstValue::Int(v), Type::Uint(_)) => Some(ConstValue::Uint(*v as u64)),
        (ConstValue::Int(v), Type::Float(_)) => Some(ConstValue::Float(*v as f64)),

        (ConstValue::Uint(_), Type::Uint(_)) => Some(const_value.clone()),
        (ConstValue::Uint(v), Type::Int(_)) => Some(ConstValue::Int(*v as i64)),
        (ConstValue::Uint(v), Type::Float(_)) => Some(ConstValue::Float(*v as f64)),

        (ConstValue::Float(_), Type::Float(_)) => Some(const_value.clone()),
        (ConstValue::Float(v), Type::Int(_)) => Some(ConstValue::Int(*v as i64)),
        (ConstValue::Float(v), Type::Uint(_)) => Some(ConstValue::Uint(*v as u64)),

        _ => None,
    }
}
