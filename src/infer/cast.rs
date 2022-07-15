use super::unify::can_coerce_mut;
use crate::types::*;

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

            (Type::Pointer(_, from_mutable), Type::Pointer(_, to_mutable))
                if can_coerce_mut(*from_mutable, *to_mutable) =>
            {
                true
            }

            (Type::Pointer(..), Type::Int(..)) | (Type::Pointer(..), Type::Uint(..)) => true,

            (Type::Int(..), Type::Pointer(..)) | (Type::Uint(..), Type::Pointer(..)) => true,

            (Type::Pointer(t, from_mutable), Type::Pointer(t_ptr, to_mutable))
                if can_coerce_mut(*from_mutable, *to_mutable) =>
            {
                match t.as_ref() {
                    Type::Array(t_array, ..) => t_array == t_ptr,
                    _ => false,
                }
            }

            (Type::Pointer(t, from_mutable), Type::Slice(t_slice, to_mutable))
                if can_coerce_mut(*from_mutable, *to_mutable) =>
            {
                match t.as_ref() {
                    Type::Array(t_array, ..) => t_array == t_slice,
                    _ => false,
                }
            }

            (Type::Var(_), _) | (_, Type::Var(_)) => true,

            _ => false,
        }
}
