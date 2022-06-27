use super::unify::can_coerce_mut;
use crate::types::*;

pub fn can_cast_type(from: &Type, to: &Type) -> bool {
    from == to
        || match (from, to) {
            (Type::Bool, Type::Int(_)) | (Type::Bool, Type::Uint(_)) => true,

            (Type::Infer(_, InferTy::AnyInt), Type::Infer(_, InferTy::AnyInt))
            | (Type::Infer(_, InferTy::AnyInt), Type::Int(_))
            | (Type::Infer(_, InferTy::AnyInt), Type::Uint(_))
            | (Type::Infer(_, InferTy::AnyInt), Type::Infer(_, InferTy::AnyFloat))
            | (Type::Infer(_, InferTy::AnyInt), Type::Float(_)) => true,

            (Type::Int(_), Type::Int(_))
            | (Type::Int(_), Type::Uint(_))
            | (Type::Int(_), Type::Infer(_, InferTy::AnyFloat))
            | (Type::Int(_), Type::Float(_)) => true,

            (Type::Uint(_), Type::Int(_))
            | (Type::Uint(_), Type::Uint(_))
            | (Type::Uint(_), Type::Float(_)) => true,

            (Type::Infer(_, InferTy::AnyFloat), Type::Infer(_, InferTy::AnyInt))
            | (Type::Infer(_, InferTy::AnyFloat), Type::Int(_))
            | (Type::Infer(_, InferTy::AnyFloat), Type::Uint(_))
            | (Type::Infer(_, InferTy::AnyFloat), Type::Infer(_, InferTy::AnyFloat))
            | (Type::Infer(_, InferTy::AnyFloat), Type::Float(_)) => true,

            (Type::Float(_), Type::Int(_))
            | (Type::Float(_), Type::Uint(_))
            | (Type::Float(_), Type::Float(_)) => true,

            (Type::Pointer(..), Type::Pointer(..)) => true,

            (Type::Pointer(..), Type::Int(..)) | (Type::Pointer(..), Type::Uint(..)) => true,

            (Type::Int(..), Type::Pointer(..)) | (Type::Uint(..), Type::Pointer(..)) => true,

            (Type::Pointer(t1, from_mutable), Type::MultiPointer(t2, to_mutable))
            | (Type::MultiPointer(t1, to_mutable), Type::Pointer(t2, from_mutable))
                if t1 == t2 && can_coerce_mut(*from_mutable, *to_mutable) =>
            {
                true
            }

            (Type::Pointer(t, from_mutable), Type::MultiPointer(t_ptr, to_mutable))
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
