use crate::unify::can_coerce_mut;
use chili_ast::ty::*;

pub trait CanCast<T> {
    fn can_cast(&self, to: &T) -> bool;
}

impl CanCast<TyKind> for TyKind {
    fn can_cast(&self, to: &TyKind) -> bool {
        self == to
            || match (self, to) {
                (TyKind::Bool, TyKind::Int(_)) | (TyKind::Bool, TyKind::Uint(_)) => true,

                (TyKind::Infer(_, InferTy::AnyInt), TyKind::Infer(_, InferTy::AnyInt))
                | (TyKind::Infer(_, InferTy::AnyInt), TyKind::Int(_))
                | (TyKind::Infer(_, InferTy::AnyInt), TyKind::Uint(_))
                | (TyKind::Infer(_, InferTy::AnyInt), TyKind::Infer(_, InferTy::AnyFloat))
                | (TyKind::Infer(_, InferTy::AnyInt), TyKind::Float(_)) => true,

                (TyKind::Int(_), TyKind::Int(_))
                | (TyKind::Int(_), TyKind::Uint(_))
                | (TyKind::Int(_), TyKind::Infer(_, InferTy::AnyFloat))
                | (TyKind::Int(_), TyKind::Float(_)) => true,

                (TyKind::Uint(_), TyKind::Int(_))
                | (TyKind::Uint(_), TyKind::Uint(_))
                | (TyKind::Uint(_), TyKind::Float(_)) => true,

                (TyKind::Infer(_, InferTy::AnyFloat), TyKind::Infer(_, InferTy::AnyInt))
                | (TyKind::Infer(_, InferTy::AnyFloat), TyKind::Int(_))
                | (TyKind::Infer(_, InferTy::AnyFloat), TyKind::Uint(_))
                | (TyKind::Infer(_, InferTy::AnyFloat), TyKind::Infer(_, InferTy::AnyFloat))
                | (TyKind::Infer(_, InferTy::AnyFloat), TyKind::Float(_)) => true,

                (TyKind::Float(_), TyKind::Int(_))
                | (TyKind::Float(_), TyKind::Uint(_))
                | (TyKind::Float(_), TyKind::Float(_)) => true,

                (TyKind::Pointer(..), TyKind::Pointer(..)) => true,

                (TyKind::Pointer(..), TyKind::Int(..))
                | (TyKind::Pointer(..), TyKind::Uint(..)) => true,

                (TyKind::Int(..), TyKind::Pointer(..))
                | (TyKind::Uint(..), TyKind::Pointer(..)) => true,

                (TyKind::Pointer(t1, from_mutable), TyKind::MultiPointer(t2, to_mutable))
                | (TyKind::MultiPointer(t1, to_mutable), TyKind::Pointer(t2, from_mutable))
                    if t1 == t2 && can_coerce_mut(*from_mutable, *to_mutable) =>
                {
                    true
                }

                (TyKind::Pointer(t, from_mutable), TyKind::MultiPointer(t_ptr, to_mutable))
                    if can_coerce_mut(*from_mutable, *to_mutable) =>
                {
                    match t.as_ref() {
                        TyKind::Array(t_array, ..) => t_array == t_ptr,
                        _ => false,
                    }
                }

                (TyKind::Pointer(t, from_mutable), TyKind::Slice(t_slice, to_mutable))
                    if can_coerce_mut(*from_mutable, *to_mutable) =>
                {
                    match t.as_ref() {
                        TyKind::Array(t_array, ..) => t_array == t_slice,
                        _ => false,
                    }
                }

                (TyKind::Var(_), _) | (_, TyKind::Var(_)) => true,

                _ => false,
            }
    }
}
