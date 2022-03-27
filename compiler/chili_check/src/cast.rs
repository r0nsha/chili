use crate::unify::can_coerce_mut;
use chili_ast::ty::*;

pub trait CanCast<T> {
    fn can_cast(&self, to: &T) -> bool;
}

impl CanCast<TyKind> for TyKind {
    fn can_cast(&self, to: &TyKind) -> bool {
        self == to
            || match (self, to) {
                // bool <=> int
                (TyKind::Bool, TyKind::Int(_)) | (TyKind::Bool, TyKind::UInt(_)) => true,

                // int <=> int
                // int <=> uint
                // int <=> float
                (TyKind::Int(_), TyKind::Int(_))
                | (TyKind::Int(_), TyKind::UInt(_))
                | (TyKind::Int(_), TyKind::Float(_)) => true,

                // uint <=> int
                // uint <=> uint
                // uint <=> float
                (TyKind::UInt(_), TyKind::Int(_))
                | (TyKind::UInt(_), TyKind::UInt(_))
                | (TyKind::UInt(_), TyKind::Float(_)) => true,

                // float <=> int
                // float <=> uint
                // float <=> float
                (TyKind::Float(_), TyKind::Int(_))
                | (TyKind::Float(_), TyKind::UInt(_))
                | (TyKind::Float(_), TyKind::Float(_)) => true,

                // pointer <=> pointer
                (TyKind::Pointer(..), TyKind::Pointer(..)) => true,

                // pointer <=> int | uint
                (TyKind::Pointer(..), TyKind::Int(..))
                | (TyKind::Pointer(..), TyKind::UInt(..)) => true,

                // int | uint <=> pointer
                (TyKind::Int(..), TyKind::Pointer(..))
                | (TyKind::UInt(..), TyKind::Pointer(..)) => true,

                // pointer <=> multi-pointer
                (TyKind::Pointer(t1, from_mutable), TyKind::MultiPointer(t2, to_mutable))
                | (TyKind::MultiPointer(t1, to_mutable), TyKind::Pointer(t2, from_mutable))
                    if t1 == t2 && can_coerce_mut(*from_mutable, *to_mutable) =>
                {
                    true
                }

                // pointer(array) => multi-pointer
                (TyKind::Pointer(t, from_mutable), TyKind::MultiPointer(t_ptr, to_mutable))
                    if can_coerce_mut(*from_mutable, *to_mutable) =>
                {
                    match t.as_ref() {
                        TyKind::Array(t_array, ..) => t_array == t_ptr,
                        _ => false,
                    }
                }

                // pointer(array) => slice
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
