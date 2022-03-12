use crate::coerce::can_coerce_mut;
use chili_ast::ty::*;

pub trait Cast {
    fn can_cast(&self, to: &Self) -> bool;
}

impl Cast for Ty {
    fn can_cast(&self, to: &Self) -> bool {
        self == to
            || match (self, to) {
                // bool <=> int
                (Ty::Bool, Ty::Int(_)) | (Ty::Bool, Ty::UInt(_)) => true,

                // int <=> int
                // int <=> uint
                // int <=> float
                (Ty::Int(_), Ty::Int(_))
                | (Ty::Int(_), Ty::UInt(_))
                | (Ty::Int(_), Ty::Float(_)) => true,

                // uint <=> int
                // uint <=> uint
                // uint <=> float
                (Ty::UInt(_), Ty::Int(_))
                | (Ty::UInt(_), Ty::UInt(_))
                | (Ty::UInt(_), Ty::Float(_)) => true,

                // float <=> int
                // float <=> uint
                // float <=> float
                (Ty::Float(_), Ty::Int(_))
                | (Ty::Float(_), Ty::UInt(_))
                | (Ty::Float(_), Ty::Float(_)) => true,

                // pointer <=> pointer
                (Ty::Pointer(..), Ty::Pointer(..)) => true,

                // pointer <=> int | uint
                (Ty::Pointer(..), Ty::Int(..))
                | (Ty::Pointer(..), Ty::UInt(..)) => true,

                // int | uint <=> pointer
                (Ty::Int(..), Ty::Pointer(..))
                | (Ty::UInt(..), Ty::Pointer(..)) => true,

                // pointer <=> multi-pointer
                (Ty::Pointer(t1, from_mutable), Ty::MultiPointer(t2, to_mutable))
                | (Ty::MultiPointer(t1, to_mutable), Ty::Pointer(t2, from_mutable))
                    if t1 == t2 && can_coerce_mut(*from_mutable, *to_mutable) =>
                {
                    true
                }

                // pointer(array) => multi-pointer
                (Ty::Pointer(t, from_mutable), Ty::MultiPointer(t_ptr, to_mutable))
                    if can_coerce_mut(*from_mutable, *to_mutable) =>
                {
                    match t.as_ref() {
                        Ty::Array(t_array, ..) => t_array == t_ptr,
                        _ => false,
                    }
                }

                // pointer(array) => slice
                (Ty::Pointer(t, from_mutable), Ty::Slice(t_slice, to_mutable))
                    if can_coerce_mut(*from_mutable, *to_mutable) =>
                {
                    match t.as_ref() {
                        Ty::Array(t_array, ..) => t_array == t_slice,
                        _ => false,
                    }
                }

                (Ty::Var(_), _) | (_, Ty::Var(_)) => true,

                _ => false,
            }
    }
}
