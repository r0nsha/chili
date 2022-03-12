use chili_ast::ast::{Cast, Expr, ExprKind};
use chili_ast::ty::{size::SizeOf, *};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CoercionResult {
    CoerceToLeft,
    CoerceToRight,
    NoCoercion,
}

pub trait TryCoerce {
    fn try_coerce(&self, to: &Ty, word_size: usize) -> CoercionResult;
}

impl TryCoerce for Ty {
    fn try_coerce(&self, to: &Ty, word_size: usize) -> CoercionResult {
        use CoercionResult::*;

        let (left, right) = (self, to);

        match (left, right) {
            // * int -> same or bigger int
            (Ty::Int(left), Ty::Int(right)) => {
                if left.size_of(word_size) <= right.size_of(word_size) {
                    CoerceToRight
                } else {
                    CoerceToLeft
                }
            }

            // * uint -> same or bigger uint
            (Ty::UInt(left), Ty::UInt(right)) => {
                if left.size_of(word_size) <= right.size_of(word_size) {
                    CoerceToRight
                } else {
                    CoerceToLeft
                }
            }

            // * float -> same or bigger float
            (Ty::Float(left), Ty::Float(right)) => {
                if left.size_of(word_size) <= right.size_of(word_size) {
                    CoerceToRight
                } else {
                    CoerceToLeft
                }
            }

            // * array[N] of T -> slice of T
            (Ty::Pointer(t, lmut), Ty::Slice(t_slice, rmut)) if can_coerce_mut(*lmut, *rmut) => {
                match t.as_ref() {
                    Ty::Array(t_array, ..) => {
                        if t_array == t_slice {
                            CoerceToRight
                        } else {
                            NoCoercion
                        }
                    }
                    _ => NoCoercion,
                }
            }

            // * slice of T <- array[N] of T
            (Ty::Slice(t_slice, lmut), Ty::Pointer(t, rmut)) if can_coerce_mut(*lmut, *rmut) => {
                match t.as_ref() {
                    Ty::Array(t_array, ..) => {
                        if t_array == t_slice {
                            CoerceToLeft
                        } else {
                            NoCoercion
                        }
                    }
                    _ => NoCoercion,
                }
            }

            // * array[N] of T -> multi-pointer of T
            (Ty::Pointer(t, lmut), Ty::MultiPointer(t_ptr, rmut))
                if can_coerce_mut(*lmut, *rmut) =>
            {
                match t.as_ref() {
                    Ty::Array(t_array, ..) => {
                        if t_array == t_ptr {
                            CoerceToRight
                        } else {
                            NoCoercion
                        }
                    }
                    _ => NoCoercion,
                }
            }

            // * multi-pointer of T <- array[N] of T
            (Ty::MultiPointer(t_ptr, lmut), Ty::Pointer(t, rmut))
                if can_coerce_mut(*lmut, *rmut) =>
            {
                match t.as_ref() {
                    Ty::Array(t_array, ..) => {
                        if t_array == t_ptr {
                            CoerceToLeft
                        } else {
                            NoCoercion
                        }
                    }
                    _ => NoCoercion,
                }
            }

            _ => NoCoercion,
        }
    }
}

pub(super) trait Coerce {
    fn coerce(&self, target_ty: Ty) -> Self;
}

impl Coerce for Expr {
    fn coerce(&self, target_ty: Ty) -> Self {
        let span = self.span;
        Expr::typed(
            ExprKind::Cast(Cast {
                expr: Box::new(self.clone()),
                type_expr: None,
                target_ty: target_ty.clone(),
            }),
            target_ty,
            span,
        )
    }
}

// NOTE (Ron): checks that mutability rules are equal
pub fn can_coerce_mut(from: bool, to: bool) -> bool {
    from == to || (!from && to)
}
