use super::{
    normalize::Normalize,
    ty_ctx::TyCtx,
    unify::{can_coerce_mut, UnifyTyResult},
};
use crate::hir;
use crate::types::{size::SizeOf, *};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CoercionResult {
    CoerceToLeft,
    CoerceToRight,
    NoCoercion,
}

trait Coerce {
    fn coerce(&self, to: &Type, word_size: usize) -> CoercionResult;
}

impl Coerce for Type {
    fn coerce(&self, to: &Type, word_size: usize) -> CoercionResult {
        use CoercionResult::*;

        match (self, to) {
            (Type::Infer(_, InferTy::AnyInt), Type::Infer(_, InferTy::AnyFloat)) => CoerceToRight,
            (Type::Infer(_, InferTy::AnyFloat), Type::Infer(_, InferTy::AnyInt)) => CoerceToLeft,

            // * int -> same or bigger int
            (Type::Int(left), Type::Int(right)) => {
                if left.size_of(word_size) <= right.size_of(word_size) {
                    CoerceToRight
                } else {
                    CoerceToLeft
                }
            }

            // * int -> same or bigger uint
            (Type::Int(left), Type::Uint(right)) => {
                if left.size_of(word_size) <= right.size_of(word_size) {
                    CoerceToRight
                } else {
                    CoerceToLeft
                }
            }

            // * uint -> same or bigger uint
            (Type::Uint(left), Type::Uint(right)) => {
                if left.size_of(word_size) <= right.size_of(word_size) {
                    CoerceToRight
                } else {
                    CoerceToLeft
                }
            }

            // * uint -> same or bigger int
            (Type::Uint(left), Type::Int(right)) => {
                if left.size_of(word_size) <= right.size_of(word_size) {
                    CoerceToRight
                } else {
                    CoerceToLeft
                }
            }

            // * float -> same or bigger float
            (Type::Float(left), Type::Float(right)) => {
                if left.size_of(word_size) <= right.size_of(word_size) {
                    CoerceToRight
                } else {
                    CoerceToLeft
                }
            }

            // * array[N] of T -> slice of T
            (Type::Pointer(t, lmut), Type::Slice(t_slice, rmut))
                if can_coerce_mut(*lmut, *rmut) =>
            {
                match t.as_ref() {
                    Type::Array(t_array, ..) => {
                        if t_array == t_slice {
                            CoerceToRight
                        } else {
                            NoCoercion
                        }
                    }
                    _ => NoCoercion,
                }
            }

            // * array[N] of T -> slice of T
            (Type::Pointer(t, lmut), Type::Slice(t_slice, rmut))
                if can_coerce_mut(*lmut, *rmut) =>
            {
                match t.as_ref() {
                    Type::Array(t_array, ..) => {
                        if t_array == t_slice {
                            CoerceToRight
                        } else {
                            NoCoercion
                        }
                    }
                    _ => NoCoercion,
                }
            }

            // * array[N] of T -> multi-pointer of T
            (Type::Pointer(t, lmut), Type::MultiPointer(t_ptr, rmut))
                if can_coerce_mut(*lmut, *rmut) =>
            {
                match t.as_ref() {
                    Type::Array(t_array, ..) => {
                        if t_array == t_ptr {
                            CoerceToRight
                        } else {
                            NoCoercion
                        }
                    }
                    _ => NoCoercion,
                }
            }

            // * array[N] of T -> multi-pointer of T
            (Type::Pointer(t, lmut), Type::MultiPointer(t_ptr, rmut))
                if can_coerce_mut(*lmut, *rmut) =>
            {
                match t.as_ref() {
                    Type::Array(t_array, ..) => {
                        if t_array == t_ptr {
                            CoerceToRight
                        } else {
                            NoCoercion
                        }
                    }
                    _ => NoCoercion,
                }
            }

            _ => {
                println!("{} {}", self, to);
                NoCoercion
            }
        }
    }
}

fn coerce_node(tycx: &mut TyCtx, node: &mut hir::Node, to: Type) {
    *node = hir::Node::Cast(hir::Cast {
        value: Box::new(node.clone()),
        ty: tycx.bound(to, node.span()),
        span: node.span(),
    })
}

pub trait OrCoerce {
    fn or_coerce(
        self,
        left: &mut hir::Node,
        right: &mut hir::Node,
        tycx: &mut TyCtx,
        word_size: usize,
    ) -> UnifyTyResult;
}

impl OrCoerce for UnifyTyResult {
    fn or_coerce(
        self,
        left: &mut hir::Node,
        right: &mut hir::Node,
        tycx: &mut TyCtx,
        word_size: usize,
    ) -> UnifyTyResult {
        match self {
            Ok(r) => Ok(r),
            Err(e) => {
                let (left_ty, right_ty) = (left.ty().normalize(tycx), right.ty().normalize(tycx));
                match left_ty.coerce(&right_ty, word_size) {
                    CoercionResult::CoerceToLeft => {
                        coerce_node(tycx, right, left_ty);
                        Ok(())
                    }
                    CoercionResult::CoerceToRight => {
                        coerce_node(tycx, left, right_ty);
                        Ok(())
                    }
                    CoercionResult::NoCoercion => Err(e),
                }
            }
        }
    }
}

pub trait OrCoerceIntoTy {
    fn or_coerce_into_ty(
        self,
        node: &mut hir::Node,
        ty: impl Normalize,
        tycx: &mut TyCtx,
        word_size: usize,
    ) -> UnifyTyResult;
}

impl OrCoerceIntoTy for UnifyTyResult {
    fn or_coerce_into_ty(
        self,
        node: &mut hir::Node,
        ty: impl Normalize,
        tycx: &mut TyCtx,
        word_size: usize,
    ) -> UnifyTyResult {
        match self {
            Ok(r) => Ok(r),
            Err(e) => {
                let (node_ty, ty) = (node.ty().normalize(tycx), ty.normalize(tycx));
                match node_ty.coerce(&ty, word_size) {
                    CoercionResult::CoerceToRight => {
                        coerce_node(tycx, node, ty);
                        Ok(())
                    }
                    CoercionResult::CoerceToLeft | CoercionResult::NoCoercion => Err(e),
                }
            }
        }
    }
}
