use chili_ast::{
    ast,
    ty::{size::SizeOf, TyKind},
};

use crate::{
    normalize::NormalizeTy,
    ty_ctx::TyCtx,
    unify::{can_coerce_mut, UnifyTyResult},
};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CoercionResult {
    CoerceToLeft,
    CoerceToRight,
    NoCoercion,
}

trait Coerce {
    fn coerce(&self, to: &TyKind, word_size: usize) -> CoercionResult;
}

impl Coerce for TyKind {
    fn coerce(&self, to: &TyKind, word_size: usize) -> CoercionResult {
        use CoercionResult::*;

        let (left, right) = (self, to);

        match (left, right) {
            (TyKind::AnyInt(_), TyKind::AnyFloat(_)) => CoerceToRight,
            (TyKind::AnyFloat(_), TyKind::AnyInt(_)) => CoerceToLeft,

            // * int -> same or bigger int
            (TyKind::Int(left), TyKind::Int(right)) => {
                if left.size_of(word_size) <= right.size_of(word_size) {
                    CoerceToRight
                } else {
                    CoerceToLeft
                }
            }

            // * int -> same or bigger uint
            (TyKind::Int(left), TyKind::UInt(right)) => {
                if left.size_of(word_size) <= right.size_of(word_size) {
                    CoerceToRight
                } else {
                    CoerceToLeft
                }
            }

            // * uint -> same or bigger uint
            (TyKind::UInt(left), TyKind::UInt(right)) => {
                if left.size_of(word_size) <= right.size_of(word_size) {
                    CoerceToRight
                } else {
                    CoerceToLeft
                }
            }

            // * uint -> same or bigger int
            (TyKind::UInt(left), TyKind::Int(right)) => {
                if left.size_of(word_size) <= right.size_of(word_size) {
                    CoerceToRight
                } else {
                    CoerceToLeft
                }
            }

            // * float -> same or bigger float
            (TyKind::Float(left), TyKind::Float(right)) => {
                if left.size_of(word_size) <= right.size_of(word_size) {
                    CoerceToRight
                } else {
                    CoerceToLeft
                }
            }

            // * array[N] of T -> slice of T
            (TyKind::Pointer(t, lmut), TyKind::Slice(t_slice, rmut))
                if can_coerce_mut(*lmut, *rmut) =>
            {
                match t.as_ref() {
                    TyKind::Array(t_array, ..) => {
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
            (TyKind::Slice(t_slice, lmut), TyKind::Pointer(t, rmut))
                if can_coerce_mut(*lmut, *rmut) =>
            {
                match t.as_ref() {
                    TyKind::Array(t_array, ..) => {
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
            (TyKind::Pointer(t, lmut), TyKind::MultiPointer(t_ptr, rmut))
                if can_coerce_mut(*lmut, *rmut) =>
            {
                match t.as_ref() {
                    TyKind::Array(t_array, ..) => {
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
            (TyKind::MultiPointer(t_ptr, lmut), TyKind::Pointer(t, rmut))
                if can_coerce_mut(*lmut, *rmut) =>
            {
                match t.as_ref() {
                    TyKind::Array(t_array, ..) => {
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

fn coerce_expr(tycx: &mut TyCtx, expr: &mut ast::Expr, to: TyKind) {
    let to = tycx.bound(to);
    *expr = ast::Expr::typed(
        ast::ExprKind::Cast(ast::Cast {
            expr: Box::new(expr.clone()),
            ty_expr: None,
            target_ty: to.clone(),
        }),
        to,
        expr.span,
    )
}

pub(crate) trait OrCoerceExprs {
    fn or_coerce_exprs(
        self,
        left: &mut ast::Expr,
        right: &mut ast::Expr,
        tycx: &mut TyCtx,
        word_size: usize,
    ) -> UnifyTyResult;
}

impl OrCoerceExprs for UnifyTyResult {
    fn or_coerce_exprs(
        self,
        left: &mut ast::Expr,
        right: &mut ast::Expr,
        tycx: &mut TyCtx,
        word_size: usize,
    ) -> UnifyTyResult {
        match self {
            Ok(r) => Ok(r),
            Err(e) => {
                let (left_ty, right_ty) = (left.ty.normalize(tycx), right.ty.normalize(tycx));
                match left_ty.coerce(&right_ty, word_size) {
                    CoercionResult::CoerceToLeft => {
                        coerce_expr(tycx, right, left_ty);
                        Ok(())
                    }
                    CoercionResult::CoerceToRight => {
                        coerce_expr(tycx, left, right_ty);
                        Ok(())
                    }
                    CoercionResult::NoCoercion => Err(e),
                }
            }
        }
    }
}

pub(crate) trait OrCoerceExprIntoTy {
    fn or_coerce_expr_into_ty(
        self,
        expr: &mut ast::Expr,
        ty: impl NormalizeTy,
        tycx: &mut TyCtx,
        word_size: usize,
    ) -> UnifyTyResult;
}

impl OrCoerceExprIntoTy for UnifyTyResult {
    fn or_coerce_expr_into_ty(
        self,
        expr: &mut ast::Expr,
        ty: impl NormalizeTy,
        tycx: &mut TyCtx,
        word_size: usize,
    ) -> UnifyTyResult {
        match self {
            Ok(r) => Ok(r),
            Err(e) => {
                let (expr_ty, ty) = (expr.ty.normalize(tycx), ty.normalize(tycx));
                match expr_ty.coerce(&ty, word_size) {
                    CoercionResult::CoerceToRight => {
                        coerce_expr(tycx, expr, ty);
                        Ok(())
                    }
                    CoercionResult::CoerceToLeft | CoercionResult::NoCoercion => Err(e),
                }
            }
        }
    }
}
