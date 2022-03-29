#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CoercionResult {
    CoerceToLeft,
    CoerceToRight,
    NoCoercion,
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
                let (left, right) = (expr.ty.normalize(tycx), ty.normalize(tycx));
                match left.coerce(&right, word_size) {
                    CoercionResult::CoerceToRight => {
                        coerce_expr(tycx, expr, right);
                        Ok(())
                    }
                    CoercionResult::CoerceToLeft | CoercionResult::NoCoercion => Err(e),
                }
            }
        }
    }
}

trait Coerce {
    fn coerce(&self, to: &TyKind, word_size: usize) -> CoercionResult;
}

impl Coerce for TyKind {
    fn coerce(&self, to: &TyKind, word_size: usize) -> CoercionResult {
        use CoercionResult::*;

        let (left, right) = (self, to);

        match (left, right) {
            // * int -> same or bigger int
            (TyKind::Int(left), TyKind::Int(right)) => {
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

// pub fn unify_or_coerce_expr_expr(
//     &mut self,
//     left_expr: &mut Expr,
//     right_expr: &mut Expr,
//     span: Span,
// ) -> DiagnosticResult<Ty> {
//     match self.unify_ty_ty(&left_expr.ty, &right_expr.ty, span) {
//         Ok(ty) => Ok(ty),
//         Err(_) => {
//             let left_expr_ty = self.normalize_ty(&left_expr.ty);
//             let right_expr_ty = self.normalize_ty(&right_expr.ty);

//             match left_expr_ty.try_coerce(&right_expr_ty, self.word_size) {
//                 CoercionResult::CoerceToLeft => {
//                     *right_expr = right_expr.coerce(left_expr_ty.clone());
//                     Ok(left_expr_ty)
//                 }
//                 CoercionResult::CoerceToRight => {
//                     *left_expr = left_expr.coerce(right_expr_ty.clone());
//                     Ok(right_expr_ty)
//                 }
//                 CoercionResult::NoCoercion => Err(self
//                     .map_unification_error(
//                         UnificationError(left_expr_ty, right_expr_ty),
//                         span,
//                     )),
//             }
//         }
//     }
// }

// pub fn unify_or_coerce_ty_expr(
//     &mut self,
//     ty: &Ty,
//     expr: &mut Expr,
//     span: Span,
// ) -> DiagnosticResult<Ty> {
//     match self.unify_ty_ty(ty, &expr.ty, span) {
//         Ok(ty) => Ok(ty),
//         Err(_) => {
//             let ty = self.normalize_ty(ty);
//             let expr_ty = self.normalize_ty(&expr.ty);

//             match expr_ty.try_coerce(&ty, self.word_size) {
//                 CoercionResult::CoerceToRight => {
//                     *expr = expr.coerce(ty.clone());
//                     Ok(ty)
//                 }
//                 CoercionResult::CoerceToLeft
//                 | CoercionResult::NoCoercion => Err(self
//                     .map_unification_error(
//                         UnificationError(ty, expr_ty),
//                         span,
//                     )),
//             }
//         }
//     }
// }

use chili_ast::{
    ast,
    ty::{size::SizeOf, TyKind},
};

use crate::{
    normalize::NormalizeTy,
    ty_ctx::TyCtx,
    unify::{can_coerce_mut, UnifyTyResult},
};
