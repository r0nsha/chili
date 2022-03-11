use crate::{CheckFrame, CheckSess};
use chili_ast::ast::{Expr, UnaryOp};
use chili_ast::ty::*;
use chili_error::{DiagnosticResult, TypeError};
use chili_span::Span;

impl<'w, 'a> CheckSess<'w, 'a> {
    pub(super) fn check_unary_expr(
        &mut self,
        frame: &mut CheckFrame,
        op: UnaryOp,
        lhs: &mut Expr,
        expected_ty: Option<TyKind>,
        span: Span,
    ) -> DiagnosticResult<TyKind> {
        lhs.ty = self.check_expr(frame, lhs, expected_ty)?;

        let lhs_ty = self.infcx.normalize_ty(&lhs.ty);

        let ty = match op {
            UnaryOp::Ref(is_mutable_ref) => {
                if is_mutable_ref {
                    self.check_expr_can_be_mutably_referenced(&lhs)?;
                }

                TyKind::Pointer(Box::new(lhs_ty), is_mutable_ref)
            }
            UnaryOp::Deref => match lhs_ty {
                TyKind::Pointer(inner, _) => inner.as_ref().clone(),
                ty => {
                    return Err(TypeError::deref_non_pointer_ty(
                        span,
                        self.infcx.normalize_ty_and_untyped(&ty).to_string(),
                    ))
                }
            },
            UnaryOp::Not => {
                if !lhs_ty.is_bool() {
                    return Err(TypeError::invalid_ty_in_condition(
                        span,
                        self.infcx.normalize_ty_and_untyped(&lhs_ty).to_string(),
                    ));
                }

                lhs_ty
            }
            UnaryOp::Neg => {
                if !self.infcx.is_integer(&lhs_ty) && !self.infcx.is_float(&lhs_ty) {
                    return Err(TypeError::invalid_ty_in_unary(
                        span,
                        "neg",
                        self.infcx.normalize_ty_and_untyped(&lhs_ty).to_string(),
                    ));
                }

                lhs_ty
            }
            UnaryOp::Plus => {
                if !self.infcx.is_number(&lhs_ty) {
                    return Err(TypeError::invalid_ty_in_unary(
                        span,
                        "plus",
                        self.infcx.normalize_ty_and_untyped(&lhs_ty).to_string(),
                    ));
                }

                lhs_ty
            }
            UnaryOp::BitwiseNot => {
                if !self.infcx.is_any_integer(&lhs_ty) {
                    return Err(TypeError::invalid_ty_in_unary(
                        span,
                        "bitwise_not",
                        self.infcx.normalize_ty_and_untyped(&lhs_ty).to_string(),
                    ));
                }

                lhs_ty
            }
        };

        Ok(ty)
    }
}
