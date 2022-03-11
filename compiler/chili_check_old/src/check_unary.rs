use crate::{CheckContext, CheckFrame, CheckedExpr};
use chili_ast::{
    ast::{Expr, ExprKind, LiteralKind, UnaryOp},
    value::Value,
};
use chili_error::{DiagnosticResult, TypeError};
use chili_span::Span;
use chili_ast::ty::*;

impl<'a> CheckContext<'a> {
    pub(super) fn check_unary_expr(
        &mut self,
        frame: &mut CheckFrame,
        op: UnaryOp,
        lhs: &Expr,
        expected_ty: Option<Ty>,
        span: Span,
    ) -> DiagnosticResult<CheckedExpr> {
        let lhs = self.check_expr(frame, lhs, expected_ty)?;
        let lhs_ty = self.infcx.normalize_ty(&lhs.ty);

        let ty = match op {
            UnaryOp::Ref(is_mutable_ref) => {
                if is_mutable_ref {
                    self.check_expr_can_be_mutably_referenced(&lhs.expr)?;
                }

                Ty::Pointer(Box::new(lhs_ty), is_mutable_ref)
            }
            UnaryOp::Deref => match lhs_ty {
                Ty::Pointer(inner, _) => inner.as_ref().clone(),
                ty => {
                    return Err(TypeError::deref_non_pointer_ty(
                        span,
                        &self.infcx.normalize_ty_and_untyped(&ty),
                    ))
                }
            },
            UnaryOp::Not => {
                if !lhs_ty.is_bool() {
                    return Err(TypeError::invalid_ty_in_condition(
                        span,
                        &self.infcx.normalize_ty_and_untyped(&lhs_ty),
                    ));
                }

                if lhs.value.is_some() {
                    let lhs = lhs.value.unwrap().into_bool();
                    let result_value = !lhs;

                    return Ok(CheckedExpr::new(
                        ExprKind::Literal(LiteralKind::Bool(result_value)),
                        lhs_ty,
                        Some(Value::Bool(result_value)),
                        span,
                    ));
                } else {
                    lhs_ty
                }
            }
            UnaryOp::Neg => {
                if !self.infcx.is_integer(&lhs_ty)
                    && !self.infcx.is_float(&lhs_ty)
                {
                    return Err(TypeError::invalid_ty_in_unary(
                        span,
                        "neg",
                        &self.infcx.normalize_ty_and_untyped(&lhs_ty),
                    ));
                }

                if lhs.value.is_some() {
                    let (expr_kind, value) = match lhs.value.unwrap() {
                        Value::Int(i) => {
                            let result_value = -i;
                            (
                                ExprKind::Literal(LiteralKind::Int(
                                    result_value,
                                )),
                                Value::Int(result_value),
                            )
                        }
                        Value::Float(f) => {
                            let result_value = -f;
                            (
                                ExprKind::Literal(LiteralKind::Float(
                                    result_value,
                                )),
                                Value::Float(result_value),
                            )
                        }
                        value => unreachable!("got {}", value),
                    };

                    return Ok(CheckedExpr::new(
                        expr_kind,
                        lhs_ty,
                        Some(value),
                        span,
                    ));
                } else {
                    lhs_ty
                }
            }
            UnaryOp::Plus => {
                if !self.infcx.is_number(&lhs_ty) {
                    return Err(TypeError::invalid_ty_in_unary(
                        span,
                        "plus",
                        &self.infcx.normalize_ty_and_untyped(&lhs_ty),
                    ));
                }

                lhs_ty
            }
            UnaryOp::BitwiseNot => {
                if !self.infcx.is_any_integer(&lhs_ty) {
                    return Err(TypeError::invalid_ty_in_unary(
                        span,
                        "bitwise_not",
                        &self.infcx.normalize_ty_and_untyped(&lhs_ty),
                    ));
                }

                lhs_ty
            }
        };

        Ok(CheckedExpr::new(
            ExprKind::Unary {
                op,
                lhs: Box::new(lhs.expr),
            },
            ty,
            None,
            span,
        ))
    }
}
