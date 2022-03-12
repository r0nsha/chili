use crate::const_fold::binary::const_fold_binary;
use crate::{CheckFrame, CheckSess, CheckedExpr};
use chili_ast::ast::{BinaryOp, Expr, ExprKind};
use chili_ast::ty::*;
use chili_error::{DiagnosticResult, TypeError};
use chili_span::Span;

impl<'c> CheckSess<'c> {
    #[inline]
    pub(super) fn check_binary_expr(
        &mut self,
        frame: &mut CheckFrame,
        lhs: &Box<Expr>,
        op: BinaryOp,
        rhs: &Box<Expr>,
        expected_ty: Option<TyKind>,
        span: Span,
    ) -> DiagnosticResult<CheckedExpr> {
        let mut lhs = self.check_expr(frame, lhs, expected_ty.clone())?;
        let mut rhs = self.check_expr(frame, rhs, expected_ty)?;

        let rhs_span = rhs.expr.span;
        let ty = self
            .infcx
            .unify_or_coerce_expr_expr(&mut lhs.expr, &mut rhs.expr, rhs_span)?;

        match op {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::Rem
            | BinaryOp::Lt
            | BinaryOp::LtEq
            | BinaryOp::Gt
            | BinaryOp::GtEq => {
                if !self.infcx.is_number(&ty) {
                    return Err(TypeError::expected(
                        span,
                        self.infcx.normalize_ty_and_untyped(&ty).to_string(),
                        "number",
                    ));
                }
            }

            BinaryOp::Shl
            | BinaryOp::Shr
            | BinaryOp::BitwiseOr
            | BinaryOp::BitwiseXor
            | BinaryOp::BitwiseAnd => {
                if !self.infcx.is_any_integer(&ty) {
                    return Err(TypeError::expected(
                        span,
                        self.infcx.normalize_ty_and_untyped(&ty).to_string(),
                        "any integer",
                    ));
                }
            }

            BinaryOp::Eq | BinaryOp::NEq => (),

            BinaryOp::And | BinaryOp::Or => {
                if !ty.is_bool() {
                    return Err(TypeError::type_mismatch(
                        span,
                        TyKind::Bool.to_string(),
                        self.infcx.normalize_ty_and_untyped(&ty).to_string(),
                    ));
                }
            }
        };

        let result_ty = match op {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::Rem
            | BinaryOp::Shl
            | BinaryOp::Shr
            | BinaryOp::BitwiseOr
            | BinaryOp::BitwiseXor
            | BinaryOp::BitwiseAnd => ty,

            BinaryOp::Eq
            | BinaryOp::NEq
            | BinaryOp::Lt
            | BinaryOp::LtEq
            | BinaryOp::Gt
            | BinaryOp::GtEq
            | BinaryOp::And
            | BinaryOp::Or => TyKind::Bool,
        };

        if lhs.value.is_some() && rhs.value.is_some() {
            let lhs = lhs.value.unwrap();
            let rhs = rhs.value.unwrap();

            let value = const_fold_binary(lhs, rhs, op, span)?;

            Ok(CheckedExpr::new(
                ExprKind::Literal(value.clone().into()),
                result_ty,
                Some(value),
                span,
            ))
        } else {
            Ok(CheckedExpr::new(
                ExprKind::Binary {
                    lhs: Box::new(lhs.expr),
                    op,
                    rhs: Box::new(rhs.expr),
                },
                result_ty,
                None,
                span,
            ))
        }
    }
}
