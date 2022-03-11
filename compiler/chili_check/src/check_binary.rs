use crate::const_fold::binary::const_fold_binary;
use crate::{CheckFrame, CheckResult, CheckSess};
use chili_ast::ast::{BinaryOp, Expr};
use chili_ast::ty::*;
use chili_error::{DiagnosticResult, TypeError};
use chili_span::Span;

impl<'w, 'a> CheckSess<'w, 'a> {
    #[inline]
    pub(super) fn check_binary_expr(
        &mut self,
        frame: &mut CheckFrame,
        lhs: &mut Expr,
        op: BinaryOp,
        rhs: &mut Expr,
        expected_ty: Option<TyKind>,
        span: Span,
    ) -> DiagnosticResult<CheckResult> {
        let lhs_result = self.check_expr(frame, lhs, expected_ty.clone())?;
        let rhs_result = self.check_expr(frame, rhs, expected_ty.clone())?;

        let ty = self.infcx.unify_or_coerce_expr_expr(lhs, rhs, rhs.span)?;

        lhs.ty = ty.clone();
        rhs.ty = ty.clone();

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

        if lhs_result.value.is_some() && rhs_result.value.is_some() {
            let value = const_fold_binary(
                lhs_result.value.unwrap(),
                rhs_result.value.unwrap(),
                op,
                span,
            )?;
            Ok(CheckResult::new(result_ty, Some(value)))
        } else {
            Ok(CheckResult::new(result_ty, None))
        }
    }
}
