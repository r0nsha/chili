use crate::{CheckFrame, CheckSess};
use chili_ast::ast::{BinaryOp, Expr};
use chili_ast::ty::*;
use chili_error::{DiagnosticResult, TypeError};
use chili_span::Span;

impl<'w, 'a> CheckSess<'w, 'a> {
    #[inline]
    pub(super) fn check_binary_expr(
        &mut self,
        frame: &mut CheckFrame,
        lhs: &Box<Expr>,
        op: BinaryOp,
        rhs: &Box<Expr>,
        expected_ty: Option<Ty>,
        span: Span,
    ) -> DiagnosticResult<Ty> {
        lhs.ty = self.check_expr(frame, lhs, expected_ty.clone())?;
        rhs.ty = self.check_expr(frame, rhs, expected_ty)?;

        let ty = self
            .infcx
            .unify_or_coerce_expr_expr(lhs.as_mut(), rhs.as_mut(), rhs.span)?;

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
                        Ty::Bool.to_string(),
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
            | BinaryOp::Or => Ty::Bool,
        };

        Ok(result_ty)
    }
}
