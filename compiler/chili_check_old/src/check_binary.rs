use crate::{AnalysisContext, AnalysisFrame, CheckedExpr};
use chili_ast::{
    ast::{BinaryOp, Expr, ExprKind, LiteralKind},
    value::Value,
};
use chili_error::{DiagnosticResult, SyntaxError, TypeError};
use chili_span::Span;
use chili_ty::*;

impl<'a> AnalysisContext<'a> {
    #[inline]
    pub(super) fn check_binary_expr(
        &mut self,
        frame: &mut AnalysisFrame,
        lhs: &Box<Expr>,
        op: BinaryOp,
        rhs: &Box<Expr>,
        expected_ty: Option<Ty>,
        span: Span,
    ) -> DiagnosticResult<CheckedExpr> {
        let mut lhs = self.check_expr(frame, lhs, expected_ty.clone())?;
        let mut rhs = self.check_expr(frame, rhs, expected_ty)?;

        let rhs_span = rhs.expr.span;
        let ty = self.infcx.unify_or_coerce_expr_expr(
            &mut lhs.expr,
            &mut rhs.expr,
            rhs_span,
        )?;

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
                        &self.infcx.normalize_ty_and_untyped(&ty),
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
                        &self.infcx.normalize_ty_and_untyped(&ty),
                        "any integer",
                    ));
                }
            }

            BinaryOp::Eq | BinaryOp::NEq => (),

            BinaryOp::And | BinaryOp::Or => {
                if !ty.is_bool() {
                    return Err(TypeError::type_mismatch(
                        span,
                        &Ty::Bool,
                        &self.infcx.normalize_ty_and_untyped(&ty),
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

        if lhs.value.is_some() && rhs.value.is_some() {
            let lhs = lhs.value.unwrap();
            let rhs = rhs.value.unwrap();

            // TODO: the logic here is a really bad copypasta. i HAVE to make
            // this generic somehow
            match (lhs, rhs) {
                (Value::Bool(lhs), Value::Bool(rhs)) => {
                    let value = match op {
                        BinaryOp::Eq => lhs == rhs,
                        BinaryOp::NEq => lhs != rhs,
                        BinaryOp::And => lhs && rhs,
                        BinaryOp::Or => lhs || rhs,
                        _ => unreachable!("got {}", op),
                    };

                    Ok(CheckedExpr::new(
                        ExprKind::Literal(LiteralKind::Bool(value)),
                        Ty::Bool,
                        Some(Value::Bool(value)),
                        span,
                    ))
                }
                (Value::Int(lhs), Value::Int(rhs)) => {
                    let int_result = |value| {
                        (
                            ExprKind::Literal(LiteralKind::Int(value)),
                            result_ty,
                            Some(Value::Int(value)),
                        )
                    };

                    let bool_result = |value| {
                        (
                            ExprKind::Literal(LiteralKind::Bool(value)),
                            Ty::Bool,
                            Some(Value::Bool(value)),
                        )
                    };

                    let (expr, ty, value) = match op {
                        BinaryOp::Add => int_result(lhs + rhs),
                        BinaryOp::Sub => int_result(lhs - rhs),
                        BinaryOp::Mul => int_result(lhs * rhs),
                        BinaryOp::Div => {
                            if rhs == 0 {
                                return Err(SyntaxError::divide_by_zero(span));
                            }

                            int_result(lhs / rhs)
                        }
                        BinaryOp::Rem => {
                            if rhs == 0 {
                                return Err(SyntaxError::divide_by_zero(span));
                            }

                            int_result(lhs % rhs)
                        }
                        BinaryOp::Shl => int_result(lhs << rhs),
                        BinaryOp::Shr => int_result(lhs >> rhs),
                        BinaryOp::BitwiseOr => int_result(lhs | rhs),
                        BinaryOp::BitwiseXor => int_result(lhs ^ rhs),
                        BinaryOp::BitwiseAnd => int_result(lhs & rhs),
                        BinaryOp::Eq => bool_result(lhs == rhs),
                        BinaryOp::NEq => bool_result(lhs != rhs),
                        BinaryOp::Lt => bool_result(lhs < rhs),
                        BinaryOp::LtEq => bool_result(lhs <= rhs),
                        BinaryOp::Gt => bool_result(lhs > rhs),
                        BinaryOp::GtEq => bool_result(lhs >= rhs),
                        _ => unreachable!("got {}", op),
                    };

                    Ok(CheckedExpr::new(expr, ty, value, span))
                }
                (Value::Float(lhs), Value::Float(rhs)) => {
                    let float_result = |value| {
                        (
                            ExprKind::Literal(LiteralKind::Float(value)),
                            result_ty,
                            Some(Value::Float(value)),
                        )
                    };

                    let bool_result = |value| {
                        (
                            ExprKind::Literal(LiteralKind::Bool(value)),
                            Ty::Bool,
                            Some(Value::Bool(value)),
                        )
                    };

                    let (expr, ty, value) = match op {
                        BinaryOp::Add => float_result(lhs + rhs),
                        BinaryOp::Sub => float_result(lhs - rhs),
                        BinaryOp::Mul => float_result(lhs * rhs),
                        BinaryOp::Div => float_result(lhs / rhs),
                        BinaryOp::Rem => float_result(lhs % rhs),
                        BinaryOp::Eq => bool_result(lhs == rhs),
                        BinaryOp::NEq => bool_result(lhs != rhs),
                        BinaryOp::Lt => bool_result(lhs < rhs),
                        BinaryOp::LtEq => bool_result(lhs <= rhs),
                        BinaryOp::Gt => bool_result(lhs > rhs),
                        BinaryOp::GtEq => bool_result(lhs >= rhs),
                        _ => unreachable!("got {}", op),
                    };

                    Ok(CheckedExpr::new(expr, ty, value, span))
                }
                (Value::Float(lhs), Value::Int(rhs)) => {
                    let float_result = |value| {
                        (
                            ExprKind::Literal(LiteralKind::Float(value)),
                            result_ty,
                            Some(Value::Float(value)),
                        )
                    };

                    let bool_result = |value| {
                        (
                            ExprKind::Literal(LiteralKind::Bool(value)),
                            Ty::Bool,
                            Some(Value::Bool(value)),
                        )
                    };

                    let rhs = rhs as f64;

                    let (expr, ty, value) = match op {
                        BinaryOp::Add => float_result(lhs + rhs),
                        BinaryOp::Sub => float_result(lhs - rhs),
                        BinaryOp::Mul => float_result(lhs * rhs),
                        BinaryOp::Div => {
                            if rhs == 0.0 {
                                return Err(SyntaxError::divide_by_zero(span));
                            }

                            float_result(lhs / rhs)
                        }
                        BinaryOp::Rem => {
                            if rhs == 0.0 {
                                return Err(SyntaxError::divide_by_zero(span));
                            }

                            float_result(lhs % rhs)
                        }
                        BinaryOp::Eq => bool_result(lhs == rhs),
                        BinaryOp::NEq => bool_result(lhs != rhs),
                        BinaryOp::Lt => bool_result(lhs < rhs),
                        BinaryOp::LtEq => bool_result(lhs <= rhs),
                        BinaryOp::Gt => bool_result(lhs > rhs),
                        BinaryOp::GtEq => bool_result(lhs >= rhs),
                        _ => unreachable!("got {}", op),
                    };

                    Ok(CheckedExpr::new(expr, ty, value, span))
                }
                (Value::Int(lhs), Value::Float(rhs)) => {
                    let float_result = |value| {
                        (
                            ExprKind::Literal(LiteralKind::Float(value)),
                            result_ty,
                            Some(Value::Float(value)),
                        )
                    };

                    let bool_result = |value| {
                        (
                            ExprKind::Literal(LiteralKind::Bool(value)),
                            Ty::Bool,
                            Some(Value::Bool(value)),
                        )
                    };

                    let lhs = lhs as f64;

                    let (expr, ty, value) = match op {
                        BinaryOp::Add => float_result(lhs + rhs),
                        BinaryOp::Sub => float_result(lhs - rhs),
                        BinaryOp::Mul => float_result(lhs * rhs),
                        BinaryOp::Div => {
                            if rhs == 0.0 {
                                return Err(SyntaxError::divide_by_zero(span));
                            }

                            float_result(lhs / rhs)
                        }
                        BinaryOp::Rem => {
                            if rhs == 0.0 {
                                return Err(SyntaxError::divide_by_zero(span));
                            }

                            float_result(lhs % rhs)
                        }
                        BinaryOp::Eq => bool_result(lhs == rhs),
                        BinaryOp::NEq => bool_result(lhs != rhs),
                        BinaryOp::Lt => bool_result(lhs < rhs),
                        BinaryOp::LtEq => bool_result(lhs <= rhs),
                        BinaryOp::Gt => bool_result(lhs > rhs),
                        BinaryOp::GtEq => bool_result(lhs >= rhs),
                        _ => unreachable!("got {}", op),
                    };

                    Ok(CheckedExpr::new(expr, ty, value, span))
                }
                _ => unreachable!(),
            }
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
