use chili_ast::{ast, value::ConstValue};
use chili_error::{DiagnosticResult, SyntaxError};
use chili_span::Span;

pub(crate) fn const_fold_binary(
    lhs: ConstValue,
    rhs: ConstValue,
    op: ast::BinaryOp,
    span: Span,
) -> DiagnosticResult<ConstValue> {
    match (lhs, rhs) {
        (ConstValue::Bool(lhs), ConstValue::Bool(rhs)) => {
            let value = match op {
                ast::BinaryOp::Eq => ConstValue::Bool(lhs == rhs),
                ast::BinaryOp::NEq => ConstValue::Bool(lhs != rhs),
                ast::BinaryOp::And => ConstValue::Bool(lhs && rhs),
                ast::BinaryOp::Or => ConstValue::Bool(lhs || rhs),
                _ => unreachable!("got {}", op),
            };

            Ok(value)
        }
        (ConstValue::Int(lhs), ConstValue::Int(rhs)) => {
            let value = match op {
                ast::BinaryOp::Add => ConstValue::Int(lhs + rhs),
                ast::BinaryOp::Sub => ConstValue::Int(lhs - rhs),
                ast::BinaryOp::Mul => ConstValue::Int(lhs * rhs),
                ast::BinaryOp::Div => {
                    if rhs == 0 {
                        return Err(SyntaxError::divide_by_zero(span));
                    }

                    ConstValue::Int(lhs / rhs)
                }
                ast::BinaryOp::Rem => {
                    if rhs == 0 {
                        return Err(SyntaxError::divide_by_zero(span));
                    }

                    ConstValue::Int(lhs % rhs)
                }
                ast::BinaryOp::Shl => ConstValue::Int(lhs << rhs),
                ast::BinaryOp::Shr => ConstValue::Int(lhs >> rhs),
                ast::BinaryOp::BitwiseOr => ConstValue::Int(lhs | rhs),
                ast::BinaryOp::BitwiseXor => ConstValue::Int(lhs ^ rhs),
                ast::BinaryOp::BitwiseAnd => ConstValue::Int(lhs & rhs),
                ast::BinaryOp::Eq => ConstValue::Bool(lhs == rhs),
                ast::BinaryOp::NEq => ConstValue::Bool(lhs != rhs),
                ast::BinaryOp::Lt => ConstValue::Bool(lhs < rhs),
                ast::BinaryOp::LtEq => ConstValue::Bool(lhs <= rhs),
                ast::BinaryOp::Gt => ConstValue::Bool(lhs > rhs),
                ast::BinaryOp::GtEq => ConstValue::Bool(lhs >= rhs),
                _ => unreachable!("got {}", op),
            };

            Ok(value)
        }
        (ConstValue::Float(lhs), ConstValue::Float(rhs)) => {
            let value = match op {
                ast::BinaryOp::Add => ConstValue::Float(lhs + rhs),
                ast::BinaryOp::Sub => ConstValue::Float(lhs - rhs),
                ast::BinaryOp::Mul => ConstValue::Float(lhs * rhs),
                ast::BinaryOp::Div => ConstValue::Float(lhs / rhs),
                ast::BinaryOp::Rem => ConstValue::Float(lhs % rhs),
                ast::BinaryOp::Eq => ConstValue::Bool(lhs == rhs),
                ast::BinaryOp::NEq => ConstValue::Bool(lhs != rhs),
                ast::BinaryOp::Lt => ConstValue::Bool(lhs < rhs),
                ast::BinaryOp::LtEq => ConstValue::Bool(lhs <= rhs),
                ast::BinaryOp::Gt => ConstValue::Bool(lhs > rhs),
                ast::BinaryOp::GtEq => ConstValue::Bool(lhs >= rhs),
                _ => unreachable!("got {}", op),
            };

            Ok(value)
        }
        (ConstValue::Float(lhs), ConstValue::Int(rhs)) => {
            let rhs = rhs as f64;

            let value = match op {
                ast::BinaryOp::Add => ConstValue::Float(lhs + rhs),
                ast::BinaryOp::Sub => ConstValue::Float(lhs - rhs),
                ast::BinaryOp::Mul => ConstValue::Float(lhs * rhs),
                ast::BinaryOp::Div => {
                    if rhs == 0.0 {
                        return Err(SyntaxError::divide_by_zero(span));
                    }

                    ConstValue::Float(lhs / rhs)
                }
                ast::BinaryOp::Rem => {
                    if rhs == 0.0 {
                        return Err(SyntaxError::divide_by_zero(span));
                    }

                    ConstValue::Float(lhs % rhs)
                }
                ast::BinaryOp::Eq => ConstValue::Bool(lhs == rhs),
                ast::BinaryOp::NEq => ConstValue::Bool(lhs != rhs),
                ast::BinaryOp::Lt => ConstValue::Bool(lhs < rhs),
                ast::BinaryOp::LtEq => ConstValue::Bool(lhs <= rhs),
                ast::BinaryOp::Gt => ConstValue::Bool(lhs > rhs),
                ast::BinaryOp::GtEq => ConstValue::Bool(lhs >= rhs),
                _ => unreachable!("got {}", op),
            };

            Ok(value)
        }
        (ConstValue::Int(lhs), ConstValue::Float(rhs)) => {
            let lhs = lhs as f64;

            let value = match op {
                ast::BinaryOp::Add => ConstValue::Float(lhs + rhs),
                ast::BinaryOp::Sub => ConstValue::Float(lhs - rhs),
                ast::BinaryOp::Mul => ConstValue::Float(lhs * rhs),
                ast::BinaryOp::Div => {
                    if rhs == 0.0 {
                        return Err(SyntaxError::divide_by_zero(span));
                    }

                    ConstValue::Float(lhs / rhs)
                }
                ast::BinaryOp::Rem => {
                    if rhs == 0.0 {
                        return Err(SyntaxError::divide_by_zero(span));
                    }

                    ConstValue::Float(lhs % rhs)
                }
                ast::BinaryOp::Eq => ConstValue::Bool(lhs == rhs),
                ast::BinaryOp::NEq => ConstValue::Bool(lhs != rhs),
                ast::BinaryOp::Lt => ConstValue::Bool(lhs < rhs),
                ast::BinaryOp::LtEq => ConstValue::Bool(lhs <= rhs),
                ast::BinaryOp::Gt => ConstValue::Bool(lhs > rhs),
                ast::BinaryOp::GtEq => ConstValue::Bool(lhs >= rhs),
                _ => unreachable!("got {}", op),
            };

            Ok(value)
        }
        _ => unreachable!(),
    }
}
