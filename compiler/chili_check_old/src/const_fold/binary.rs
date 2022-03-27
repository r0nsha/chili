use chili_ast::{ast, value::Value};
use chili_error::{DiagnosticResult, SyntaxError};
use chili_span::Span;

pub(crate) fn const_fold_binary(
    lhs: Value,
    rhs: Value,
    op: ast::BinaryOp,
    span: Span,
) -> DiagnosticResult<Value> {
    match (lhs, rhs) {
        (Value::Bool(lhs), Value::Bool(rhs)) => {
            let value = match op {
                ast::BinaryOp::Eq => Value::Bool(lhs == rhs),
                ast::BinaryOp::NEq => Value::Bool(lhs != rhs),
                ast::BinaryOp::And => Value::Bool(lhs && rhs),
                ast::BinaryOp::Or => Value::Bool(lhs || rhs),
                _ => unreachable!("got {}", op),
            };

            Ok(value)
        }
        (Value::Int(lhs), Value::Int(rhs)) => {
            let value = match op {
                ast::BinaryOp::Add => Value::Int(lhs + rhs),
                ast::BinaryOp::Sub => Value::Int(lhs - rhs),
                ast::BinaryOp::Mul => Value::Int(lhs * rhs),
                ast::BinaryOp::Div => {
                    if rhs == 0 {
                        return Err(SyntaxError::divide_by_zero(span));
                    }

                    Value::Int(lhs / rhs)
                }
                ast::BinaryOp::Rem => {
                    if rhs == 0 {
                        return Err(SyntaxError::divide_by_zero(span));
                    }

                    Value::Int(lhs % rhs)
                }
                ast::BinaryOp::Shl => Value::Int(lhs << rhs),
                ast::BinaryOp::Shr => Value::Int(lhs >> rhs),
                ast::BinaryOp::BitwiseOr => Value::Int(lhs | rhs),
                ast::BinaryOp::BitwiseXor => Value::Int(lhs ^ rhs),
                ast::BinaryOp::BitwiseAnd => Value::Int(lhs & rhs),
                ast::BinaryOp::Eq => Value::Bool(lhs == rhs),
                ast::BinaryOp::NEq => Value::Bool(lhs != rhs),
                ast::BinaryOp::Lt => Value::Bool(lhs < rhs),
                ast::BinaryOp::LtEq => Value::Bool(lhs <= rhs),
                ast::BinaryOp::Gt => Value::Bool(lhs > rhs),
                ast::BinaryOp::GtEq => Value::Bool(lhs >= rhs),
                _ => unreachable!("got {}", op),
            };

            Ok(value)
        }
        (Value::Float(lhs), Value::Float(rhs)) => {
            let value = match op {
                ast::BinaryOp::Add => Value::Float(lhs + rhs),
                ast::BinaryOp::Sub => Value::Float(lhs - rhs),
                ast::BinaryOp::Mul => Value::Float(lhs * rhs),
                ast::BinaryOp::Div => Value::Float(lhs / rhs),
                ast::BinaryOp::Rem => Value::Float(lhs % rhs),
                ast::BinaryOp::Eq => Value::Bool(lhs == rhs),
                ast::BinaryOp::NEq => Value::Bool(lhs != rhs),
                ast::BinaryOp::Lt => Value::Bool(lhs < rhs),
                ast::BinaryOp::LtEq => Value::Bool(lhs <= rhs),
                ast::BinaryOp::Gt => Value::Bool(lhs > rhs),
                ast::BinaryOp::GtEq => Value::Bool(lhs >= rhs),
                _ => unreachable!("got {}", op),
            };

            Ok(value)
        }
        (Value::Float(lhs), Value::Int(rhs)) => {
            let rhs = rhs as f64;

            let value = match op {
                ast::BinaryOp::Add => Value::Float(lhs + rhs),
                ast::BinaryOp::Sub => Value::Float(lhs - rhs),
                ast::BinaryOp::Mul => Value::Float(lhs * rhs),
                ast::BinaryOp::Div => {
                    if rhs == 0.0 {
                        return Err(SyntaxError::divide_by_zero(span));
                    }

                    Value::Float(lhs / rhs)
                }
                ast::BinaryOp::Rem => {
                    if rhs == 0.0 {
                        return Err(SyntaxError::divide_by_zero(span));
                    }

                    Value::Float(lhs % rhs)
                }
                ast::BinaryOp::Eq => Value::Bool(lhs == rhs),
                ast::BinaryOp::NEq => Value::Bool(lhs != rhs),
                ast::BinaryOp::Lt => Value::Bool(lhs < rhs),
                ast::BinaryOp::LtEq => Value::Bool(lhs <= rhs),
                ast::BinaryOp::Gt => Value::Bool(lhs > rhs),
                ast::BinaryOp::GtEq => Value::Bool(lhs >= rhs),
                _ => unreachable!("got {}", op),
            };

            Ok(value)
        }
        (Value::Int(lhs), Value::Float(rhs)) => {
            let lhs = lhs as f64;

            let value = match op {
                ast::BinaryOp::Add => Value::Float(lhs + rhs),
                ast::BinaryOp::Sub => Value::Float(lhs - rhs),
                ast::BinaryOp::Mul => Value::Float(lhs * rhs),
                ast::BinaryOp::Div => {
                    if rhs == 0.0 {
                        return Err(SyntaxError::divide_by_zero(span));
                    }

                    Value::Float(lhs / rhs)
                }
                ast::BinaryOp::Rem => {
                    if rhs == 0.0 {
                        return Err(SyntaxError::divide_by_zero(span));
                    }

                    Value::Float(lhs % rhs)
                }
                ast::BinaryOp::Eq => Value::Bool(lhs == rhs),
                ast::BinaryOp::NEq => Value::Bool(lhs != rhs),
                ast::BinaryOp::Lt => Value::Bool(lhs < rhs),
                ast::BinaryOp::LtEq => Value::Bool(lhs <= rhs),
                ast::BinaryOp::Gt => Value::Bool(lhs > rhs),
                ast::BinaryOp::GtEq => Value::Bool(lhs >= rhs),
                _ => unreachable!("got {}", op),
            };

            Ok(value)
        }
        _ => unreachable!(),
    }
}
