use crate::{
    ast,
    error::{
        diagnostic::{Diagnostic, Label},
        DiagnosticResult, SyntaxError,
    },
    hir::const_value::ConstValue,
    span::Span,
};

pub fn binary(
    lhs: &ConstValue,
    rhs: &ConstValue,
    op: ast::BinaryOp,
    span: Span,
) -> DiagnosticResult<ConstValue> {
    let int_overflow = |action: &str| int_overflow(action, lhs, rhs, span);

    match op {
        ast::BinaryOp::Add => lhs.add(rhs).ok_or_else(|| int_overflow("adding")),
        ast::BinaryOp::Sub => lhs.sub(rhs).ok_or_else(|| int_overflow("subtracting")),
        ast::BinaryOp::Mul => lhs.mul(rhs).ok_or_else(|| int_overflow("multiplying")),
        ast::BinaryOp::Div => match rhs {
            ConstValue::Int(0) | ConstValue::Uint(0) => Err(SyntaxError::divide_by_zero(span)),
            _ => lhs.div(rhs).ok_or_else(|| int_overflow("dividing")),
        },
        ast::BinaryOp::Rem => match rhs {
            ConstValue::Int(0) | ConstValue::Uint(0) => Err(SyntaxError::divide_by_zero(span)),
            _ => lhs
                .rem(rhs)
                .ok_or_else(|| int_overflow("taking the remainder of")),
        },
        ast::BinaryOp::Eq => todo!(),
        ast::BinaryOp::Neq => todo!(),
        ast::BinaryOp::Lt => todo!(),
        ast::BinaryOp::LtEq => todo!(),
        ast::BinaryOp::Gt => todo!(),
        ast::BinaryOp::GtEq => todo!(),
        ast::BinaryOp::And => todo!(),
        ast::BinaryOp::Or => todo!(),
        ast::BinaryOp::Shl => todo!(),
        ast::BinaryOp::Shr => todo!(),
        ast::BinaryOp::BitwiseAnd => todo!(),
        ast::BinaryOp::BitwiseOr => todo!(),
        ast::BinaryOp::BitwiseXor => todo!(),
    }
    // match (lhs, rhs) {
    //     (ConstValue::Bool(lhs), ConstValue::Bool(rhs)) => {
    //         let value = match op {
    //             ast::BinaryOp::Eq => ConstValue::Bool(lhs == rhs),
    //             ast::BinaryOp::Neq => ConstValue::Bool(lhs != rhs),
    //             ast::BinaryOp::And => ConstValue::Bool(lhs && rhs),
    //             ast::BinaryOp::Or => ConstValue::Bool(lhs || rhs),
    //             _ => unreachable!("got {}", op),
    //         };

    //         Ok(value)
    //     }
    //     (ConstValue::Int(lhs), ConstValue::Int(rhs)) => {
    //         let value = match op {
    //             ast::BinaryOp::Add => ConstValue::Int(lhs + rhs),
    //             ast::BinaryOp::Sub => ConstValue::Int(lhs - rhs),
    //             ast::BinaryOp::Mul => ConstValue::Int(lhs * rhs),
    //             ast::BinaryOp::Div => {
    //                 if rhs == 0 {
    //                     return Err(SyntaxError::divide_by_zero(span));
    //                 }

    //                 ConstValue::Int(lhs / rhs)
    //             }
    //             ast::BinaryOp::Rem => {
    //                 if rhs == 0 {
    //                     return Err(SyntaxError::divide_by_zero(span));
    //                 }

    //                 ConstValue::Int(lhs % rhs)
    //             }
    //             ast::BinaryOp::Shl => ConstValue::Int(lhs << rhs),
    //             ast::BinaryOp::Shr => ConstValue::Int(lhs >> rhs),
    //             ast::BinaryOp::BitwiseOr => ConstValue::Int(lhs | rhs),
    //             ast::BinaryOp::BitwiseXor => ConstValue::Int(lhs ^ rhs),
    //             ast::BinaryOp::BitwiseAnd => ConstValue::Int(lhs & rhs),
    //             ast::BinaryOp::Eq => ConstValue::Bool(lhs == rhs),
    //             ast::BinaryOp::Neq => ConstValue::Bool(lhs != rhs),
    //             ast::BinaryOp::Lt => ConstValue::Bool(lhs < rhs),
    //             ast::BinaryOp::LtEq => ConstValue::Bool(lhs <= rhs),
    //             ast::BinaryOp::Gt => ConstValue::Bool(lhs > rhs),
    //             ast::BinaryOp::GtEq => ConstValue::Bool(lhs >= rhs),
    //             _ => unreachable!("got {}", op),
    //         };

    //         Ok(value)
    //     }
    //     (ConstValue::Uint(lhs), ConstValue::Uint(rhs)) => {
    //         let value = match op {
    //             ast::BinaryOp::Add => ConstValue::Uint(lhs + rhs),
    //             ast::BinaryOp::Sub => ConstValue::Uint(lhs - rhs),
    //             ast::BinaryOp::Mul => ConstValue::Uint(lhs * rhs),
    //             ast::BinaryOp::Div => {
    //                 if rhs == 0 {
    //                     return Err(SyntaxError::divide_by_zero(span));
    //                 }

    //                 ConstValue::Uint(lhs / rhs)
    //             }
    //             ast::BinaryOp::Rem => {
    //                 if rhs == 0 {
    //                     return Err(SyntaxError::divide_by_zero(span));
    //                 }

    //                 ConstValue::Uint(lhs % rhs)
    //             }
    //             ast::BinaryOp::Shl => ConstValue::Uint(lhs << rhs),
    //             ast::BinaryOp::Shr => ConstValue::Uint(lhs >> rhs),
    //             ast::BinaryOp::BitwiseOr => ConstValue::Uint(lhs | rhs),
    //             ast::BinaryOp::BitwiseXor => ConstValue::Uint(lhs ^ rhs),
    //             ast::BinaryOp::BitwiseAnd => ConstValue::Uint(lhs & rhs),
    //             ast::BinaryOp::Eq => ConstValue::Bool(lhs == rhs),
    //             ast::BinaryOp::Neq => ConstValue::Bool(lhs != rhs),
    //             ast::BinaryOp::Lt => ConstValue::Bool(lhs < rhs),
    //             ast::BinaryOp::LtEq => ConstValue::Bool(lhs <= rhs),
    //             ast::BinaryOp::Gt => ConstValue::Bool(lhs > rhs),
    //             ast::BinaryOp::GtEq => ConstValue::Bool(lhs >= rhs),
    //             _ => unreachable!("got {}", op),
    //         };

    //         Ok(value)
    //     }
    //     (ConstValue::Uint(lhs), ConstValue::Int(rhs)) => {
    //         let lhs = lhs as i64;

    //         let value = match op {
    //             ast::BinaryOp::Add => ConstValue::Int(lhs + rhs),
    //             ast::BinaryOp::Sub => ConstValue::Int(lhs - rhs),
    //             ast::BinaryOp::Mul => ConstValue::Int(lhs * rhs),
    //             ast::BinaryOp::Div => {
    //                 if rhs == 0 {
    //                     return Err(SyntaxError::divide_by_zero(span));
    //                 }

    //                 ConstValue::Int(lhs / rhs)
    //             }
    //             ast::BinaryOp::Rem => {
    //                 if rhs == 0 {
    //                     return Err(SyntaxError::divide_by_zero(span));
    //                 }

    //                 ConstValue::Int(lhs % rhs)
    //             }
    //             ast::BinaryOp::Shl => ConstValue::Int(lhs << rhs),
    //             ast::BinaryOp::Shr => ConstValue::Int(lhs >> rhs),
    //             ast::BinaryOp::BitwiseOr => ConstValue::Int(lhs | rhs),
    //             ast::BinaryOp::BitwiseXor => ConstValue::Int(lhs ^ rhs),
    //             ast::BinaryOp::BitwiseAnd => ConstValue::Int(lhs & rhs),
    //             ast::BinaryOp::Eq => ConstValue::Bool(lhs == rhs),
    //             ast::BinaryOp::Neq => ConstValue::Bool(lhs != rhs),
    //             ast::BinaryOp::Lt => ConstValue::Bool(lhs < rhs),
    //             ast::BinaryOp::LtEq => ConstValue::Bool(lhs <= rhs),
    //             ast::BinaryOp::Gt => ConstValue::Bool(lhs > rhs),
    //             ast::BinaryOp::GtEq => ConstValue::Bool(lhs >= rhs),
    //             _ => unreachable!("got {}", op),
    //         };

    //         Ok(value)
    //     }
    //     (ConstValue::Int(lhs), ConstValue::Uint(rhs)) => {
    //         let rhs = rhs as i64;

    //         let value = match op {
    //             ast::BinaryOp::Add => ConstValue::Int(lhs + rhs),
    //             ast::BinaryOp::Sub => ConstValue::Int(lhs - rhs),
    //             ast::BinaryOp::Mul => ConstValue::Int(lhs * rhs),
    //             ast::BinaryOp::Div => {
    //                 if rhs == 0 {
    //                     return Err(SyntaxError::divide_by_zero(span));
    //                 }

    //                 ConstValue::Int(lhs / rhs)
    //             }
    //             ast::BinaryOp::Rem => {
    //                 if rhs == 0 {
    //                     return Err(SyntaxError::divide_by_zero(span));
    //                 }

    //                 ConstValue::Int(lhs % rhs)
    //             }
    //             ast::BinaryOp::Shl => ConstValue::Int(lhs << rhs),
    //             ast::BinaryOp::Shr => ConstValue::Int(lhs >> rhs),
    //             ast::BinaryOp::BitwiseOr => ConstValue::Int(lhs | rhs),
    //             ast::BinaryOp::BitwiseXor => ConstValue::Int(lhs ^ rhs),
    //             ast::BinaryOp::BitwiseAnd => ConstValue::Int(lhs & rhs),
    //             ast::BinaryOp::Eq => ConstValue::Bool(lhs == rhs),
    //             ast::BinaryOp::Neq => ConstValue::Bool(lhs != rhs),
    //             ast::BinaryOp::Lt => ConstValue::Bool(lhs < rhs),
    //             ast::BinaryOp::LtEq => ConstValue::Bool(lhs <= rhs),
    //             ast::BinaryOp::Gt => ConstValue::Bool(lhs > rhs),
    //             ast::BinaryOp::GtEq => ConstValue::Bool(lhs >= rhs),
    //             _ => unreachable!("got {}", op),
    //         };

    //         Ok(value)
    //     }
    //     (ConstValue::Float(lhs), ConstValue::Float(rhs)) => {
    //         let value = match op {
    //             ast::BinaryOp::Add => ConstValue::Float(lhs + rhs),
    //             ast::BinaryOp::Sub => ConstValue::Float(lhs - rhs),
    //             ast::BinaryOp::Mul => ConstValue::Float(lhs * rhs),
    //             ast::BinaryOp::Div => ConstValue::Float(lhs / rhs),
    //             ast::BinaryOp::Rem => ConstValue::Float(lhs % rhs),
    //             ast::BinaryOp::Eq => ConstValue::Bool(lhs == rhs),
    //             ast::BinaryOp::Neq => ConstValue::Bool(lhs != rhs),
    //             ast::BinaryOp::Lt => ConstValue::Bool(lhs < rhs),
    //             ast::BinaryOp::LtEq => ConstValue::Bool(lhs <= rhs),
    //             ast::BinaryOp::Gt => ConstValue::Bool(lhs > rhs),
    //             ast::BinaryOp::GtEq => ConstValue::Bool(lhs >= rhs),
    //             _ => unreachable!("got {}", op),
    //         };

    //         Ok(value)
    //     }
    //     (ConstValue::Float(lhs), ConstValue::Int(rhs)) => {
    //         let rhs = rhs as f64;

    //         let value = match op {
    //             ast::BinaryOp::Add => ConstValue::Float(lhs + rhs),
    //             ast::BinaryOp::Sub => ConstValue::Float(lhs - rhs),
    //             ast::BinaryOp::Mul => ConstValue::Float(lhs * rhs),
    //             ast::BinaryOp::Div => ConstValue::Float(lhs / rhs),
    //             ast::BinaryOp::Rem => ConstValue::Float(lhs % rhs),
    //             ast::BinaryOp::Eq => ConstValue::Bool(lhs == rhs),
    //             ast::BinaryOp::Neq => ConstValue::Bool(lhs != rhs),
    //             ast::BinaryOp::Lt => ConstValue::Bool(lhs < rhs),
    //             ast::BinaryOp::LtEq => ConstValue::Bool(lhs <= rhs),
    //             ast::BinaryOp::Gt => ConstValue::Bool(lhs > rhs),
    //             ast::BinaryOp::GtEq => ConstValue::Bool(lhs >= rhs),
    //             _ => unreachable!("got {}", op),
    //         };

    //         Ok(value)
    //     }
    //     (ConstValue::Int(lhs), ConstValue::Float(rhs)) => {
    //         let lhs = lhs as f64;

    //         let value = match op {
    //             ast::BinaryOp::Add => ConstValue::Float(lhs + rhs),
    //             ast::BinaryOp::Sub => ConstValue::Float(lhs - rhs),
    //             ast::BinaryOp::Mul => ConstValue::Float(lhs * rhs),
    //             ast::BinaryOp::Div => ConstValue::Float(lhs / rhs),
    //             ast::BinaryOp::Rem => ConstValue::Float(lhs % rhs),
    //             ast::BinaryOp::Eq => ConstValue::Bool(lhs == rhs),
    //             ast::BinaryOp::Neq => ConstValue::Bool(lhs != rhs),
    //             ast::BinaryOp::Lt => ConstValue::Bool(lhs < rhs),
    //             ast::BinaryOp::LtEq => ConstValue::Bool(lhs <= rhs),
    //             ast::BinaryOp::Gt => ConstValue::Bool(lhs > rhs),
    //             ast::BinaryOp::GtEq => ConstValue::Bool(lhs >= rhs),
    //             _ => unreachable!("got {}", op),
    //         };

    //         Ok(value)
    //     }
    //     (ConstValue::Float(lhs), ConstValue::Uint(rhs)) => {
    //         let rhs = rhs as f64;

    //         let value = match op {
    //             ast::BinaryOp::Add => ConstValue::Float(lhs + rhs),
    //             ast::BinaryOp::Sub => ConstValue::Float(lhs - rhs),
    //             ast::BinaryOp::Mul => ConstValue::Float(lhs * rhs),
    //             ast::BinaryOp::Div => ConstValue::Float(lhs / rhs),
    //             ast::BinaryOp::Rem => ConstValue::Float(lhs % rhs),
    //             ast::BinaryOp::Eq => ConstValue::Bool(lhs == rhs),
    //             ast::BinaryOp::Neq => ConstValue::Bool(lhs != rhs),
    //             ast::BinaryOp::Lt => ConstValue::Bool(lhs < rhs),
    //             ast::BinaryOp::LtEq => ConstValue::Bool(lhs <= rhs),
    //             ast::BinaryOp::Gt => ConstValue::Bool(lhs > rhs),
    //             ast::BinaryOp::GtEq => ConstValue::Bool(lhs >= rhs),
    //             _ => unreachable!("got {}", op),
    //         };

    //         Ok(value)
    //     }
    //     (ConstValue::Uint(lhs), ConstValue::Float(rhs)) => {
    //         let lhs = lhs as f64;

    //         let value = match op {
    //             ast::BinaryOp::Add => ConstValue::Float(lhs + rhs),
    //             ast::BinaryOp::Sub => ConstValue::Float(lhs - rhs),
    //             ast::BinaryOp::Mul => ConstValue::Float(lhs * rhs),
    //             ast::BinaryOp::Div => ConstValue::Float(lhs / rhs),
    //             ast::BinaryOp::Rem => ConstValue::Float(lhs % rhs),
    //             ast::BinaryOp::Eq => ConstValue::Bool(lhs == rhs),
    //             ast::BinaryOp::Neq => ConstValue::Bool(lhs != rhs),
    //             ast::BinaryOp::Lt => ConstValue::Bool(lhs < rhs),
    //             ast::BinaryOp::LtEq => ConstValue::Bool(lhs <= rhs),
    //             ast::BinaryOp::Gt => ConstValue::Bool(lhs > rhs),
    //             ast::BinaryOp::GtEq => ConstValue::Bool(lhs >= rhs),
    //             _ => unreachable!("got {}", op),
    //         };

    //         Ok(value)
    //     }
    //     _ => unreachable!(),
    // }
}

fn int_overflow(action: &str, lhs: &ConstValue, rhs: &ConstValue, span: Span) -> Diagnostic {
    Diagnostic::error()
        .with_message(format!(
            "integer overflowed while {} {} and {} at compile-time",
            action, lhs, rhs
        ))
        .with_label(Label::primary(span, "integer overflow"))
}
