use chilic_ast::ast::{Expr, ExprKind, LiteralKind};
use chilic_error::DiagnosticResult;
use chilic_span::Span;
use chilic_ty::{IntTy, Ty, UIntTy};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::fmt::Display;

pub fn check_type_limits(e: &Expr) -> DiagnosticResult<()> {
    match &e.kind {
        ExprKind::Literal(k) => match k {
            &LiteralKind::Int(value) => match &e.ty {
                Ty::Int(int_ty) => {
                    let (min, max) = int_ty_range(*int_ty);

                    if value < min || value > max {
                        Err(overflow_err(value, &e.ty, min, max, e.span))
                    } else {
                        Ok(())
                    }
                }
                Ty::UInt(uint_ty) => {
                    let (min, max) = uint_ty_range(*uint_ty);

                    if value.is_negative() {
                        Err(overflow_err(value, &e.ty, min, max, e.span))
                    } else {
                        let value = value as u64;

                        if value < min || value > max {
                            Err(overflow_err(value, &e.ty, min, max, e.span))
                        } else {
                            Ok(())
                        }
                    }
                }
                _ => Ok(()),
            },
            LiteralKind::Float(_)
            | LiteralKind::Unit
            | LiteralKind::Nil
            | LiteralKind::Bool(_)
            | LiteralKind::Str(_)
            | LiteralKind::Char(_) => Ok(()),
        },
        _ => Ok(()),
    }
}

fn int_ty_range(int_ty: IntTy) -> (i64, i64) {
    match int_ty {
        IntTy::I8 => (i8::MIN as i64, i8::MAX as i64),
        IntTy::I16 => (i16::MIN as i64, i16::MAX as i64),
        IntTy::I32 => (i32::MIN as i64, i32::MAX as i64),
        IntTy::I64 => (i64::MIN, i64::MAX),
        IntTy::Isize => (isize::MIN as i64, isize::MAX as i64),
    }
}

fn uint_ty_range(uint_ty: UIntTy) -> (u64, u64) {
    match uint_ty {
        UIntTy::U8 => (u8::MIN as u64, u8::MAX as u64),
        UIntTy::U16 => (u16::MIN as u64, u16::MAX as u64),
        UIntTy::U32 => (u32::MIN as u64, u32::MAX as u64),
        UIntTy::U64 => (u64::MIN, u64::MAX),
        UIntTy::Usize => (usize::MIN as u64, usize::MAX as u64),
    }
}

fn overflow_err<V: Copy + Display, M: Copy + Display>(
    value: V,
    ty: &Ty,
    min: M,
    max: M,
    span: Span,
) -> Diagnostic<usize> {
    Diagnostic::error()
        .with_message(format!(
            "integer literal of type `{}` must be between {} and {}, but found {}",
            ty, min, max,value
        ))
        .with_labels(vec![Label::primary(span.file_id, span.range().clone())
            .with_message("integer literal overflow")])
}
