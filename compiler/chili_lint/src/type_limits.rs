use chili_ast::{
    ast,
    ty::{IntTy, Ty, TyKind, UIntTy},
};
use chili_error::diagnostic::{Diagnostic, Label};
use chili_infer::normalize::NormalizeTy;
use chili_span::Span;
use std::fmt::Display;

use crate::sess::LintSess;

impl<'s> LintSess<'s> {
    pub fn check_type_limits(&mut self, e: &ast::Expr) {
        match &e.kind {
            ast::ExprKind::Literal(lit) => match &lit.kind {
                &ast::LiteralKind::Int(value) => match &e.ty.normalize(self.tycx) {
                    TyKind::Int(int_ty) => {
                        let (min, max) = int_ty_range(*int_ty);

                        if value < min || value > max {
                            self.workspace
                                .diagnostics
                                .push(overflow_err(value, &e.ty, min, max, e.span))
                        }
                    }
                    TyKind::UInt(uint_ty) => {
                        let (min, max) = uint_ty_range(*uint_ty);

                        if value.is_negative() {
                            self.workspace
                                .diagnostics
                                .push(overflow_err(value, &e.ty, min, max, e.span))
                        } else {
                            let value = value as u64;

                            if value < min || value > max {
                                self.workspace
                                    .diagnostics
                                    .push(overflow_err(value, &e.ty, min, max, e.span))
                            }
                        }
                    }
                    _ => (),
                },
                ast::LiteralKind::Float(_)
                | ast::LiteralKind::Unit
                | ast::LiteralKind::Nil
                | ast::LiteralKind::Bool(_)
                | ast::LiteralKind::Str(_)
                | ast::LiteralKind::Char(_) => (),
            },
            _ => (),
        }
    }
}

fn int_ty_range(int_ty: IntTy) -> (i64, i64) {
    match int_ty {
        IntTy::I8 => (i8::MIN as i64, i8::MAX as i64),
        IntTy::I16 => (i16::MIN as i64, i16::MAX as i64),
        IntTy::I32 => (i32::MIN as i64, i32::MAX as i64),
        IntTy::I64 => (i64::MIN, i64::MAX),
        IntTy::Int => (isize::MIN as i64, isize::MAX as i64),
    }
}

fn uint_ty_range(uint_ty: UIntTy) -> (u64, u64) {
    match uint_ty {
        UIntTy::U8 => (u8::MIN as u64, u8::MAX as u64),
        UIntTy::U16 => (u16::MIN as u64, u16::MAX as u64),
        UIntTy::U32 => (u32::MIN as u64, u32::MAX as u64),
        UIntTy::U64 => (u64::MIN, u64::MAX),
        UIntTy::UInt => (usize::MIN as u64, usize::MAX as u64),
    }
}

fn overflow_err<V: Copy + Display, M: Copy + Display>(
    value: V,
    ty: &Ty,
    min: M,
    max: M,
    span: Span,
) -> Diagnostic {
    Diagnostic::error()
        .with_message(format!(
            "integer literal of type `{}` must be between {} and {}, but found {}",
            ty, min, max, value
        ))
        .with_label(Label::primary(span, "integer literal overflow"))
}
