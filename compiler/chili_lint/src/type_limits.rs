use chili_ast::{
    ast,
    const_value::ConstValue,
    ty::{IntTy, Ty, TyKind, UintTy},
};
use chili_error::diagnostic::{Diagnostic, Label};
use chili_infer::normalize::NormalizeTy;
use chili_span::Span;
use std::fmt::Display;

use crate::sess::LintSess;

impl<'s> LintSess<'s> {
    pub fn check_type_limits(&mut self, e: &ast::Expr) {
        if let ast::ExprKind::ConstValue(const_value) = &e.kind {
            match const_value {
                ConstValue::Int(value) => match &e.ty.normalize(self.tycx) {
                    TyKind::Int(int_ty) => self.check_int_limits(int_ty, *value, e),
                    TyKind::Uint(uint_ty) => self.check_uint_limits(uint_ty, *value, e),
                    _ => (),
                },
                ConstValue::Uint(value) => match &e.ty.normalize(self.tycx) {
                    TyKind::Int(int_ty) => self.check_int_limits(int_ty, *value as _, e),
                    TyKind::Uint(uint_ty) => self.check_uint_limits(uint_ty, *value as _, e),
                    _ => (),
                },
                _ => (),
            }
        }
    }

    fn check_int_limits(&mut self, int_ty: &IntTy, value: i64, e: &ast::Expr) {
        let (min, max) = int_ty_range(*int_ty);
        if value < min || value > max {
            self.workspace
                .diagnostics
                .push(overflow_err(value, &e.ty, min, max, e.span))
        }
    }

    fn check_uint_limits(&mut self, uint_ty: &UintTy, value: i64, e: &ast::Expr) {
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

fn uint_ty_range(uint_ty: UintTy) -> (u64, u64) {
    match uint_ty {
        UintTy::U8 => (u8::MIN as u64, u8::MAX as u64),
        UintTy::U16 => (u16::MIN as u64, u16::MAX as u64),
        UintTy::U32 => (u32::MIN as u64, u32::MAX as u64),
        UintTy::U64 => (u64::MIN, u64::MAX),
        UintTy::Uint => (usize::MIN as u64, usize::MAX as u64),
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
            "integer literal of type `{}` must be between {} and {}, found {}",
            ty, min, max, value
        ))
        .with_label(Label::primary(span, "integer literal overflow"))
}
