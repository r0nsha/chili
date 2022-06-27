use super::LintSess;
use crate::{
    error::diagnostic::{Diagnostic, Label},
    hir::{self, const_value::ConstValue},
    infer::normalize::Normalize,
    span::Span,
    types::{IntType, Type, TypeId, UintType},
};
use std::fmt::Display;

impl<'s> LintSess<'s> {
    pub fn check_type_limits(&mut self, const_: &hir::Const) {
        match &const_.value {
            ConstValue::Int(value) => match &const_.ty.normalize(self.tycx) {
                Type::Int(int_ty) => self.check_int_limits(int_ty, *value, const_),
                Type::Uint(uint_ty) => self.check_uint_limits(uint_ty, *value, const_),
                _ => (),
            },
            ConstValue::Uint(value) => match &const_.ty.normalize(self.tycx) {
                Type::Int(int_ty) => self.check_int_limits(int_ty, *value as _, const_),
                Type::Uint(uint_ty) => self.check_uint_limits(uint_ty, *value as _, const_),
                _ => (),
            },
            _ => (),
        }
    }

    fn check_int_limits(&mut self, int_ty: &IntType, value: i64, const_: &hir::Const) {
        let (min, max) = int_ty_range(*int_ty);
        if value < min || value > max {
            self.workspace
                .diagnostics
                .push(overflow_err(value, &const_.ty, min, max, const_.span))
        }
    }

    fn check_uint_limits(&mut self, uint_ty: &UintType, value: i64, const_: &hir::Const) {
        let (min, max) = uint_ty_range(*uint_ty);

        if value.is_negative() {
            self.workspace
                .diagnostics
                .push(overflow_err(value, &const_.ty, min, max, const_.span))
        } else {
            let value = value as u64;

            if value < min || value > max {
                self.workspace.diagnostics.push(overflow_err(
                    value,
                    &const_.ty,
                    min,
                    max,
                    const_.span,
                ))
            }
        }
    }
}

fn int_ty_range(int_ty: IntType) -> (i64, i64) {
    match int_ty {
        IntType::I8 => (i8::MIN as i64, i8::MAX as i64),
        IntType::I16 => (i16::MIN as i64, i16::MAX as i64),
        IntType::I32 => (i32::MIN as i64, i32::MAX as i64),
        IntType::I64 => (i64::MIN, i64::MAX),
        IntType::Int => (isize::MIN as i64, isize::MAX as i64),
    }
}

fn uint_ty_range(uint_ty: UintType) -> (u64, u64) {
    match uint_ty {
        UintType::U8 => (u8::MIN as u64, u8::MAX as u64),
        UintType::U16 => (u16::MIN as u64, u16::MAX as u64),
        UintType::U32 => (u32::MIN as u64, u32::MAX as u64),
        UintType::U64 => (u64::MIN, u64::MAX),
        UintType::Uint => (usize::MIN as u64, usize::MAX as u64),
    }
}

fn overflow_err<V: Copy + Display, M: Copy + Display>(
    value: V,
    ty: &TypeId,
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
