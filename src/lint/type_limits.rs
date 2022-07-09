use super::LintSess;
use crate::{
    error::diagnostic::{Diagnostic, Label},
    hir::{self, const_value::ConstValue},
    infer::{display::DisplayTy, normalize::Normalize},
    span::Span,
    types::{IntType, Type, UintType},
};
use std::fmt::Display;

impl<'s> LintSess<'s> {
    pub fn check_type_limits(&mut self, const_: &hir::Const) {
        match &const_.value {
            &ConstValue::Int(value) => match &const_.ty.normalize(self.tcx) {
                Type::Int(int_type) => {
                    let (min, max) = int_type_range(*int_type);

                    if value < min || value > max {
                        self.push_overflow_err(
                            value,
                            &const_.ty.display(self.tcx),
                            min,
                            max,
                            const_.span,
                        );
                    }
                }
                Type::Uint(uint_type) => {
                    let (_, max) = uint_type_range(*uint_type);

                    if value < 0 || value as u64 > max {
                        self.push_overflow_err(
                            value,
                            &const_.ty.display(self.tcx),
                            0,
                            max,
                            const_.span,
                        );
                    }
                }
                _ => (),
            },
            &ConstValue::Uint(value) => match &const_.ty.normalize(self.tcx) {
                Type::Int(int_type) => {
                    let (min, max) = int_type_range(*int_type);

                    if value > max as u64 {
                        self.push_overflow_err(
                            value,
                            &const_.ty.display(self.tcx),
                            min,
                            max,
                            const_.span,
                        );
                    }
                }
                Type::Uint(uint_type) => {
                    let (min, max) = uint_type_range(*uint_type);

                    if value < min || value > max {
                        self.push_overflow_err(
                            value,
                            &const_.ty.display(self.tcx),
                            min,
                            max,
                            const_.span,
                        );
                    }
                }
                _ => (),
            },
            _ => (),
        }
    }

    fn push_overflow_err<V: Copy + Display, M: Copy + Display, N: Copy + Display>(
        &mut self,
        value: V,
        type_display: &str,
        min: M,
        max: N,
        span: Span,
    ) {
        self.workspace.diagnostics.push(
            Diagnostic::error()
                .with_message(format!(
                    "integer literal of type `{}` must be between {} and {}, found {}",
                    type_display, min, max, value
                ))
                .with_label(Label::primary(span, "integer literal overflow")),
        );
    }
}

fn int_type_range(int_ty: IntType) -> (i64, i64) {
    match int_ty {
        IntType::I8 => (i8::MIN as i64, i8::MAX as i64),
        IntType::I16 => (i16::MIN as i64, i16::MAX as i64),
        IntType::I32 => (i32::MIN as i64, i32::MAX as i64),
        IntType::I64 => (i64::MIN, i64::MAX),
        IntType::Int => (isize::MIN as i64, isize::MAX as i64),
    }
}

fn uint_type_range(uint_ty: UintType) -> (u64, u64) {
    match uint_ty {
        UintType::U8 => (u8::MIN as u64, u8::MAX as u64),
        UintType::U16 => (u16::MIN as u64, u16::MAX as u64),
        UintType::U32 => (u32::MIN as u64, u32::MAX as u64),
        UintType::U64 => (u64::MIN, u64::MAX),
        UintType::Uint => (usize::MIN as u64, usize::MAX as u64),
    }
}
