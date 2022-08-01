use crate::types::{FloatType, InferType, IntType, Type, UintType};

use super::{
    super::ffi::RawPointer,
    value::{Pointer, Value},
    VM,
};

macro_rules! cast_to_int {
    ($value:expr => $name:ident, $to:ty) => {
        match $value {
            Value::I8(v) => Value::$name(v as $to),
            Value::I16(v) => Value::$name(v as $to),
            Value::I32(v) => Value::$name(v as $to),
            Value::I64(v) => Value::$name(v as $to),
            Value::Int(v) => Value::$name(v as $to),
            Value::U8(v) => Value::$name(v as $to),
            Value::U16(v) => Value::$name(v as $to),
            Value::U32(v) => Value::$name(v as $to),
            Value::U64(v) => Value::$name(v as $to),
            Value::Uint(v) => Value::$name(v as $to),
            Value::F32(v) => Value::$name(v as $to),
            Value::F64(v) => Value::$name(v as $to),
            Value::Bool(v) => Value::$name(v as $to),
            _ => panic!("invalid value {}", $value.to_string()),
        }
    };
}

// Note (Ron): We have a variant for floats since bool can't be cast to float
macro_rules! cast_to_float {
    ($value:expr => $name:ident, $to:ty) => {
        match $value {
            Value::I8(v) => Value::$name(v as $to),
            Value::I16(v) => Value::$name(v as $to),
            Value::I32(v) => Value::$name(v as $to),
            Value::I64(v) => Value::$name(v as $to),
            Value::Int(v) => Value::$name(v as $to),
            Value::U8(v) => Value::$name(v as $to),
            Value::U16(v) => Value::$name(v as $to),
            Value::U32(v) => Value::$name(v as $to),
            Value::U64(v) => Value::$name(v as $to),
            Value::Uint(v) => Value::$name(v as $to),
            Value::F32(v) => Value::$name(v as $to),
            Value::F64(v) => Value::$name(v as $to),
            _ => panic!("invalid value {}", $value.to_string()),
        }
    };
}

impl<'vm> VM<'vm> {
    #[inline]
    pub fn cast_op(&mut self) {
        let ty = self.stack.pop().into_type();
        let value = self.stack.pop();

        let new_value = match ty {
            Type::Int(IntType::I8) => cast_to_int!(value => I8, i8),
            Type::Int(IntType::I16) => cast_to_int!(value => I16, i16),
            Type::Int(IntType::I32) => cast_to_int!(value => I32, i32),
            Type::Int(IntType::I64) => cast_to_int!(value => I64, i64),
            Type::Int(IntType::Int) | Type::Infer(_, InferType::AnyInt) => {
                cast_to_int!(value => Int, isize)
            }
            Type::Uint(UintType::U8) => cast_to_int!(value => U8, u8),
            Type::Uint(UintType::U16) => cast_to_int!(value => U16, u16),
            Type::Uint(UintType::U32) => cast_to_int!(value => U32, u32),
            Type::Uint(UintType::U64) => cast_to_int!(value => U64, u64),
            Type::Uint(UintType::Uint) => cast_to_int!(value => Uint, usize),
            Type::Float(FloatType::F32) => cast_to_float!(value => F32, f32),
            Type::Float(FloatType::F64) => cast_to_float!(value => F64, f64),
            Type::Float(FloatType::Float) | Type::Infer(_, InferType::AnyFloat) => {
                cast_to_float!(value => F64, f64)
            }
            Type::Pointer(inner, _) => {
                let raw_ptr = match value {
                    Value::Int(value) => value as RawPointer,
                    Value::Uint(value) => value as RawPointer,
                    Value::Pointer(ptr) => ptr.as_inner_raw(),
                    _ => panic!("invalid value {}", value.to_string()),
                };

                let new_ptr = Pointer::from_type_and_ptr(&inner, raw_ptr);
                Value::Pointer(new_ptr)
            }
            _ => panic!("{:?}", ty),
        };

        self.stack.push(new_value);
    }
}
