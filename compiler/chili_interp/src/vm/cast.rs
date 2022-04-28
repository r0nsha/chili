use crate::{
    instruction::CastInstruction,
    value::{Value, ValuePtr},
    vm::VM,
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
            _ => panic!("invalid value {}", $value),
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
            _ => panic!("invalid value {}", $value),
        }
    };
}

impl<'vm> VM<'vm> {
    #[inline]
    pub(super) fn cast_inst(&mut self, cast: CastInstruction) {
        let value = self.stack.pop();

        let new_value = match cast {
            CastInstruction::I8 => cast_to_int!(value => I8, i8),
            CastInstruction::I16 => cast_to_int!(value => I16, i16),
            CastInstruction::I32 => cast_to_int!(value => I32, i32),
            CastInstruction::I64 => cast_to_int!(value => I64, i64),
            CastInstruction::Int => cast_to_int!(value => Int, isize),
            CastInstruction::U8 => cast_to_int!(value => U8, u8),
            CastInstruction::U16 => cast_to_int!(value => U16, u16),
            CastInstruction::U32 => cast_to_int!(value => U32, u32),
            CastInstruction::U64 => cast_to_int!(value => U64, u64),
            CastInstruction::Uint => cast_to_int!(value => Uint, usize),
            CastInstruction::F32 => cast_to_float!(value => F32, f32),
            CastInstruction::F64 => cast_to_float!(value => F64, f64),
            CastInstruction::Ptr(kind) => match value {
                // TODO: think how to pass the ptr's value (maybe as a type constant?)
                Value::Pointer(ptr) => {
                    let raw = ptr.as_inner_raw();
                    let new_ptr = ValuePtr::from_kind_and_ptr(kind, raw);
                    Value::Pointer(new_ptr)
                }
                _ => panic!("invalid value {}", value),
            },
        };

        self.stack.push(new_value);
    }
}
