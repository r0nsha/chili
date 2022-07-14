use super::{
    super::{IS_64BIT, WORD_SIZE},
    value::{Pointer, Value},
};
use crate::types::{FloatType, InferTy, IntType, Type, UintType};
use byteorder::{NativeEndian, ReadBytesExt, WriteBytesExt};
use std::io::Write;

#[derive(Debug, Clone)]
pub struct ByteSeq {
    pub inner: Box<[u8]>,
}

impl From<&[u8]> for ByteSeq {
    fn from(slice: &[u8]) -> Self {
        let bytes = Self::new(slice.len());
        bytes.as_mut().write_all(slice);
        bytes
    }
}

impl ByteSeq {
    pub fn new(len: usize) -> Self {
        Self {
            inner: vec![0u8; len].into_boxed_slice(),
        }
    }

    pub unsafe fn from_raw_parts(ptr: *mut u8, len: usize) -> Self {
        Self {
            inner: Vec::from_raw_parts(ptr, len, len).into_boxed_slice(),
        }
    }

    pub(crate) fn from_values(values: &[Value], size: usize, align: usize) -> Self {
        let bytes = Self::new(size);

        for (index, value) in values.iter().enumerate() {
            bytes.offset(index * align).put_value(value);
        }

        bytes
    }

    pub fn offset(&self, offset: usize) -> &[u8] {
        &self.inner[offset..]
    }

    pub fn offset_mut(&mut self, offset: usize) -> &mut [u8] {
        &mut self.inner[offset..]
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn as_ref(&self) -> &[u8] {
        &self.inner
    }

    pub fn as_mut(&mut self) -> &mut [u8] {
        &mut self.inner
    }

    #[allow(unused)]
    pub fn as_ptr(&self) -> *const u8 {
        self.inner.as_ptr()
    }

    pub fn as_mut_ptr(&mut self) -> *mut u8 {
        self.inner.as_mut_ptr()
    }
}

pub trait PutValue {
    fn put_value(&mut self, value: &Value);
}

impl PutValue for ByteSeq {
    fn put_value(&mut self, value: &Value) {
        self.as_mut().put_value(value)
    }
}

// Note (Ron): Important - This function WILL fail in Big Endian systems!!
// Note (Ron): This isn't very crucial, since the most common systems are little endian - but this needs to be fixed anyway.
impl PutValue for [u8] {
    fn put_value(&mut self, value: &Value) {
        match value {
            Value::I8(v) => self.as_mut().write_i8(*v),
            Value::I16(v) => self.as_mut().write_i16::<NativeEndian>(*v),
            Value::I32(v) => self.as_mut().write_i32::<NativeEndian>(*v),
            Value::I64(v) => self.as_mut().write_i64::<NativeEndian>(*v),
            Value::Int(v) => self
                .as_mut()
                .write_int::<NativeEndian>(*v as i64, WORD_SIZE),
            Value::U8(v) => self.as_mut().write_u8(*v),
            Value::U16(v) => self.as_mut().write_u16::<NativeEndian>(*v),
            Value::U32(v) => self.as_mut().write_u32::<NativeEndian>(*v),
            Value::U64(v) => self.as_mut().write_u64::<NativeEndian>(*v),
            Value::Uint(v) => self
                .as_mut()
                .write_uint::<NativeEndian>(*v as u64, WORD_SIZE),
            Value::F32(v) => self.as_mut().write_f32::<NativeEndian>(*v),
            Value::F64(v) => self.as_mut().write_f64::<NativeEndian>(*v),
            Value::Bool(v) => self.as_mut().write_u8(*v as u8),
            Value::Buffer(v) => {
                self.as_mut().write_all(v.bytes.inner.as_ref());
                Ok(())
            }
            Value::Pointer(v) => self
                .as_mut()
                .write_uint::<NativeEndian>(v.as_inner_raw() as u64, WORD_SIZE),
            Value::Function(_) => todo!(),
            _ => panic!(
                "can't convert `{}` to raw self.as_mut().inner",
                value.to_string()
            ),
        }
        .unwrap()
    }
}

pub trait GetValue {
    fn get_value(&self, ty: &Type) -> Value;
}

impl GetValue for ByteSeq {
    fn get_value(&self, ty: &Type) -> Value {
        self.as_ref().get_value(ty)
    }
}

impl GetValue for [u8] {
    fn get_value(&self, ty: &Type) -> Value {
        match ty {
            Type::Never | Type::Unit => Value::unit(), // these types' sizes are zero self.as_ref().inner
            Type::Bool => Value::Bool(self.as_ref().read_u8().unwrap() != 0),
            Type::Int(ty) => match ty {
                IntType::I8 => Value::I8(self.as_ref().read_i8().unwrap()),
                IntType::I16 => Value::I16(self.as_ref().read_i16::<NativeEndian>().unwrap()),
                IntType::I32 => Value::I32(self.as_ref().read_i32::<NativeEndian>().unwrap()),
                IntType::I64 => Value::I64(self.as_ref().read_i64::<NativeEndian>().unwrap()),
                IntType::Int => {
                    Value::Int(self.as_ref().read_int::<NativeEndian>(WORD_SIZE).unwrap() as isize)
                }
            },
            Type::Uint(ty) => match ty {
                UintType::U8 => Value::U8(self.as_ref().read_u8().unwrap()),
                UintType::U16 => Value::U16(self.as_ref().read_u16::<NativeEndian>().unwrap()),
                UintType::U32 => Value::U32(self.as_ref().read_u32::<NativeEndian>().unwrap()),
                UintType::U64 => Value::U64(self.as_ref().read_u64::<NativeEndian>().unwrap()),
                UintType::Uint => {
                    Value::Uint(self.as_ref().read_uint::<NativeEndian>(WORD_SIZE).unwrap() as usize)
                }
            },
            Type::Float(ty) => match ty {
                FloatType::F16 | FloatType::F32 => {
                    Value::F32(self.as_ref().read_f32::<NativeEndian>().unwrap())
                }
                FloatType::F64 => Value::F64(self.as_ref().read_f64::<NativeEndian>().unwrap()),
                FloatType::Float => {
                    if IS_64BIT {
                        Value::F64(self.as_ref().read_f64::<NativeEndian>().unwrap())
                    } else {
                        Value::F32(self.as_ref().read_f32::<NativeEndian>().unwrap())
                    }
                }
            },
            Type::Pointer(ty, _) => Value::Pointer(Pointer::from_type_and_ptr(
                ty,
                self.as_ref().read_uint::<NativeEndian>(WORD_SIZE).unwrap() as _,
            )),
            Type::Array(_, _) => todo!(),
            Type::Slice(_, _) => todo!(),
            Type::Tuple(_) => todo!(),
            Type::Struct(_) => todo!(),
            Type::Infer(_, InferTy::AnyInt) => {
                Value::Int(self.as_ref().read_int::<NativeEndian>(WORD_SIZE).unwrap() as isize)
            }
            Type::Infer(_, InferTy::AnyFloat) => {
                if IS_64BIT {
                    Value::F64(self.as_ref().read_f64::<NativeEndian>().unwrap())
                } else {
                    Value::F32(self.as_ref().read_f32::<NativeEndian>().unwrap())
                }
            }
            _ => panic!(
                "can't get value of type `{}` from raw self.as_ref().inner",
                ty
            ),
        }
    }
}
