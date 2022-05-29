use byteorder::{NativeEndian, ReadBytesExt, WriteBytesExt};
use chili_ast::ty::{FloatTy, InferTy, IntTy, TyKind, UintTy};

use crate::{
    vm::value::{Pointer, Value},
    IS_64BIT, WORD_SIZE,
};

#[derive(Debug, Clone)]
pub struct ByteSeq {
    pub inner: Box<[u8]>,
}

impl ByteSeq {
    pub(crate) fn new(len: usize) -> Self {
        Self {
            inner: vec![0u8; len].into_boxed_slice(),
        }
    }
    pub(crate) unsafe fn from_raw_parts(ptr: *mut u8, len: usize) -> Self {
        Self {
            inner: Vec::from_raw_parts(ptr, len, len).into_boxed_slice(),
        }
    }

    pub(crate) fn offset(&self, offset: usize) -> &[u8] {
        &self.inner[offset..]
    }

    pub(crate) fn offset_mut(&mut self, offset: usize) -> &mut [u8] {
        &mut self.inner[offset..]
    }

    pub(crate) fn len(&self) -> usize {
        self.inner.len()
    }

    pub(crate) fn as_ref(&self) -> &[u8] {
        &self.inner
    }

    pub(crate) fn as_mut(&mut self) -> &mut [u8] {
        &mut self.inner
    }

    #[allow(unused)]
    pub(crate) fn as_ptr(&self) -> *const u8 {
        self.inner.as_ptr()
    }

    pub(crate) fn as_mut_ptr(&mut self) -> *mut u8 {
        self.inner.as_mut_ptr()
    }
}

pub(crate) trait PutValue {
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
            Value::Aggregate(v) => {
                // TODO: need to include struct padding here
                for value in v.elements.iter() {
                    self.as_mut().put_value(value)
                }
                Ok(())
            }
            Value::Pointer(v) => self
                .as_mut()
                .write_uint::<NativeEndian>(v.as_inner_raw() as u64, WORD_SIZE),
            Value::Function(_) => todo!(),
            Value::ExternFunction(_) => todo!(),
            _ => panic!(
                "can't convert `{}` to raw self.as_mut().inner",
                value.to_string()
            ),
        }
        .unwrap()
    }
}

pub(crate) trait GetValue {
    fn get_value(&self, ty: &TyKind) -> Value;
}

impl GetValue for ByteSeq {
    fn get_value(&self, ty: &TyKind) -> Value {
        self.as_ref().get_value(ty)
    }
}

impl GetValue for [u8] {
    fn get_value(&self, ty: &TyKind) -> Value {
        match ty {
            TyKind::Never | TyKind::Unit => Value::unit(), // these types' sizes are zero self.as_ref().inner
            TyKind::Bool => Value::Bool(self.as_ref().read_u8().unwrap() != 0),
            TyKind::Int(ty) => match ty {
                IntTy::I8 => Value::I8(self.as_ref().read_i8().unwrap()),
                IntTy::I16 => Value::I16(self.as_ref().read_i16::<NativeEndian>().unwrap()),
                IntTy::I32 => Value::I32(self.as_ref().read_i32::<NativeEndian>().unwrap()),
                IntTy::I64 => Value::I64(self.as_ref().read_i64::<NativeEndian>().unwrap()),
                IntTy::Int => {
                    Value::Int(self.as_ref().read_int::<NativeEndian>(WORD_SIZE).unwrap() as isize)
                }
            },
            TyKind::Uint(ty) => match ty {
                UintTy::U8 => Value::U8(self.as_ref().read_u8().unwrap()),
                UintTy::U16 => Value::U16(self.as_ref().read_u16::<NativeEndian>().unwrap()),
                UintTy::U32 => Value::U32(self.as_ref().read_u32::<NativeEndian>().unwrap()),
                UintTy::U64 => Value::U64(self.as_ref().read_u64::<NativeEndian>().unwrap()),
                UintTy::Uint => {
                    Value::Uint(self.as_ref().read_uint::<NativeEndian>(WORD_SIZE).unwrap() as usize)
                }
            },
            TyKind::Float(ty) => match ty {
                FloatTy::F16 | FloatTy::F32 => {
                    Value::F32(self.as_ref().read_f32::<NativeEndian>().unwrap())
                }
                FloatTy::F64 => Value::F64(self.as_ref().read_f64::<NativeEndian>().unwrap()),
                FloatTy::Float => {
                    if IS_64BIT {
                        Value::F64(self.as_ref().read_f64::<NativeEndian>().unwrap())
                    } else {
                        Value::F32(self.as_ref().read_f32::<NativeEndian>().unwrap())
                    }
                }
            },
            TyKind::Pointer(ty, _) | TyKind::MultiPointer(ty, _) => {
                Value::Pointer(Pointer::from_type_and_ptr(
                    ty,
                    self.as_ref().read_uint::<NativeEndian>(WORD_SIZE).unwrap() as _,
                ))
            }
            TyKind::Array(_, _) => todo!(),
            TyKind::Slice(_, _) => todo!(),
            TyKind::Tuple(_) => todo!(),
            TyKind::Struct(_) => todo!(),
            TyKind::Infer(_, InferTy::AnyInt) => {
                Value::Int(self.as_ref().read_int::<NativeEndian>(WORD_SIZE).unwrap() as isize)
            }
            TyKind::Infer(_, InferTy::AnyFloat) => {
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
