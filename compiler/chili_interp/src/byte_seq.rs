use std::ptr;

use bytes::{buf::UninitSlice, Buf, BufMut};
use chili_ast::ty::{FloatTy, InferTy, IntTy, TyKind, UintTy};

use crate::{
    value::{Pointer, Value},
    IS_64BIT,
};

#[derive(Debug, Clone)]
pub struct ByteSeq {
    inner: Box<[u8]>,
}

impl ByteSeq {
    pub(crate) fn new(len: usize) -> Self {
        Self {
            inner: vec![0u8; len.into()].into_boxed_slice(),
        }
    }
    pub(crate) unsafe fn from_raw_parts(ptr: *mut u8, len: usize) -> Self {
        Self {
            inner: Vec::from_raw_parts(ptr, len, len).into_boxed_slice(),
        }
    }

    pub(crate) fn offset(&self, offset: usize) -> &[u8] {
        &self.inner[offset.into()..]
    }

    pub(crate) fn offset_mut(&mut self, offset: usize) -> &mut [u8] {
        &mut self.inner[offset.into()..]
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

    pub(crate) fn as_ptr(&self) -> *const u8 {
        self.inner.as_ptr()
    }

    pub(crate) fn as_mut_ptr(&mut self) -> *mut u8 {
        self.inner.as_mut_ptr()
    }
}

impl Buf for ByteSeq {
    #[inline]
    fn remaining(&self) -> usize {
        self.inner.len()
    }

    #[inline]
    fn chunk(&self) -> &[u8] {
        &self.inner
    }

    #[inline]
    fn advance(&mut self, _cnt: usize) {
        // currently a no-op
        // // *self = &self[cnt..];
    }
}

unsafe impl BufMut for ByteSeq {
    #[inline]
    fn remaining_mut(&self) -> usize {
        self.inner.len()
    }

    #[inline]
    fn chunk_mut(&mut self) -> &mut UninitSlice {
        // UninitSlice is repr(transparent), so safe to transmute
        unsafe { &mut *(&mut *self.inner as *mut [u8] as *mut _) }
    }

    #[inline]
    unsafe fn advance_mut(&mut self, _cnt: usize) {
        // Lifetime dance taken from `impl Write for &mut [u8]`.
        // let (_, b) = core::mem::replace(&mut self.inner, &mut []).split_at_mut(cnt);
        // self.inner = b;
    }

    #[inline]
    fn put_slice(&mut self, src: &[u8]) {
        self.inner[..src.len()].copy_from_slice(src);
        unsafe {
            self.advance_mut(src.len());
        }
    }

    fn put_bytes(&mut self, val: u8, cnt: usize) {
        assert!(self.remaining_mut() >= cnt);
        unsafe {
            ptr::write_bytes(self.inner.as_mut_ptr(), val, cnt);
            self.advance_mut(cnt);
        }
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
            Value::I8(v) => self.as_mut().put_i8(*v),
            Value::I16(v) => self.as_mut().put_i16_le(*v),
            Value::I32(v) => self.as_mut().put_i32_le(*v),
            Value::I64(v) => self.as_mut().put_i64(*v),
            Value::Int(v) => {
                if IS_64BIT {
                    self.as_mut().put_i64_le(*v as i64)
                } else {
                    self.as_mut().put_i32_le(*v as i32)
                }
            }
            Value::U8(v) => self.as_mut().put_u8(*v),
            Value::U16(v) => self.as_mut().put_u16_le(*v),
            Value::U32(v) => self.as_mut().put_u32_le(*v),
            Value::U64(v) => self.as_mut().put_u64_le(*v),
            Value::Uint(v) => {
                if IS_64BIT {
                    self.as_mut().put_u64_le(*v as u64)
                } else {
                    self.as_mut().put_u32_le(*v as u32)
                }
            }
            Value::F32(v) => self.as_mut().put_f32_le(*v),
            Value::F64(v) => self.as_mut().put_f64_le(*v),
            Value::Bool(v) => self.as_mut().put_uint_le(*v as u64, 1),
            Value::Aggregate(v) => {
                // TODO: need to include struct padding here
                for value in v.elements.iter() {
                    self.as_mut().put_value(value)
                }
            }
            Value::Pointer(v) => self.as_mut().put_u64_le(v.as_inner_raw() as u64),
            Value::Func(_) => todo!(),
            Value::ForeignFunc(_) => todo!(),
            _ => panic!(
                "can't convert `{}` to raw self.as_mut().inner",
                value.to_string()
            ),
        }
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
            TyKind::Bool => Value::Bool(self.as_ref().get_u8() != 0),
            TyKind::Int(ty) => match ty {
                IntTy::I8 => Value::I8(self.as_ref().get_i8()),
                IntTy::I16 => Value::I16(self.as_ref().get_i16()),
                IntTy::I32 => Value::I32(self.as_ref().get_i32()),
                IntTy::I64 => Value::I64(self.as_ref().get_i64()),
                IntTy::Int => Value::Int(if IS_64BIT {
                    self.as_ref().get_i64() as isize
                } else {
                    self.as_ref().get_i32() as isize
                }),
            },
            TyKind::Uint(ty) => match ty {
                UintTy::U8 => Value::U8(self.as_ref().get_u8()),
                UintTy::U16 => Value::U16(self.as_ref().get_u16()),
                UintTy::U32 => Value::U32(self.as_ref().get_u32()),
                UintTy::U64 => Value::U64(self.as_ref().get_u64()),
                UintTy::Uint => Value::Uint(if IS_64BIT {
                    self.as_ref().get_u64() as usize
                } else {
                    self.as_ref().get_u32() as usize
                }),
            },
            TyKind::Float(ty) => match ty {
                FloatTy::F16 | FloatTy::F32 => Value::F32(self.as_ref().get_f32()),
                FloatTy::F64 => Value::F64(self.as_ref().get_f64()),
                FloatTy::Float => {
                    if IS_64BIT {
                        Value::F64(self.as_ref().get_f64())
                    } else {
                        Value::F32(self.as_ref().get_f32())
                    }
                }
            },
            TyKind::Pointer(ty, _) | TyKind::MultiPointer(ty, _) => {
                Value::Pointer(Pointer::from_type_and_ptr(ty, self.as_ref().get_i64() as _))
            }
            TyKind::Array(_, _) => todo!(),
            TyKind::Slice(_, _) => todo!(),
            TyKind::Tuple(_) => todo!(),
            TyKind::Struct(_) => todo!(),
            TyKind::Infer(_, InferTy::AnyInt) => Value::I32(self.as_ref().get_i32()),
            TyKind::Infer(_, InferTy::AnyFloat) => Value::F32(self.as_ref().get_f32()),
            _ => panic!(
                "can't get value of type `{}` from raw self.as_ref().inner",
                ty
            ),
        }
    }
}
