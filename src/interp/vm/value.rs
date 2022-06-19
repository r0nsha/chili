use super::{
    super::ffi::RawPointer,
    super::{IS_64BIT, WORD_SIZE},
    byte_seq::{ByteSeq, GetValue},
    instruction::CompiledCode,
};
use crate::ast::{
    ast::{FunctionId, Intrinsic},
    const_value::{ConstArray, ConstElement, ConstFunction, ConstValue},
    ty::{align::AlignOf, size::SizeOf, FloatType, InferTy, IntType, Type, UintType},
};
use crate::infer::ty_ctx::TyCtx;
use crate::span::Span;
use byteorder::{NativeEndian, WriteBytesExt};
use indexmap::IndexMap;
use paste::paste;
use std::{ffi::c_void, fmt::Display, mem, slice, str};
use ustr::{ustr, Ustr};

macro_rules! impl_value {
    ($($variant:ident($ty:ty)) , + $(,)?) => {
        #[derive(PartialEq, Debug, Clone, Copy)]
        pub enum ValueKind {
            $(
                $variant
            ),+
        }

        impl Display for ValueKind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", match self {
                    $(
                        ValueKind::$variant => String::from(stringify!($variant))
                    ),+
                })
            }
        }

        #[derive(Debug, Clone)]
        pub enum Value {
            $(
                $variant($ty)
            ),+
        }

        #[allow(unused)]
        impl Value {
            pub fn kind(&self) -> ValueKind {
                match self {
                    $(
                        Value::$variant(_) => ValueKind::$variant
                    ),+
                }
            }

            pub fn as_c_ref(&mut self) -> *mut c_void {
                match self {
                    $(
                        Self::$variant(v) => v as *mut _ as *mut c_void
                    ),+
                }
            }

            pub unsafe fn from_kind_and_ptr(kind: ValueKind, ptr: RawPointer) -> Self {
                match kind {
                    $(
                        ValueKind::$variant => Self::$variant((*(ptr as *mut $ty)).clone())
                    ),+
                }
            }

            paste! {
                $(
                    pub fn [<is_ $variant:snake>](&self) -> bool {
                        match self {
                            Self::$variant(_) => true,
                            _ => false
                        }
                    }
                )+

                $(
                    pub fn [<into_ $variant:snake>](self) -> $ty {
                        match self {
                            Self::$variant(v) => v,
                            _ => panic!("got {}, expected {}", self.to_string(), stringify!($variant))
                        }
                    }
                )+

                $(
                    pub fn [<as_ $variant:snake>](&self) -> &$ty {
                        match self {
                            Self::$variant(v) => v,
                            _ => panic!("got {}, expected {}", self.to_string(), stringify!($variant))
                        }
                    }
                )+

                $(
                    pub fn [<as_ $variant:snake _mut>](&mut self) -> &mut $ty {
                        match self {
                            Self::$variant(v) => v,
                            _ => panic!("got {}, expected {}", self.to_string(), stringify!($variant))
                        }
                    }
                )+
            }
        }

        #[derive(Debug, Clone)]
        pub enum Pointer {
            $(
                $variant(*mut $ty)
            ),+
        }

        impl From<&mut Value> for Pointer {
            fn from(value: &mut Value) -> Self {
                match value {
                    $(
                        Value::$variant(v) => Pointer::$variant(v as _)
                    ),+
                }
            }
        }

        #[allow(unused)]
        impl Pointer {
            pub fn kind(&self) -> ValueKind {
                match self {
                    $(
                        Self::$variant(_) => ValueKind::$variant
                    ),+
                }
            }

            pub unsafe fn as_raw(&mut self) -> *mut RawPointer {
                match self {
                    $(
                        Self::$variant(ref mut v) => mem::transmute::<&mut *mut _, *mut RawPointer>(v)
                    ),+
                }
            }

            pub fn as_inner_raw(&self) -> RawPointer {
                match self {
                    $(
                        Self::$variant(v) => *v as RawPointer
                    ),+
                }
            }

            pub fn is_null(&self) -> bool {
                match self {
                    $(
                        Self::$variant(v) => v.is_null()
                    ),+
                }
            }

            pub unsafe fn deref_value(&self) -> Value {
                match self {
                    $(
                        Self::$variant(v) => Value::$variant((**v).clone())
                    ),+
                }
            }

            pub fn from_kind_and_ptr(kind: ValueKind, ptr: RawPointer) -> Self {
                match kind {
                    $(
                        ValueKind::$variant => Pointer::$variant(ptr as _)
                    ),+
                }
            }

            pub unsafe fn print(&self) {
                match self {
                    $(
                        Self::$variant(v) => println!("{:?}", **v)
                    ),+
                }
            }

            paste! {
                $(
                    pub fn [<is_ $variant:snake>](&self) -> bool {
                        match self {
                            Self::$variant(_) => true,
                            _ => false
                        }
                    }
                )+

                $(
                    pub fn [<into_ $variant:snake>](self) -> *mut $ty {
                        match self {
                            Self::$variant(v) => v,
                            _ => panic!("got {}, expected {}", self.to_string(), stringify!($variant))
                        }
                    }
                )+

                $(
                    pub fn [<as_ $variant:snake>](&self) -> &*mut $ty {
                        match self {
                            Self::$variant(v) => v,
                            _ => panic!("got {}, expected {}", self.to_string(), stringify!($variant))
                        }
                    }
                )+

                $(
                    pub fn [<as_ $variant:snake _mut>](&mut self) -> &mut *mut $ty {
                        match self {
                            Self::$variant(v) => v,
                            _ => panic!("got {}, expected {}", self.to_string(), stringify!($variant))
                        }
                    }
                )+
            }
        }
    };
}

impl_value! {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    Int(isize),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    Uint(usize),
    F32(f32),
    F64(f64),
    Bool(bool),
    Aggregate(Aggregate),
    Array(Array),
    Pointer(Pointer),
    Function(FunctionAddress),
    Intrinsic(IntrinsicFunction),
    Type(Type),
}

#[derive(Debug)]
pub struct Aggregate {
    pub elements: Vec<Value>,
    pub ty: Type,
}

impl Clone for Aggregate {
    fn clone(&self) -> Self {
        Self {
            elements: self.elements.clone(),
            ty: self.ty.clone(),
        }
    }
}

impl Aggregate {
    pub unsafe fn as_slice<T>(&self) -> &[T] {
        std::slice::from_raw_parts(
            self.elements[0].as_pointer().as_inner_raw() as *const T,
            *self.elements[1].as_uint(),
        )
    }

    pub unsafe fn as_str(&self) -> &str {
        std::str::from_utf8(self.as_slice::<u8>()).unwrap()
    }
}

#[derive(Debug, Clone)]
pub struct Array {
    pub bytes: ByteSeq,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub id: FunctionId,
    pub name: Ustr,
    pub arg_types: Vec<Type>,
    pub return_type: Type,
    pub code: CompiledCode,
}

#[derive(Debug, Clone)]
pub struct FunctionAddress {
    pub id: FunctionId,
    pub name: Ustr,
}

#[derive(Debug, Clone)]
pub struct ExternFunction {
    pub lib_path: Ustr,
    pub name: Ustr,
    pub param_tys: Vec<Type>,
    pub return_ty: Type,
    pub variadic: bool,
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum IntrinsicFunction {
    StartWorkspace,
}

impl From<Intrinsic> for IntrinsicFunction {
    fn from(intrinsic: Intrinsic) -> Self {
        match intrinsic {
            Intrinsic::StartWorkspace => IntrinsicFunction::StartWorkspace,
        }
    }
}

#[derive(Debug, Clone)]
pub enum FunctionValue<'a> {
    Orphan(&'a Function),
    Extern(&'a ExternFunction),
}

impl Display for IntrinsicFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                IntrinsicFunction::StartWorkspace => "start_workspace",
            }
        )
    }
}

impl From<&Type> for ValueKind {
    fn from(ty: &Type) -> Self {
        match ty {
            Type::Never | Type::Unit => ValueKind::Aggregate,
            Type::Bool => ValueKind::Bool,
            Type::Int(ty) => match ty {
                IntType::I8 => Self::I8,
                IntType::I16 => Self::I16,
                IntType::I32 => Self::I32,
                IntType::I64 => Self::I64,
                IntType::Int => Self::Int,
            },
            Type::Uint(ty) => match ty {
                UintType::U8 => Self::U8,
                UintType::U16 => Self::U16,
                UintType::U32 => Self::U32,
                UintType::U64 => Self::U64,
                UintType::Uint => Self::Uint,
            },
            Type::Float(ty) => match ty {
                FloatType::F16 | FloatType::F32 => Self::F32,
                FloatType::F64 => Self::F64,
                FloatType::Float => {
                    if IS_64BIT {
                        Self::F64
                    } else {
                        Self::F32
                    }
                }
            },
            Type::Pointer(_, _) | Type::MultiPointer(_, _) => Self::Pointer,
            Type::Function(_) => Self::Function,
            Type::Array(_, _) => Self::Array,
            Type::Slice(_, _) | Type::Tuple(_) | Type::Struct(_) => Self::Aggregate,
            Type::Module(_) => panic!(),
            Type::Type(_) => Self::Type,
            Type::Infer(_, InferTy::AnyInt) => Self::Int,
            Type::Infer(_, InferTy::AnyFloat) => {
                if IS_64BIT {
                    Self::F64
                } else {
                    Self::F32
                }
            }
            Type::Infer(_, InferTy::PartialStruct(_) | InferTy::PartialTuple(_)) => Self::Aggregate,
            _ => panic!("invalid type {}", ty),
        }
    }
}

impl Value {
    pub fn unit() -> Self {
        Value::Aggregate(Aggregate {
            elements: Vec::with_capacity(0),
            ty: Type::Unit,
        })
    }

    pub unsafe fn from_type_and_ptr(ty: &Type, ptr: RawPointer) -> Self {
        match ty {
            Type::Never | Type::Unit => Self::unit(),
            Type::Bool => Self::Bool(*(ptr as *mut bool)),
            Type::Int(ty) => match ty {
                IntType::I8 => Self::I8(*(ptr as *mut i8)),
                IntType::I16 => Self::I16(*(ptr as *mut i16)),
                IntType::I32 => Self::I32(*(ptr as *mut i32)),
                IntType::I64 => Self::I64(*(ptr as *mut i64)),
                IntType::Int => Self::Int(*(ptr as *mut isize)),
            },
            Type::Uint(ty) => match ty {
                UintType::U8 => Self::U8(*(ptr as *mut u8)),
                UintType::U16 => Self::U16(*(ptr as *mut u16)),
                UintType::U32 => Self::U32(*(ptr as *mut u32)),
                UintType::U64 => Self::U64(*(ptr as *mut u64)),
                UintType::Uint => Self::Uint(*(ptr as *mut usize)),
            },
            Type::Float(ty) => match ty {
                FloatType::F16 | FloatType::F32 => Self::F32(*(ptr as *mut f32)),
                FloatType::F64 => Self::F64(*(ptr as *mut f64)),
                FloatType::Float => {
                    if IS_64BIT {
                        Self::F64(*(ptr as *mut f64))
                    } else {
                        Self::F32(*(ptr as *mut f32))
                    }
                }
            },
            Type::Pointer(ty, _) | Type::MultiPointer(ty, _) => {
                Self::Pointer(Pointer::from_type_and_ptr(ty, *(ptr as *mut RawPointer)))
            }
            Type::Function(_) => todo!(),
            Type::Array(inner, size) => Self::Array(Array {
                bytes: ByteSeq::from_raw_parts(ptr as *mut u8, *size * inner.size_of(WORD_SIZE)),
                ty: ty.clone(),
            }),
            Type::Slice(_, _) => todo!(),
            Type::Tuple(_) => todo!(),
            Type::Struct(struct_ty) => {
                let mut elements = Vec::with_capacity(struct_ty.fields.len());
                let align = struct_ty.align_of(WORD_SIZE);

                for (index, field) in struct_ty.fields.iter().enumerate() {
                    let field_ptr = ptr.add(align * index);
                    let value = Value::from_type_and_ptr(&field.ty, field_ptr);
                    elements.push(value);
                }

                Self::Aggregate(Aggregate {
                    elements,
                    ty: ty.clone(),
                })
            }
            Type::Infer(_, InferTy::AnyInt) => Self::Int(*(ptr as *mut isize)),
            Type::Infer(_, InferTy::AnyFloat) => {
                if IS_64BIT {
                    Self::F64(*(ptr as *mut f64))
                } else {
                    Self::F32(*(ptr as *mut f32))
                }
            }
            Type::Infer(_, _) => todo!(),
            _ => panic!("invalid type {}", ty),
        }
    }

    pub fn get_ty_kind(&self) -> Type {
        match self {
            Self::I8(_) => Type::Int(IntType::I8),
            Self::I16(_) => Type::Int(IntType::I16),
            Self::I32(_) => Type::Int(IntType::I32),
            Self::I64(_) => Type::Int(IntType::I64),
            Self::Int(_) => Type::Int(IntType::Int),
            Self::U8(_) => Type::Uint(UintType::U8),
            Self::U16(_) => Type::Uint(UintType::U16),
            Self::U32(_) => Type::Uint(UintType::U32),
            Self::U64(_) => Type::Uint(UintType::U64),
            Self::Uint(_) => Type::Uint(UintType::Uint),
            Self::F32(_) => Type::Float(FloatType::F32),
            Self::F64(_) => Type::Float(FloatType::F64),
            Self::Bool(_) => Type::Bool,
            Self::Aggregate(agg) => agg.ty.clone(),
            Self::Array(arr) => arr.ty.clone(),
            Self::Pointer(p) => Type::Pointer(Box::new(p.get_ty_kind()), true),
            Self::Function(_) => todo!(),
            _ => panic!(),
        }
    }

    pub fn try_into_const_value(
        self,
        tycx: &mut TyCtx,
        ty: &Type,
        eval_span: Span,
    ) -> Result<ConstValue, &'static str> {
        match self {
            Self::I8(v) => Ok(ConstValue::Int(v as _)),
            Self::I16(v) => Ok(ConstValue::Int(v as _)),
            Self::I32(v) => Ok(ConstValue::Int(v as _)),
            Self::I64(v) => Ok(ConstValue::Int(v)),
            Self::Int(v) => Ok(ConstValue::Int(v as _)),
            Self::U8(v) => Ok(ConstValue::Uint(v as _)),
            Self::U16(v) => Ok(ConstValue::Uint(v as _)),
            Self::U32(v) => Ok(ConstValue::Uint(v as _)),
            Self::U64(v) => Ok(ConstValue::Uint(v as _)),
            Self::Uint(v) => Ok(ConstValue::Uint(v as _)),
            Self::F32(v) => Ok(ConstValue::Float(v as _)),
            Self::F64(v) => Ok(ConstValue::Float(v)),
            Self::Bool(v) => Ok(ConstValue::Bool(v)),
            Self::Type(t) => Ok(ConstValue::Type(tycx.bound(t, eval_span))),
            Self::Aggregate(agg) => match ty {
                Type::Unit => Ok(ConstValue::Unit(())),
                Type::Slice(inner, _) => {
                    if matches!(inner.as_ref(), Type::Uint(UintType::U8)) {
                        let data = agg.elements[0].as_pointer().as_inner_raw() as *mut u8;
                        let len = *agg.elements[1].as_uint();
                        let slice = unsafe { slice::from_raw_parts(data, len) };
                        let s = str::from_utf8(slice).expect("slice is not a valid utf8 string");
                        Ok(ConstValue::Str(ustr(s)))
                    } else {
                        Err("slice")
                    }
                }
                Type::Tuple(elements) => {
                    let mut values = Vec::with_capacity(agg.elements.len());

                    for (value, ty) in agg.elements.iter().zip(elements) {
                        let value = value.clone().try_into_const_value(tycx, ty, eval_span)?;
                        values.push(ConstElement {
                            value,
                            ty: tycx.bound(ty.clone(), eval_span),
                        });
                    }

                    Ok(ConstValue::Tuple(values))
                }
                Type::Struct(struct_ty) => {
                    let mut fields = IndexMap::<Ustr, ConstElement>::new();

                    for (value, field) in agg.elements.iter().zip(struct_ty.fields.iter()) {
                        let value = value
                            .clone()
                            .try_into_const_value(tycx, &field.ty, eval_span)?;
                        fields.insert(
                            field.symbol,
                            ConstElement {
                                value,
                                ty: tycx.bound(field.ty.clone(), field.span),
                            },
                        );
                    }

                    Ok(ConstValue::Struct(fields))
                }
                Type::Infer(_, InferTy::PartialTuple(elements)) => {
                    let mut values = Vec::with_capacity(agg.elements.len());

                    for (value, ty) in agg.elements.iter().zip(elements) {
                        let value = value.clone().try_into_const_value(tycx, ty, eval_span)?;
                        values.push(ConstElement {
                            value,
                            ty: tycx.bound(ty.clone(), eval_span),
                        });
                    }

                    Ok(ConstValue::Tuple(values))
                }
                Type::Infer(_, InferTy::PartialStruct(struct_ty)) => {
                    let mut fields = IndexMap::<Ustr, ConstElement>::new();

                    for (value, (name, ty)) in agg.elements.iter().zip(struct_ty.iter()) {
                        let value = value.clone().try_into_const_value(tycx, ty, eval_span)?;
                        fields.insert(
                            *name,
                            ConstElement {
                                value,
                                ty: tycx.bound(ty.clone(), eval_span),
                            },
                        );
                    }

                    Ok(ConstValue::Struct(fields))
                }
                ty => panic!(
                    "value type mismatch. expected an aggregate type, got {}",
                    ty
                ),
            },
            Self::Array(array) => {
                let (el_ty, array_len) = if let Type::Array(el_ty, len) = array.ty {
                    (el_ty, len)
                } else {
                    panic!()
                };

                let mut values = Vec::with_capacity(array_len);
                let el_size = el_ty.size_of(WORD_SIZE);

                for i in 0..array_len {
                    let value = array.bytes.offset(i * el_size).get_value(&el_ty);
                    let const_value = value.try_into_const_value(tycx, &el_ty, eval_span)?;
                    values.push(const_value);
                }

                Ok(ConstValue::Array(ConstArray {
                    values,
                    element_ty: tycx.bound(*el_ty, eval_span),
                }))
            }
            Self::Function(f) => Ok(ConstValue::Function(ConstFunction {
                id: f.id,
                name: f.name,
            })),
            Self::Pointer(_) => Err("pointer"),
            Self::Intrinsic(_) => Err("intrinsic function"),
        }
    }
}

impl From<Ustr> for Value {
    fn from(s: Ustr) -> Self {
        Value::Aggregate(Aggregate {
            elements: vec![
                Value::Pointer(Pointer::U8(s.as_char_ptr() as *mut u8)),
                Value::Uint(s.len()),
            ],
            ty: Type::str(),
        })
    }
}

impl Pointer {
    #[allow(unused)]
    pub fn unit() -> Self {
        Pointer::Aggregate(&mut Aggregate {
            elements: Vec::with_capacity(0),
            ty: Type::Unit,
        })
    }

    pub fn from_type_and_ptr(ty: &Type, ptr: RawPointer) -> Self {
        match ty {
            Type::Never | Type::Unit => Self::U8(ptr as _),
            Type::Bool => Self::Bool(ptr as _),
            Type::Int(ty) => match ty {
                IntType::I8 => Self::I8(ptr as _),
                IntType::I16 => Self::I16(ptr as _),
                IntType::I32 => Self::I32(ptr as _),
                IntType::I64 => Self::I64(ptr as _),
                IntType::Int => Self::Int(ptr as _),
            },
            Type::Uint(ty) => match ty {
                UintType::U8 => Self::U8(ptr as _),
                UintType::U16 => Self::U16(ptr as _),
                UintType::U32 => Self::U32(ptr as _),
                UintType::U64 => Self::U64(ptr as _),
                UintType::Uint => Self::Uint(ptr as _),
            },
            Type::Float(ty) => match ty {
                FloatType::F16 | FloatType::F32 => Self::F32(ptr as _),
                FloatType::F64 => Self::F64(ptr as _),
                FloatType::Float => {
                    if IS_64BIT {
                        Self::F32(ptr as _)
                    } else {
                        Self::F64(ptr as _)
                    }
                }
            },
            Type::Pointer(ty, _) | Type::MultiPointer(ty, _) => Self::from_type_and_ptr(ty, ptr),
            Type::Function(_) => todo!(),
            Type::Array(inner, size) => {
                let bytes = unsafe {
                    ByteSeq::from_raw_parts(ptr as *mut u8, *size * inner.size_of(WORD_SIZE))
                };

                let array = Box::new(Array {
                    bytes,
                    ty: ty.clone(),
                });

                Self::Array(Box::leak(array) as *mut Array)
            }
            Type::Slice(_, _) => todo!(),
            Type::Tuple(_) => todo!(),
            Type::Struct(_) => todo!(),
            Type::Infer(_, InferTy::AnyInt) => Self::Int(ptr as _),
            Type::Infer(_, InferTy::AnyFloat) => {
                if IS_64BIT {
                    Self::F32(ptr as _)
                } else {
                    Self::F64(ptr as _)
                }
            }
            Type::Infer(_, _) => todo!(),
            _ => panic!("invalid type {}", ty),
        }
    }

    pub fn get_ty_kind(&self) -> Type {
        match self {
            Self::I8(_) => Type::Int(IntType::I8),
            Self::I16(_) => Type::Int(IntType::I16),
            Self::I32(_) => Type::Int(IntType::I32),
            Self::I64(_) => Type::Int(IntType::I64),
            Self::Int(_) => Type::Int(IntType::Int),
            Self::U8(_) => Type::Uint(UintType::U8),
            Self::U16(_) => Type::Uint(UintType::U16),
            Self::U32(_) => Type::Uint(UintType::U32),
            Self::U64(_) => Type::Uint(UintType::U64),
            Self::Uint(_) => Type::Uint(UintType::Uint),
            Self::F32(_) => Type::Float(FloatType::F32),
            Self::F64(_) => Type::Float(FloatType::F64),
            Self::Bool(_) => Type::Bool,
            Self::Aggregate(_) => todo!(),
            Self::Pointer(p) => Type::Pointer(
                if p.is_null() {
                    Box::new(Type::Uint(UintType::U8))
                } else {
                    Box::new(unsafe { &**p }.get_ty_kind())
                },
                true,
            ),
            Self::Function(_) => todo!(),
            _ => panic!(),
        }
    }

    pub unsafe fn write_value(&self, value: Value) {
        unsafe fn slice<'a, T>(p: &*mut T) -> &'a mut [u8] {
            slice::from_raw_parts_mut(*p as *mut u8, mem::size_of::<T>())
        }

        match (self, value) {
            (Self::I8(p), Value::I8(v)) => slice(p).write_i8(v).unwrap(),
            (Self::I16(p), Value::I16(v)) => slice(p).write_i16::<NativeEndian>(v).unwrap(),
            (Self::I32(p), Value::I32(v)) => slice(p).write_i32::<NativeEndian>(v).unwrap(),
            (Self::I64(p), Value::I64(v)) => slice(p).write_i64::<NativeEndian>(v).unwrap(),
            (Self::Int(p), Value::Int(v)) => slice(p)
                .write_int::<NativeEndian>(v as i64, WORD_SIZE)
                .unwrap(),
            (Self::U8(p), Value::U8(v)) => slice(p).write_u8(v).unwrap(),
            (Self::U16(p), Value::U16(v)) => slice(p).write_u16::<NativeEndian>(v).unwrap(),
            (Self::U32(p), Value::U32(v)) => slice(p).write_u32::<NativeEndian>(v).unwrap(),
            (Self::U64(p), Value::U64(v)) => slice(p).write_u64::<NativeEndian>(v).unwrap(),
            (Self::Uint(p), Value::Uint(v)) => slice(p)
                .write_uint::<NativeEndian>(v as u64, WORD_SIZE)
                .unwrap(),
            (Self::F32(p), Value::F32(v)) => slice(p).write_f32::<NativeEndian>(v).unwrap(),
            (Self::F64(p), Value::F64(v)) => slice(p).write_f64::<NativeEndian>(v).unwrap(),
            (Self::Bool(p), Value::Bool(v)) => slice(p).write_u8(v as u8).unwrap(),
            (Self::Aggregate(p), Value::Aggregate(v)) => **p = v,
            (Self::Array(p), Value::Array(v)) => **p = v,
            (Self::Pointer(p), Value::Pointer(v)) => **p = v,
            (Self::Function(p), Value::Function(v)) => **p = v,
            (Self::Type(p), Value::Type(v)) => **p = v,
            (p, v) => panic!("invalid pair {:?} , {}", p, v.to_string()),
        }
    }
}

const MAX_CONSECUTIVE_VALUES: isize = 4;

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Value::I8(v) => format!("i8 {}", v),
                Value::I16(v) => format!("i16 {}", v),
                Value::I32(v) => format!("i32 {}", v),
                Value::I64(v) => format!("i64 {}", v),
                Value::Int(v) => format!("int {}", v),
                Value::U8(v) => format!("u8 {}", v),
                Value::U16(v) => format!("u16 {}", v),
                Value::U32(v) => format!("u32 {}", v),
                Value::U64(v) => format!("u64 {}", v),
                Value::Uint(v) => format!("uint {}", v),
                Value::F32(v) => format!("f32 {}", v),
                Value::F64(v) => format!("f64 {}", v),
                Value::Bool(v) => format!("bool {}", v),
                Value::Aggregate(v) => v.to_string(),
                Value::Array(v) => v.to_string(),
                Value::Pointer(p) => p.to_string(),
                Value::Function(v) => v.to_string(),
                Value::Intrinsic(v) => v.to_string(),
                Value::Type(ty) => format!("type {}", ty),
            }
        )
    }
}

impl Display for Aggregate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let extra_values = self.elements.len() as isize - MAX_CONSECUTIVE_VALUES;

        write!(
            f,
            "{{ {}{} }}",
            self.elements
                .iter()
                .take(MAX_CONSECUTIVE_VALUES as usize)
                .map(|v| v.to_string())
                .collect::<Vec<String>>()
                .join(", "),
            if extra_values > 0 {
                format!(", +{} more", extra_values)
            } else {
                "".to_string()
            }
        )
    }
}

impl Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let bytes = &self.bytes;

        let ty = self.ty.inner();
        let element_size = ty.size_of(WORD_SIZE);
        let size = (bytes.len() / element_size) as isize;

        let mut elements = vec![];

        for i in 0..size.min(MAX_CONSECUTIVE_VALUES) {
            let el = bytes.offset(element_size * (i as usize)).get_value(ty);
            elements.push(el.to_string());
        }

        let extra_values = size - MAX_CONSECUTIVE_VALUES;

        write!(
            f,
            "[{}{}]",
            elements.join(", "),
            if extra_values > 0 {
                format!(", +{} more", extra_values)
            } else {
                "".to_string()
            }
        )
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}", self.name)
    }
}

impl Display for FunctionAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}", self.name)
    }
}

impl Display for ExternFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "extern fn {}", self.name)
    }
}

impl Display for Pointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value = if self.as_inner_raw().is_null() {
            "null".to_string()
        } else {
            unsafe {
                match self {
                    Pointer::I8(v) => format!("i8 {}", **v),
                    Pointer::I16(v) => format!("i16 {}", **v),
                    Pointer::I32(v) => format!("i32 {}", **v),
                    Pointer::I64(v) => format!("i64 {}", **v),
                    Pointer::Int(v) => format!("int {}", **v),
                    Pointer::U8(v) => format!("u8 {}", **v),
                    Pointer::U16(v) => format!("u16 {}", **v),
                    Pointer::U32(v) => format!("u32 {}", **v),
                    Pointer::U64(v) => format!("u64 {}", **v),
                    Pointer::Uint(v) => format!("uint {}", **v),
                    Pointer::F32(v) => format!("f32 {}", **v),
                    Pointer::F64(v) => format!("f64 {}", **v),
                    Pointer::Bool(v) => format!("bool {}", **v),
                    Pointer::Aggregate(v) => (**v).to_string(),
                    Pointer::Array(v) => (**v).to_string(),
                    Pointer::Pointer(p) => (**p).to_string(),
                    Pointer::Function(v) => (**v).to_string(),
                    Pointer::Intrinsic(v) => (**v).to_string(),
                    Pointer::Type(ty) => format!("type {}", (**ty)),
                }
            }
        };

        write!(f, "ptr {}", value)
    }
}
