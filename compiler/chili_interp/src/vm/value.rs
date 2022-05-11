use crate::{
    ffi::RawPointer,
    vm::{
        byte_seq::{ByteSeq, GetValue},
        instruction::CompiledCode,
    },
    IS_64BIT, WORD_SIZE,
};
use byteorder::{NativeEndian, WriteBytesExt};
use chili_ast::{
    const_value::ConstValue,
    ty::{align::AlignOf, size::SizeOf, FloatTy, InferTy, IntTy, TyKind, UintTy},
};
use chili_infer::ty_ctx::TyCtx;
use chili_span::Span;
use paste::paste;
use std::{ffi::c_void, fmt::Display, mem, slice};
use ustr::Ustr;

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

        impl Value {
            #[allow(dead_code)]
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

        impl Pointer {
            #[allow(dead_code)]
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
    Func(Func),
    ForeignFunc(ForeignFunc),
    Type(TyKind),
}

#[derive(Debug, Clone)]
pub struct Aggregate {
    pub elements: Vec<Value>,
    pub ty: TyKind,
}

#[derive(Debug, Clone)]
pub struct Array {
    pub bytes: ByteSeq,
    pub ty: TyKind,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: Ustr,
    pub arg_types: Vec<TyKind>,
    pub return_type: TyKind,
    pub code: CompiledCode,
}

#[derive(Debug, Clone)]
pub struct ForeignFunc {
    pub lib_path: Ustr,
    pub name: Ustr,
    pub param_tys: Vec<TyKind>,
    pub return_ty: TyKind,
    pub variadic: bool,
}

impl From<&TyKind> for ValueKind {
    fn from(ty: &TyKind) -> Self {
        match ty {
            TyKind::Never | TyKind::Unit => ValueKind::Aggregate,
            TyKind::Bool => ValueKind::Bool,
            TyKind::Int(ty) => match ty {
                IntTy::I8 => Self::I8,
                IntTy::I16 => Self::I16,
                IntTy::I32 => Self::I32,
                IntTy::I64 => Self::I64,
                IntTy::Int => Self::Int,
            },
            TyKind::Uint(ty) => match ty {
                UintTy::U8 => Self::U8,
                UintTy::U16 => Self::U16,
                UintTy::U32 => Self::U32,
                UintTy::U64 => Self::U64,
                UintTy::Uint => Self::Uint,
            },
            TyKind::Float(ty) => match ty {
                FloatTy::F16 | FloatTy::F32 => Self::F32,
                FloatTy::F64 => Self::F64,
                FloatTy::Float => {
                    if IS_64BIT {
                        Self::F64
                    } else {
                        Self::F32
                    }
                }
            },
            TyKind::Pointer(_, _) | TyKind::MultiPointer(_, _) => Self::Pointer,
            TyKind::Fn(_) => todo!(),
            TyKind::Array(_, _) => todo!(),
            TyKind::Slice(_, _) => todo!(),
            TyKind::Tuple(_) => todo!(),
            TyKind::Struct(_) => todo!(),
            TyKind::Module(_) => todo!(),
            TyKind::Type(_) => Self::Type,
            TyKind::Infer(_, InferTy::AnyInt) => Self::I32,
            TyKind::Infer(_, InferTy::AnyFloat) => Self::F64,
            TyKind::Infer(_, _) => todo!(),
            _ => panic!("invalid type {}", ty),
        }
    }
}

impl Value {
    pub fn unit() -> Self {
        Value::Aggregate(Aggregate {
            elements: Vec::with_capacity(0),
            ty: TyKind::Unit,
        })
    }

    pub unsafe fn from_type_and_ptr(ty: &TyKind, ptr: RawPointer) -> Self {
        match ty {
            TyKind::Never | TyKind::Unit => Self::unit(),
            TyKind::Bool => Self::Bool(*(ptr as *mut bool)),
            TyKind::Int(ty) => match ty {
                IntTy::I8 => Self::I8(*(ptr as *mut i8)),
                IntTy::I16 => Self::I16(*(ptr as *mut i16)),
                IntTy::I32 => Self::I32(*(ptr as *mut i32)),
                IntTy::I64 => Self::I64(*(ptr as *mut i64)),
                IntTy::Int => Self::Int(*(ptr as *mut isize)),
            },
            TyKind::Uint(ty) => match ty {
                UintTy::U8 => Self::U8(*(ptr as *mut u8)),
                UintTy::U16 => Self::U16(*(ptr as *mut u16)),
                UintTy::U32 => Self::U32(*(ptr as *mut u32)),
                UintTy::U64 => Self::U64(*(ptr as *mut u64)),
                UintTy::Uint => Self::Uint(*(ptr as *mut usize)),
            },
            TyKind::Float(ty) => match ty {
                FloatTy::F16 | FloatTy::F32 => Self::F32(*(ptr as *mut f32)),
                FloatTy::F64 => Self::F64(*(ptr as *mut f64)),
                FloatTy::Float => {
                    if IS_64BIT {
                        Self::F64(*(ptr as *mut f64))
                    } else {
                        Self::F32(*(ptr as *mut f32))
                    }
                }
            },
            TyKind::Pointer(ty, _) | TyKind::MultiPointer(ty, _) => {
                Self::Pointer(Pointer::from_type_and_ptr(ty, *(ptr as *mut RawPointer)))
            }
            TyKind::Fn(_) => todo!(),
            TyKind::Array(inner, size) => Self::Array(Array {
                bytes: ByteSeq::from_raw_parts(ptr as *mut u8, *size * inner.size_of(WORD_SIZE)),
                ty: ty.clone(),
            }),
            TyKind::Slice(_, _) => todo!(),
            TyKind::Tuple(_) => todo!(),
            TyKind::Struct(struct_ty) => {
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
            TyKind::Infer(_, InferTy::AnyInt) => Self::I32(*(ptr as *mut i32)),
            TyKind::Infer(_, InferTy::AnyFloat) => Self::F64(*(ptr as *mut f64)),
            TyKind::Infer(_, _) => todo!(),
            _ => panic!("invalid type {}", ty),
        }
    }

    pub fn get_ty_kind(&self) -> TyKind {
        match self {
            Self::I8(_) => TyKind::Int(IntTy::I8),
            Self::I16(_) => TyKind::Int(IntTy::I16),
            Self::I32(_) => TyKind::Int(IntTy::I32),
            Self::I64(_) => TyKind::Int(IntTy::I64),
            Self::Int(_) => TyKind::Int(IntTy::Int),
            Self::U8(_) => TyKind::Uint(UintTy::U8),
            Self::U16(_) => TyKind::Uint(UintTy::U16),
            Self::U32(_) => TyKind::Uint(UintTy::U32),
            Self::U64(_) => TyKind::Uint(UintTy::U64),
            Self::Uint(_) => TyKind::Uint(UintTy::Uint),
            Self::F32(_) => TyKind::Float(FloatTy::F32),
            Self::F64(_) => TyKind::Float(FloatTy::F64),
            Self::Bool(_) => TyKind::Bool,
            Self::Aggregate(agg) => agg.ty.clone(),
            Self::Array(arr) => arr.ty.clone(),
            Self::Pointer(p) => TyKind::Pointer(Box::new(p.get_ty_kind()), true),
            Self::Func(_) => todo!(),
            _ => panic!(),
        }
    }

    // TODO: Merge Value with ConstValue
    pub fn try_into_const_value(
        self,
        tycx: &mut TyCtx,
        eval_span: Span,
    ) -> Result<ConstValue, &'static str> {
        match self {
            Self::I8(v) => Ok(ConstValue::Int(v as _)),
            Self::I16(v) => Ok(ConstValue::Int(v as _)),
            Self::I32(v) => Ok(ConstValue::Int(v as _)),
            Self::I64(v) => Ok(ConstValue::Int(v)),
            Self::Int(v) => Ok(ConstValue::Int(v as _)),
            Self::U8(v) => Ok(ConstValue::Int(v as _)),
            Self::U16(v) => Ok(ConstValue::Int(v as _)),
            Self::U32(v) => Ok(ConstValue::Int(v as _)),
            Self::U64(v) => Ok(ConstValue::Int(v as _)),
            Self::Uint(v) => Ok(ConstValue::Int(v as _)),
            Self::F32(v) => Ok(ConstValue::Float(v as _)),
            Self::F64(v) => Ok(ConstValue::Float(v)),
            Self::Bool(v) => Ok(ConstValue::Bool(v)),
            Self::Type(t) => Ok(ConstValue::Type(tycx.bound(t, eval_span))),
            Self::Aggregate(_) => todo!(),
            Self::Array(_) => todo!(),
            Self::Pointer(_) => Err("pointer"),
            Self::Func(_) => Err("function"),
            Self::ForeignFunc(_) => Err("function"),
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
            ty: TyKind::str(),
        })
    }
}

impl Pointer {
    pub fn unit() -> Self {
        Pointer::Aggregate(&mut Aggregate {
            elements: Vec::with_capacity(0),
            ty: TyKind::Unit,
        })
    }

    pub fn from_type_and_ptr(ty: &TyKind, ptr: RawPointer) -> Self {
        match ty {
            TyKind::Never | TyKind::Unit => Self::U8(ptr as _),
            TyKind::Bool => Self::Bool(ptr as _),
            TyKind::Int(ty) => match ty {
                IntTy::I8 => Self::I8(ptr as _),
                IntTy::I16 => Self::I16(ptr as _),
                IntTy::I32 => Self::I32(ptr as _),
                IntTy::I64 => Self::I64(ptr as _),
                IntTy::Int => Self::Int(ptr as _),
            },
            TyKind::Uint(ty) => match ty {
                UintTy::U8 => Self::U8(ptr as _),
                UintTy::U16 => Self::U16(ptr as _),
                UintTy::U32 => Self::U32(ptr as _),
                UintTy::U64 => Self::U64(ptr as _),
                UintTy::Uint => Self::Uint(ptr as _),
            },
            TyKind::Float(_) => Self::F64(ptr as _),
            TyKind::Pointer(ty, _) | TyKind::MultiPointer(ty, _) => {
                Self::from_type_and_ptr(ty, ptr)
            }
            TyKind::Fn(_) => todo!(),
            TyKind::Array(inner, size) => {
                let bytes = unsafe {
                    ByteSeq::from_raw_parts(ptr as *mut u8, *size * inner.size_of(WORD_SIZE))
                };

                let array = Box::new(Array {
                    bytes,
                    ty: ty.clone(),
                });

                Self::Array(Box::leak(array) as *mut Array)
            }
            TyKind::Slice(_, _) => todo!(),
            TyKind::Tuple(_) => todo!(),
            TyKind::Struct(_) => todo!(),
            TyKind::Infer(_, InferTy::AnyInt) => Self::I32(ptr as _),
            TyKind::Infer(_, InferTy::AnyFloat) => Self::F64(ptr as _),
            TyKind::Infer(_, _) => todo!(),
            _ => panic!("invalid type {}", ty),
        }
    }

    pub fn get_ty_kind(&self) -> TyKind {
        match self {
            Self::I8(_) => TyKind::Int(IntTy::I8),
            Self::I16(_) => TyKind::Int(IntTy::I16),
            Self::I32(_) => TyKind::Int(IntTy::I32),
            Self::I64(_) => TyKind::Int(IntTy::I64),
            Self::Int(_) => TyKind::Int(IntTy::Int),
            Self::U8(_) => TyKind::Uint(UintTy::U8),
            Self::U16(_) => TyKind::Uint(UintTy::U16),
            Self::U32(_) => TyKind::Uint(UintTy::U32),
            Self::U64(_) => TyKind::Uint(UintTy::U64),
            Self::Uint(_) => TyKind::Uint(UintTy::Uint),
            Self::F32(_) => TyKind::Float(FloatTy::F32),
            Self::F64(_) => TyKind::Float(FloatTy::F64),
            Self::Bool(_) => TyKind::Bool,
            Self::Aggregate(_) => todo!(),
            Self::Pointer(p) => TyKind::Pointer(
                if p.is_null() {
                    Box::new(TyKind::Uint(UintTy::U8))
                } else {
                    Box::new(unsafe { &**p }.get_ty_kind())
                },
                true,
            ),
            Self::Func(_) => todo!(),
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
            (Self::Func(p), Value::Func(v)) => **p = v,
            (Self::ForeignFunc(p), Value::ForeignFunc(v)) => **p = v,
            (Self::Type(p), Value::Type(v)) => **p = v,
            (p, v) => panic!("invalid pair {:?} , {}", p, v.to_string()),
        }
    }
}

const MAX_CONSECUTIVE_VALUES: isize = 4;

impl ToString for Value {
    fn to_string(&self) -> String {
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
            Value::Aggregate(v) => {
                let extra_values = v.elements.len() as isize - MAX_CONSECUTIVE_VALUES;

                format!(
                    "{{{}{}}}",
                    v.elements
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
            Value::Array(v) => {
                let bytes = &v.bytes;

                let ty = v.ty.inner();
                let element_size = ty.size_of(WORD_SIZE);
                let size = (bytes.len() / element_size) as isize;

                let mut elements = vec![];

                for i in 0..size.min(MAX_CONSECUTIVE_VALUES) {
                    let el = bytes.offset(element_size * (i as usize)).get_value(ty);
                    elements.push(el.to_string());
                }

                let extra_values = size - MAX_CONSECUTIVE_VALUES;

                format!(
                    "[{}{}]",
                    elements.join(", "),
                    if extra_values > 0 {
                        format!(", +{} more", extra_values)
                    } else {
                        "".to_string()
                    }
                )
            }
            Value::Pointer(p) => p.to_string(),
            Value::Func(func) => format!("fn {}", func.name),
            Value::ForeignFunc(func) => format!("foreign fn {}", func.name),
            Value::Type(ty) => format!("type {}", ty),
        }
    }
}

impl ToString for Pointer {
    fn to_string(&self) -> String {
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
                    Pointer::Aggregate(v) => {
                        let extra_values = (**v).elements.len() as isize - MAX_CONSECUTIVE_VALUES;

                        format!(
                            "{{{}{}}}",
                            (**v)
                                .elements
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
                    Pointer::Array(v) => {
                        let bytes = &(**v).bytes;

                        let ty = (**v).ty.inner();
                        let element_size = ty.size_of(WORD_SIZE);
                        let size = (bytes.len() / element_size) as isize;

                        let mut elements = vec![];

                        for i in 0..size.min(MAX_CONSECUTIVE_VALUES) {
                            let el = bytes.offset(element_size * (i as usize)).get_value(ty);
                            elements.push(el.to_string());
                        }

                        let extra_values = size - MAX_CONSECUTIVE_VALUES;

                        format!(
                            "[{}{}]",
                            elements.join(", "),
                            if extra_values > 0 {
                                format!(", +{} more", extra_values)
                            } else {
                                "".to_string()
                            }
                        )
                    }
                    Pointer::Pointer(p) => (**p).to_string(),
                    Pointer::Func(func) => format!("fn {}", (**func).name),
                    Pointer::ForeignFunc(func) => format!("foreign fn {}", (**func).name),
                    Pointer::Type(ty) => format!("type {}", (**ty)),
                }
            }
        };

        format!("ptr {}", value)
    }
}