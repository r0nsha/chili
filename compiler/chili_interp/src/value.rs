use crate::{ffi::RawPointer, instruction::CompiledCode, IS_64BIT};
use bytes::BytesMut;
use chili_ast::ty::{FloatTy, InferTy, IntTy, TyKind, UintTy};
use paste::paste;
use std::{fmt::Display, mem};
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

            pub unsafe fn from_kind_and_ptr(kind: ValueKind, ptr: RawPointer) -> Self {
                match kind {
                    $(
                        ValueKind::$variant => Self::$variant((*(ptr as *mut $ty)).clone())
                    ),+
                }
            }

            paste! {
                $(
                    pub fn [<into_ $variant:snake>](self) -> $ty {
                        match self {
                            Value::$variant(v) => v,
                            _ => panic!("got {}, expected {}", self.to_string(), stringify!($variant))
                        }
                    }
                )+

                $(
                    pub fn [<as_ $variant:snake>](&self) -> &$ty {
                        match self {
                            Value::$variant(v) => v,
                            _ => panic!("got {}, expected {}", self.to_string(), stringify!($variant))
                        }
                    }
                )+

                $(
                    pub fn [<as_ $variant:snake _mut>](&mut self) -> &mut $ty {
                        match self {
                            Value::$variant(v) => v,
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
                        Pointer::$variant(_) => ValueKind::$variant
                    ),+
                }
            }

            pub unsafe fn as_raw(&mut self) -> *mut RawPointer {
                match self {
                    $(
                        Pointer::$variant(ref mut v) => mem::transmute::<&mut *mut _, *mut RawPointer>(v)
                    ),+
                }
            }

            pub fn as_inner_raw(&self) -> RawPointer {
                match self {
                    $(
                        Pointer::$variant(v) => *v as RawPointer
                    ),+
                }
            }

            pub fn write_value(&self, value: Value) {
                match (self, value) {
                    $(
                        (Pointer::$variant(ptr), Value::$variant(value)) => unsafe { ptr.write(value) }
                    ),+,
                    (ptr, value) => panic!("invalid pair {:?} , {}", ptr, value.to_string())
                }
            }

            pub unsafe fn deref_value(&self) -> Value {
                match self {
                    $(
                        Pointer::$variant(v) => Value::$variant((**v).clone())
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
                        Pointer::$variant(v) => println!("{:?}", **v)
                    ),+
                }
            }

            paste! {
                $(
                    pub fn [<into_ $variant:snake>](self) -> *mut $ty {
                        match self {
                            Pointer::$variant(v) => v,
                            _ => panic!("got {}, expected {}", self.to_string(), stringify!($variant))
                        }
                    }
                )+

                $(
                    pub fn [<as_ $variant:snake>](&self) -> &*mut $ty {
                        match self {
                            Pointer::$variant(v) => v,
                            _ => panic!("got {}, expected {}", self.to_string(), stringify!($variant))
                        }
                    }
                )+

                $(
                    pub fn [<as_ $variant:snake _mut>](&mut self) -> &mut *mut $ty {
                        match self {
                            Pointer::$variant(v) => v,
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
    Aggregate(Vec<Value>),
    Pointer(Pointer),
    Func(Func),
    ForeignFunc(ForeignFunc),
    Type(TyKind),
    LazyCompute(CompiledCode)
}

#[derive(Debug, Clone)]
pub struct Slice {
    pub ptr: Pointer,
    pub len: usize,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: Ustr,
    pub param_count: u16,
    pub code: CompiledCode,
}

#[derive(Debug, Clone)]
pub struct ForeignFunc {
    pub lib_path: Ustr,
    pub name: Ustr,
    pub param_tys: Vec<TyKind>,
    pub ret_ty: TyKind,
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
            TyKind::Type(t) => Self::Type,
            TyKind::Infer(_, InferTy::AnyInt) => Self::I32,
            TyKind::Infer(_, InferTy::AnyFloat) => Self::F64,
            TyKind::Infer(_, _) => todo!(),
            _ => panic!("invalid type {}", ty),
        }
    }
}

impl Value {
    pub fn unit() -> Self {
        Value::Aggregate(Vec::with_capacity(0))
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
            TyKind::Array(_, _) => todo!(),
            TyKind::Slice(_, _) => todo!(),
            TyKind::Tuple(_) => todo!(),
            TyKind::Struct(ty) => {
                // TODO: let aggregate = Vec::with_capacity(ty.fields.len())
                // TODO: calculate struct alignment
                // TODO: let curr_offset = 0
                // TODO: for each field
                // TODO:    calculate size of field ty
                // TODO:    raw = offset(ptr, curr_offset)
                // TODO:    value = value::from_raw(field_ty, raw)
                // TODO:    curr_offset += alignment
                // TODO: Self::Aggregate(aggregate)
            }
            TyKind::Infer(_, InferTy::AnyInt) => Self::I32(*(ptr as *mut i32)),
            TyKind::Infer(_, InferTy::AnyFloat) => Self::F64(*(ptr as *mut f64)),
            TyKind::Infer(_, _) => todo!(),
            _ => panic!("invalid type {}", ty),
        }
    }
}

impl Pointer {
    pub fn unit() -> Self {
        Pointer::Aggregate(&mut Vec::with_capacity(0))
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
            TyKind::Array(_, _) => todo!(),
            TyKind::Slice(_, _) => todo!(),
            TyKind::Tuple(_) => todo!(),
            TyKind::Struct(_) => todo!(),
            TyKind::Infer(_, InferTy::AnyInt) => Self::I32(ptr as _),
            TyKind::Infer(_, InferTy::AnyFloat) => Self::F64(ptr as _),
            TyKind::Infer(_, _) => todo!(),
            _ => panic!("invalid type {}", ty),
        }
    }
}

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
                const MAX_VALUES: isize = 4;
                let extra_values = v.len() as isize - MAX_VALUES;

                format!(
                    "{{{}{}}}",
                    v.iter()
                        .take(MAX_VALUES as usize)
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
            Value::Pointer(p) => p.to_string(),
            Value::Func(func) => format!("fn {}", func.name),
            Value::ForeignFunc(func) => format!("foreign fn {}", func.name),
            Value::Type(ty) => format!("type {}", ty),
            Value::LazyCompute(_) => "lazy compute".to_string(),
        }
    }
}

impl ToString for Pointer {
    fn to_string(&self) -> String {
        let value = if self.as_inner_raw() == std::ptr::null_mut() {
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
                        const MAX_VALUES: isize = 4;
                        let extra_values = (**v).len() as isize - MAX_VALUES;

                        format!(
                            "{{{}{}}}",
                            (**v)
                                .iter()
                                .take(MAX_VALUES as usize)
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
                    Pointer::Pointer(p) => (**p).to_string(),
                    Pointer::Func(func) => format!("fn {}", (**func).name),
                    Pointer::ForeignFunc(func) => format!("foreign fn {}", (**func).name),
                    Pointer::Type(ty) => format!("type {}", (**ty)),
                    Pointer::LazyCompute(_) => "lazy compute".to_string(),
                }
            }
        };

        format!("ptr {}", value)
    }
}
