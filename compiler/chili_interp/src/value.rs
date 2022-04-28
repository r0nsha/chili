use crate::{instruction::CompiledCode, IS_64BIT};
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

        #[derive(Debug, Clone)]
        pub enum Value {
            $(
                $variant($ty)
            ),+
        }

        impl Value {
            paste! {
                $(
                    pub fn [<into_ $variant:snake>](self) -> $ty {
                        match self {
                            Value::$variant(v) => v,
                            _ => panic!("got {}", self)
                        }
                    }
                )+

                $(
                    pub fn [<as_ $variant:snake>](&self) -> &$ty {
                        match self {
                            Value::$variant(v) => v,
                            _ => panic!("got {}", self)
                        }
                    }
                )+

                $(
                    pub fn [<as_ $variant:snake _mut>](&mut self) -> &mut $ty {
                        match self {
                            Value::$variant(v) => v,
                            _ => panic!("got {}", self)
                        }
                    }
                )+
            }
        }

        #[derive(Debug, Clone)]
        pub enum ValuePointer {
            $(
                $variant(*mut $ty)
            ),+
        }

        impl From<&mut Value> for ValuePointer {
            fn from(value: &mut Value) -> Self {
                match value {
                    $(
                        Value::$variant(v) => ValuePointer::$variant(v as _)
                    ),+
                }
            }
        }

        impl ValuePointer {
            pub unsafe fn as_raw(&mut self) -> *mut *mut u8 {
                match self {
                    $(
                        ValuePointer::$variant(ref mut v) => mem::transmute::<&mut *mut _, *mut *mut u8>(v)
                    ),+
                }
            }

            pub fn as_inner_raw(&self) -> *mut u8 {
                match self {
                    $(
                        ValuePointer::$variant(v) => *v as *mut u8
                    ),+
                }
            }

            pub fn write_value(&mut self, value: Value) {
                match (self, value) {
                    $(
                        (ValuePointer::$variant(ptr), Value::$variant(value)) => unsafe { **ptr = value }
                    ),+,
                    (ptr, value) => panic!("invalid pair {:?} , {}", ptr, value)
                }
            }

            pub unsafe fn deref(&mut self) -> Value {
                match self {
                    $(
                        ValuePointer::$variant(v) => Value::$variant((**v).clone())
                    ),+
                }
            }

            pub fn from_kind_and_ptr(kind: ValueKind, ptr: *mut u8) -> Self {
                match kind {
                    $(
                        ValueKind::$variant => ValuePointer::$variant(ptr as _)
                    ),+
                }
            }

            pub unsafe fn print(&self) {
                match self {
                    $(
                        ValuePointer::$variant(v) => println!("{:?}", **v)
                    ),+
                }
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
    Pointer(ValuePointer),
    Slice(Slice),
    Func(Func),
    ForeignFunc(ForeignFunc),
    Type(TyKind)
}

#[derive(Debug, Clone)]
pub struct Slice {
    pub ptr: ValuePointer,
    pub len: usize,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: Ustr,
    pub param_count: usize,
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

impl From<TyKind> for ValueKind {
    fn from(ty: TyKind) -> Self {
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
            TyKind::Type(_) => todo!(),
            TyKind::Infer(_, InferTy::AnyInt) => Self::I32,
            TyKind::Infer(_, InferTy::AnyFloat) => Self::F64,
            TyKind::Infer(_, _) => todo!(),
            _ => panic!("invalid type {}", ty),
        }
    }
}

impl Value {
    pub fn unit() -> Self {
        Value::Aggregate(vec![])
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(v) => *v,
            _ => false,
        }
    }

    pub unsafe fn from_type_and_ptr(ty: &TyKind, ptr: *mut u8) -> Self {
        match ty {
            TyKind::Never | TyKind::Unit => Self::U8(*ptr),
            TyKind::Bool => Self::Bool(*ptr != 0),
            TyKind::Int(ty) => match ty {
                IntTy::I8 => Self::I8(*(ptr as *mut i8)),
                IntTy::I16 => Self::I16(*(ptr as *mut i16)),
                IntTy::I32 => Self::I32(*(ptr as *mut i32)),
                IntTy::I64 => Self::I64(*(ptr as *mut i64)),
                IntTy::Int => Self::Int(*(ptr as *mut isize)),
            },
            TyKind::Uint(ty) => match ty {
                UintTy::U8 => Self::U8(*ptr),
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
                Self::Pointer(ValuePointer::from_type_and_ptr(ty, ptr))
            }
            TyKind::Fn(_) => todo!(),
            TyKind::Array(_, _) => todo!(),
            TyKind::Slice(_, _) => todo!(),
            TyKind::Tuple(_) => todo!(),
            TyKind::Struct(_) => todo!(),
            TyKind::Infer(_, InferTy::AnyInt) => Self::I32(*(ptr as *mut i32)),
            TyKind::Infer(_, InferTy::AnyFloat) => Self::F64(*(ptr as *mut f64)),
            TyKind::Infer(_, _) => todo!(),
            _ => panic!("invalid type {}", ty),
        }
    }
}

impl ValuePointer {
    pub fn unit() -> Self {
        // Note (Ron): Leak
        let mut elements = Vec::<Value>::new();
        let ptr = ValuePointer::Aggregate(&mut elements as _);
        mem::forget(elements);
        ptr
    }

    pub fn from_type_and_ptr(ty: &TyKind, ptr: *mut u8) -> Self {
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
                Value::Aggregate(v) => format!(
                    "({})",
                    v.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                Value::Pointer(p) => format!("ptr {:?}", p),
                Value::Slice(slice) => format!("slice({:?}, {})", slice.ptr, slice.len),
                Value::Func(func) => format!("fn {}", func.name),
                Value::ForeignFunc(func) => format!("foreign fn {}", func.name),
                Value::Type(ty) => format!("type {}", ty),
            }
        )
    }
}
