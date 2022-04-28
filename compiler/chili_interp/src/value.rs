use crate::instruction::Bytecode;
use chili_ast::ty::{InferTy, IntTy, TyKind, UIntTy};
use std::{fmt::Display, mem};
use ustr::Ustr;

macro_rules! impl_value {
    ($($variant:ident($ty:ty)) , + $(,)?) => {
        #[derive(Debug, Clone)]
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

        #[derive(Debug, Clone)]
        pub enum ValuePtr {
            $(
                $variant(*mut $ty)
            ),+
        }

        impl From<&mut Value> for ValuePtr {
            fn from(value: &mut Value) -> Self {
                match value {
                    $(
                        Value::$variant(v) => ValuePtr::$variant(v as _)
                    ),+
                }
            }
        }

        impl ValuePtr {
            pub fn as_raw(&self) -> *mut u8 {
                match self {
                    $(
                        ValuePtr::$variant(v) => *v as _
                    ),+
                }
            }

            pub fn set(&self, value: Value) {
                match (self, value) {
                    $(
                        (ValuePtr::$variant(ptr), Value::$variant(value)) => unsafe { **ptr = value }
                    ),+,
                    (ptr, value) => panic!("invalid pair {:?} , {}", ptr, value)
                }
            }

            pub unsafe fn deref(&mut self) -> Value {
                match self {
                    $(
                        ValuePtr::$variant(v) => Value::$variant((**v).clone())
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
    UInt(usize),
    F32(f32),
    F64(f64),
    Bool(bool),
    Tuple(Vec<Value>),
    Ptr(ValuePtr),
    Slice(Slice),
    Func(Func),
    ForeignFunc(ForeignFunc),
}

#[derive(Debug, Clone)]
pub struct Slice {
    pub ptr: ValuePtr,
    pub len: usize,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: Ustr,
    pub param_count: usize,
    pub code: Bytecode,
}

#[derive(Debug, Clone)]
pub struct ForeignFunc {
    pub lib_path: Ustr,
    pub name: Ustr,
    pub param_tys: Vec<TyKind>,
    pub ret_ty: TyKind,
    pub variadic: bool,
}

impl Value {
    pub fn unit() -> Self {
        Value::Tuple(vec![])
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(v) => *v,
            _ => false,
        }
    }

    pub unsafe fn from_ptr(ty: &TyKind, ptr: *mut u8) -> Self {
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
            TyKind::UInt(ty) => match ty {
                UIntTy::U8 => Self::U8(*ptr),
                UIntTy::U16 => Self::U16(*(ptr as *mut u16)),
                UIntTy::U32 => Self::U32(*(ptr as *mut u32)),
                UIntTy::U64 => Self::U64(*(ptr as *mut u64)),
                UIntTy::UInt => Self::UInt(*(ptr as *mut usize)),
            },
            TyKind::Float(_) => Self::F64(*(ptr as *mut f64)),
            TyKind::Pointer(_, _) | TyKind::MultiPointer(_, _) => {
                Self::Ptr(ValuePtr::from_ptr(ty, ptr))
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

impl ValuePtr {
    pub fn unit() -> Self {
        // Note (Ron): Leak
        let mut elements = Vec::<Value>::new();
        let ptr = ValuePtr::Tuple(&mut elements as _);
        mem::forget(elements);
        ptr
    }

    pub fn from_ptr(ty: &TyKind, ptr: *mut u8) -> Self {
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
            TyKind::UInt(ty) => match ty {
                UIntTy::U8 => Self::U8(ptr as _),
                UIntTy::U16 => Self::U16(ptr as _),
                UIntTy::U32 => Self::U32(ptr as _),
                UIntTy::U64 => Self::U64(ptr as _),
                UIntTy::UInt => Self::UInt(ptr as _),
            },
            TyKind::Float(_) => Self::F64(ptr as _),
            TyKind::Pointer(_, _) | TyKind::MultiPointer(_, _) => Self::Ptr(ptr as _),
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
                Value::UInt(v) => format!("uint {}", v),
                Value::F32(v) => format!("f32 {}", v),
                Value::F64(v) => format!("f64 {}", v),
                Value::Bool(v) => format!("bool {}", v),
                Value::Tuple(v) => format!(
                    "({})",
                    v.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                Value::Ptr(p) => format!("ptr {:?}", p.as_raw()),
                Value::Slice(slice) => format!("slice({:?}, {})", slice.ptr, slice.len),
                Value::Func(func) => format!("fn {}", func.name),
                Value::ForeignFunc(func) => format!("foreign fn {}", func.name),
            }
        )
    }
}
