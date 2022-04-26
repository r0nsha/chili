use crate::instruction::Bytecode;
use chili_ast::ty::{InferTy, TyKind};
use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Value {
    // TODO: Have all int values instead of only `Int`
    Int(i64),
    // TODO: Have all float values instead of only `Float`
    Float(f64),
    Bool(bool),
    Tuple(Vec<Value>),
    Ptr(TyKind, *mut u8),
    ValuePtr(*mut Value),
    Slice(Slice),
    Func(Func),
    ForeignFunc(ForeignFunc),
}

#[derive(Debug, Clone)]
pub struct Slice {
    pub ty: TyKind,
    pub ptr: *mut u8,
    pub len: usize,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: String,
    pub param_count: usize,
    pub code: Bytecode,
}

#[derive(Debug, Clone)]
pub struct ForeignFunc {
    pub lib_path: String,
    pub name: String,
    pub param_tys: Vec<TyKind>,
    pub ret_ty: TyKind,
    pub variadic: bool,
}

impl Value {
    pub fn unit() -> Self {
        Value::Tuple(vec![])
    }

    pub unsafe fn from_ptr(ty: &TyKind, ptr: *mut u8) -> *mut Self {
        &mut match ty {
            TyKind::Never => todo!(),
            TyKind::Unit => Value::unit(),
            TyKind::Bool => Value::Bool(*(ptr as *mut bool)),
            TyKind::Infer(_, InferTy::AnyInt) | TyKind::Int(_) | TyKind::UInt(_) => {
                Value::Int(*(ptr as *mut i64))
            }
            TyKind::Infer(_, InferTy::AnyFloat) | TyKind::Float(_) => {
                Value::Float(*(ptr as *mut f64))
            }
            TyKind::Pointer(_, _) | TyKind::MultiPointer(_, _) => Value::Ptr(ty.clone(), ptr),
            TyKind::Fn(_) => todo!(),
            TyKind::Array(_, _) => todo!(),
            TyKind::Slice(_, _) => todo!(),
            TyKind::Tuple(_) => todo!(),
            TyKind::Struct(_) => todo!(),
            TyKind::Infer(_, _) => todo!(),
            _ => panic!("invalid type {}", ty),
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(v) => *v,
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Value::Int(v) => format!("int {}", v),
                Value::Float(v) => format!("float {}", v),
                Value::Bool(v) => format!("bool {}", v),
                Value::Tuple(v) => format!(
                    "({})",
                    v.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                Value::Ptr(t, p) => format!("ptr({}) {:?}", t, p),
                Value::ValuePtr(v) => format!("value ptr {}", unsafe { &**v }),
                Value::Slice(slice) => format!("slice({}, {})", slice.ty, slice.len),
                Value::Func(func) => format!("fn {}", func.name),
                Value::ForeignFunc(func) =>
                    format!("foreign(\"{}\") fn {}", func.lib_path, func.name),
            }
        )
    }
}
