use crate::instruction::Bytecode;
use chili_ast::ty::TyKind;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Value {
    // TODO: Have all int values instead of only `Int`
    Int(i64),
    // TODO: Have all float values instead of only `Float`
    Float(f64),
    Bool(bool),
    Tuple(Vec<Value>),
    Ptr(*mut u8),
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
                Value::Ptr(p) => format!("ptr {:?}", p),
                Value::Slice(slice) => format!("slice({}, {})", slice.ty, slice.len),
                Value::Func(func) => format!("fn {}", func.name),
                Value::ForeignFunc(func) =>
                    format!("foreign(\"{}\") fn {}", func.lib_path, func.name),
            }
        )
    }
}
