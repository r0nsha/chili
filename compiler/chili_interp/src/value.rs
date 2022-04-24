use std::fmt::Display;

use chili_ast::ty::TyKind;

use crate::instruction::Bytecode;

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Tuple(Vec<Value>),
    Func(Func),
    Slice(FatPtr), // ForeignFunc(ForeignFunc),
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: String,
    pub param_count: usize,
    pub code: Bytecode,
}

#[derive(Debug, Clone)]
pub struct FatPtr {
    pub ty: TyKind,
    pub ptr: *mut u8,
    pub len: usize,
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
                Value::Func(func) => format!("fn {}", func.name),
                Value::Slice(fp) => format!("slice({}, {}, {})", fp.ty, unsafe { *fp.ptr }, fp.len),
                // Value::ForeignFunc(func) => format!("foreign(\"{}\") func {}", func.lib, func.name),
            }
        )
    }
}
