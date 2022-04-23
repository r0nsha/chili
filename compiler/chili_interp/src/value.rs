use std::fmt::Display;

use crate::instruction::Bytecode;

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Tuple(Vec<Value>),
    Func(Func),
    // ForeignFunc(ForeignFunc),
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
                // Value::ForeignFunc(func) => format!("foreign(\"{}\") func {}", func.lib, func.name),
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: String,
    pub param_count: usize,
    pub code: Bytecode,
}
