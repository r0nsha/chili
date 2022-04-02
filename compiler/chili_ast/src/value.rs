use crate::{ast::Literal, ty::Ty};
use strum_macros::Display;

#[derive(Debug, Display, PartialEq, Clone, Copy)]
pub enum Value {
    Type(Ty),
    Bool(bool),
    Int(i64),
    Float(f64),
}

impl Value {
    pub fn is_type(&self) -> bool {
        match self {
            Value::Type(_) => true,
            _ => false,
        }
    }

    pub fn into_type(self) -> Ty {
        match self {
            Value::Type(ty) => ty,
            _ => panic!(),
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Value::Bool(_) => true,
            _ => false,
        }
    }

    pub fn into_bool(self) -> bool {
        match self {
            Value::Bool(b) => b,
            _ => panic!(),
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Value::Int(_) => true,
            _ => false,
        }
    }

    pub fn into_int(self) -> i64 {
        match self {
            Value::Int(i) => i,
            _ => panic!(),
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Value::Float(_) => true,
            _ => false,
        }
    }

    pub fn into_float(self) -> f64 {
        match self {
            Value::Float(f) => f,
            _ => panic!(),
        }
    }

    pub fn into_literal(self) -> Literal {
        match self {
            Value::Type(_) => panic!("unexpected Value::Type"),
            Value::Bool(v) => Literal::Bool(v),
            Value::Int(v) => Literal::Int(v),
            Value::Float(v) => Literal::Float(v),
        }
    }
}
