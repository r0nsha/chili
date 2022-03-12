use crate::{ast::LiteralKind, ty::TyKind};
use strum_macros::Display;

#[derive(Debug, Display, PartialEq, Clone)]
pub enum Value {
    Type(TyKind),
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

    pub fn into_type(self) -> TyKind {
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
}

impl Into<LiteralKind> for Value {
    fn into(self) -> LiteralKind {
        match self {
            Value::Type(_) => panic!(),
            Value::Bool(v) => LiteralKind::Bool(v),
            Value::Int(v) => LiteralKind::Int(v),
            Value::Float(v) => LiteralKind::Float(v),
        }
    }
}
