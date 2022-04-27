use crate::{ast::LiteralKind, ty::Ty};
use strum_macros::Display;

#[derive(Debug, Display, PartialEq, Clone, Copy)]
pub enum ConstValue {
    Type(Ty),
    Bool(bool),
    Int(i64),
    Float(f64),
}

impl ConstValue {
    pub fn is_type(&self) -> bool {
        matches!(self, ConstValue::Type(_))
    }

    pub fn into_type(self) -> Ty {
        match self {
            ConstValue::Type(ty) => ty,
            _ => panic!(),
        }
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, ConstValue::Bool(_))
    }

    pub fn into_bool(self) -> bool {
        match self {
            ConstValue::Bool(b) => b,
            _ => panic!(),
        }
    }

    pub fn is_int(&self) -> bool {
        matches!(self, ConstValue::Int(_))
    }

    pub fn into_int(self) -> i64 {
        match self {
            ConstValue::Int(i) => i,
            _ => panic!(),
        }
    }

    pub fn is_float(&self) -> bool {
        matches!(self, ConstValue::Float(_))
    }

    pub fn into_float(self) -> f64 {
        match self {
            ConstValue::Float(f) => f,
            _ => panic!(),
        }
    }

    pub fn into_literal(self) -> LiteralKind {
        match self {
            ConstValue::Type(_) => panic!("unexpected Value::Type"),
            ConstValue::Bool(v) => LiteralKind::Bool(v),
            ConstValue::Int(v) => LiteralKind::Int(v),
            ConstValue::Float(v) => LiteralKind::Float(v),
        }
    }
}
