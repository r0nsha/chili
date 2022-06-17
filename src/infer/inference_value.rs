use crate::ast::ty::{PartialStructType, Type};
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InferenceValue {
    Bound(Type),
    AnyInt,
    AnyFloat,
    PartialTuple(Vec<Type>),
    PartialStruct(PartialStructType),
    Unbound,
}

impl fmt::Display for InferenceValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                InferenceValue::Bound(t) => t.to_string(),
                InferenceValue::AnyInt => "[integer]".to_string(),
                InferenceValue::AnyFloat => "[float]".to_string(),
                InferenceValue::PartialTuple(elements) => Type::Tuple(elements.clone()).to_string(),
                InferenceValue::PartialStruct(partial) => partial.to_string(),
                InferenceValue::Unbound => "unbound".to_string(),
            }
        )
    }
}

impl InferenceValue {
    pub fn is_concrete(&self) -> bool {
        match self {
            InferenceValue::Bound(Type::Var(_)) => false,
            InferenceValue::Bound(_) => true,
            _ => false,
        }
    }
}
