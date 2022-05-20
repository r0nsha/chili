use chili_ast::ty::{PartialStructTy, TyKind};
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InferenceValue {
    Bound(TyKind),
    AnyInt,
    AnyFloat,
    PartialTuple(Vec<TyKind>),
    PartialStruct(PartialStructTy),
    Unbound,
}

impl fmt::Display for InferenceValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                InferenceValue::Bound(t) => t.to_string(),
                InferenceValue::AnyInt => "[anyint]".to_string(),
                InferenceValue::AnyFloat => "[anyfloat]".to_string(),
                InferenceValue::PartialTuple(elements) =>
                    TyKind::Tuple(elements.clone()).to_string(),
                InferenceValue::PartialStruct(partial) => partial.to_string(),
                InferenceValue::Unbound => "unbound".to_string(),
            }
        )
    }
}
