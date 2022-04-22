use chili_ast::ty::TyKind;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum InferenceValue {
    Bound(TyKind),
    AnyInt,
    AnyFloat,
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
                InferenceValue::Unbound => "unbound".to_string(),
            }
        )
    }
}
