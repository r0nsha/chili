use super::display::DisplayType;
use crate::types::Type;

#[derive(Clone, Debug, PartialEq)]
pub enum InferenceValue {
    Bound(Type),
    AnyInt,
    AnyFloat,
    Unbound,
}

impl DisplayType for InferenceValue {
    fn display(&self, tcx: &super::type_ctx::TypeCtx) -> String {
        match self {
            InferenceValue::Bound(t) => t.display(tcx),
            InferenceValue::AnyInt => "[integer]".to_string(),
            InferenceValue::AnyFloat => "[float]".to_string(),
            InferenceValue::Unbound => "unbound".to_string(),
        }
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
