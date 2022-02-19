use crate::{entity::Entity, module::ModuleInfo, r#use::Use};
use chilic_span::Span;

pub type Items = Vec<Item>;

#[derive(Debug, PartialEq, Clone)]
pub struct Item {
    pub module_info: ModuleInfo,
    pub kind: ItemKind,
    pub span: Span,
}

impl Item {
    pub fn new(module_info: ModuleInfo, value: ItemKind, span: Span) -> Self {
        Self {
            module_info,
            kind: value,
            span,
        }
    }
}

#[derive(
    strum_macros::IntoStaticStr, strum_macros::Display, Debug, PartialEq, Clone,
)]
pub enum ItemKind {
    Use(Use),
    Entity(Entity),
}
