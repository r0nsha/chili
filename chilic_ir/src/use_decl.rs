use chilic_span::{Span, Spanned};
use ustr::Ustr;

use crate::{entity::Visibility, module::ModuleInfo};

#[derive(Debug, PartialEq, Clone)]
pub struct UseDecl {
    pub module_info: ModuleInfo,
    pub alias: Ustr,
    pub use_path: UsePath,
    pub visibility: Visibility,
    pub span: Span,
}

impl UseDecl {
    pub fn span_ref(&self) -> &Span {
        if self.use_path.is_empty() {
            &self.span
        } else {
            &self.use_path.last().unwrap().span
        }
    }

    pub fn is_wildcard(&self) -> bool {
        if self.use_path.is_empty() {
            false
        } else {
            self.use_path.last().unwrap().value.is_wildcard()
        }
    }

    pub fn use_path_str(&self) -> String {
        if self.use_path.is_empty() {
            self.module_info.name.to_string()
        } else {
            format!(
                "{}.{}",
                self.module_info.name,
                self.use_path
                    .iter()
                    .map(|p| p.value.to_string())
                    .collect::<Vec<String>>()
                    .join(".")
            )
        }
    }
}

pub type UsePath = Vec<Spanned<UsePathNode>>;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum UsePathNode {
    Symbol(Ustr),
    Wildcard,
}

impl UsePathNode {
    pub fn into_symbol(&self) -> Ustr {
        match self {
            UsePathNode::Symbol(s) => *s,
            _ => panic!(),
        }
    }

    pub fn is_wildcard(&self) -> bool {
        match self {
            UsePathNode::Wildcard => true,
            _ => false,
        }
    }
}

impl ToString for UsePathNode {
    fn to_string(&self) -> String {
        match self {
            UsePathNode::Symbol(s) => s.to_string(),
            UsePathNode::Wildcard => String::from("?"),
        }
    }
}
