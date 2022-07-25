use crate::{ast, span::Span};
use std::{collections::HashMap, fmt::Display};

use super::const_value::ConstValue;

#[derive(Debug, PartialEq, Clone)]
pub struct Attrs {
    inner: HashMap<AttrKind, Attr>,
}

impl Attrs {
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    pub fn insert(&mut self, attr: Attr) -> Option<Attr> {
        self.inner.insert(attr.kind, attr)
    }

    pub fn get(&self, key: AttrKind) -> Option<&Attr> {
        self.inner.get(&key)
    }

    #[allow(unused)]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Attr {
    pub kind: AttrKind,
    pub value: ConstValue,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum AttrKind {
    Intrinsic,
}

impl From<ast::AttrKind> for AttrKind {
    fn from(kind: ast::AttrKind) -> Self {
        match kind {
            ast::AttrKind::Intrinsic => AttrKind::Intrinsic,
        }
    }
}

impl Display for AttrKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AttrKind::Intrinsic => write!(f, "intrinsic"),
        }
    }
}
