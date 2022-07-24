use super::NameAndSpan;
use crate::span::Span;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub struct Attrs {
    inner: HashMap<AttrKey, Attr>,
}

impl Attrs {
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    pub fn insert(&mut self, attr: Attr) -> Option<Attr> {
        self.inner.insert(attr.key(), attr)
    }

    pub fn get(&self, key: AttrKey) -> Option<&Attr> {
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
    pub name: NameAndSpan,
    pub kind: AttrKind,
    pub span: Span,
}

impl Attr {
    pub fn key(&self) -> AttrKey {
        match &self.kind {
            AttrKind::Intrinsic => AttrKey::Intrinsic,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum AttrKind {
    Intrinsic,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum AttrKey {
    Intrinsic,
}
