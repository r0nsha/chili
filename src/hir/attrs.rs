use crate::span::Span;
use std::{
    collections::{hash_map, HashMap},
    fmt::Display,
};

use super::const_value::ConstValue;

#[derive(Debug, PartialEq, Clone)]
pub struct Attrs {
    inner: HashMap<AttrKind, Attr>,
}

impl Attrs {
    pub fn new() -> Self {
        Self { inner: HashMap::new() }
    }

    pub fn insert(&mut self, attr: Attr) -> Option<Attr> {
        self.inner.insert(attr.kind, attr)
    }

    pub fn get(&self, kind: AttrKind) -> Option<&Attr> {
        self.inner.get(&kind)
    }

    pub fn has(&self, kind: AttrKind) -> bool {
        self.inner.contains_key(&kind)
    }

    pub fn iter(&self) -> hash_map::Iter<AttrKind, Attr> {
        self.inner.iter()
    }

    #[allow(unused)]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    #[allow(unused)]
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
    Lib,
    Dylib,
    LinkName,
    TrackCaller,
}

pub const ATTR_NAME_INTRINSIC: &str = "intrinsic";
pub const ATTR_NAME_LIB: &str = "lib";
pub const ATTR_NAME_DYLIB: &str = "dylib";
pub const ATTR_NAME_LINK_NAME: &str = "link_name";
pub const ATTR_NAME_TRACK_CALLER: &str = "track_caller";

impl TryFrom<&str> for AttrKind {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            ATTR_NAME_INTRINSIC => Ok(AttrKind::Intrinsic),
            ATTR_NAME_LIB => Ok(AttrKind::Lib),
            ATTR_NAME_DYLIB => Ok(AttrKind::Dylib),
            ATTR_NAME_LINK_NAME => Ok(AttrKind::LinkName),
            ATTR_NAME_TRACK_CALLER => Ok(AttrKind::TrackCaller),
            _ => Err(()),
        }
    }
}

impl Display for AttrKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                AttrKind::Intrinsic => ATTR_NAME_INTRINSIC,
                AttrKind::Lib => ATTR_NAME_LIB,
                AttrKind::Dylib => ATTR_NAME_DYLIB,
                AttrKind::LinkName => ATTR_NAME_LINK_NAME,
                AttrKind::TrackCaller => ATTR_NAME_TRACK_CALLER,
            }
        )
    }
}
