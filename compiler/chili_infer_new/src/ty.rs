use std::fmt::Display;

use ena::unify::UnifyKey;

use crate::constraint::Constraint;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub(crate) struct Ty(u32);

impl From<u32> for Ty {
    fn from(index: u32) -> Ty {
        Ty(index)
    }
}

impl Ty {
    #[inline(always)]
    pub(crate) fn index(self) -> u32 {
        self.0
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[t:{}]", self.index())
    }
}

impl UnifyKey for Ty {
    type Value = Constraint;

    #[inline(always)]
    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> Self {
        u.into()
    }

    fn tag() -> &'static str {
        "Ty"
    }
}
