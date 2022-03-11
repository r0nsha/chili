use std::fmt::Display;

use chili_ast::ty::TyKind;
use ena::unify::UnifyKey;

use crate::constraint::Constraint;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub(crate) struct TyVar(u32);

impl From<u32> for TyVar {
    fn from(index: u32) -> TyVar {
        TyVar(index)
    }
}

impl Into<TyKind> for TyVar {
    fn into(self) -> TyKind {
        TyKind::Var(self.index())
    }
}

impl TyVar {
    #[inline(always)]
    pub(crate) fn index(&self) -> u32 {
        self.0
    }
}

impl Display for TyVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[t:{}]", self.index())
    }
}

impl UnifyKey for TyVar {
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
