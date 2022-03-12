use chili_ast::ty::Ty;
use core::fmt;
use std::hash::Hash;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct TyVar(pub(crate) u32);

impl fmt::Display for TyVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'{}", self.0)
    }
}

impl Into<Ty> for TyVar {
    fn into(self) -> Ty {
        Ty::Var(self.0)
    }
}

impl TyVar {}

pub(crate) struct TyVarGen {
    supply: u32,
}

impl TyVarGen {
    pub(crate) fn new() -> Self {
        Self { supply: 0 }
    }

    pub(crate) fn next(&mut self) -> TyVar {
        let v = TyVar(self.supply);
        self.supply += 1;
        v
    }
}
