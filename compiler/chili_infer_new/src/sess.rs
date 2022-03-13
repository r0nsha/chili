use chili_ast::ty::Ty;
use core::fmt;
use std::hash::Hash;

pub(crate) struct InferSess {
    type_bindings: Vec<TypeBinding>,
}

impl InferSess {
    pub(crate) fn new() -> Self {
        Self {
            type_bindings: Default::default(),
        }
    }

    pub(crate) fn new_variable(&mut self) -> TyVar {
        let var = TyVar(self.type_bindings.len() as _);
        self.type_bindings.push(TypeBinding::Unbound);
        var
    }

    pub(crate) fn new_bound_variable(&mut self, ty: Ty) -> TyVar {
        let var = TyVar(self.type_bindings.len() as _);
        self.type_bindings.push(TypeBinding::Bound(ty));
        var
    }

    pub(crate) fn print_type_bindings(&mut self) {
        for (i, tb) in self.type_bindings.iter().enumerate() {
            println!("'{} -> {}", i, tb)
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct TyVar(u32);

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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum TypeBinding {
    Bound(Ty),
    Unbound,
}

impl fmt::Display for TypeBinding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TypeBinding::Bound(t) => t.to_string(),
                TypeBinding::Unbound => "unbound".to_string(),
            }
        )
    }
}
