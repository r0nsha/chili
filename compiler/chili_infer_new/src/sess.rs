use chili_ast::ty::Ty;
use core::fmt;
use std::hash::Hash;

pub(crate) struct InferSess {
    type_bindings: Vec<TyBinding>,
}

impl InferSess {
    pub(crate) fn new() -> Self {
        Self {
            type_bindings: Default::default(),
        }
    }

    pub(crate) fn new_variable(&mut self) -> TyVar {
        let var = TyVar(self.type_bindings.len() as _);
        self.type_bindings.push(TyBinding::Unbound);
        var
    }

    pub(crate) fn new_bound_variable(&mut self, ty: Ty) -> TyVar {
        let var = TyVar(self.type_bindings.len() as _);
        self.type_bindings.push(TyBinding::Bound(ty));
        var
    }

    pub(crate) fn find_type_binding(&self, var: TyVar) -> TyBinding {
        match self.type_bindings.get(var.0 as usize) {
            Some(ty) => ty.clone(),
            None => TyBinding::Unbound,
        }
    }

    pub(crate) fn bind(&mut self, var: TyVar, ty: Ty) {
        self.type_bindings[var.0 as usize] = TyBinding::Bound(ty);
    }

    pub(crate) fn print_type_bindings(&mut self) {
        for (i, tb) in self.type_bindings.iter().enumerate() {
            println!("'{} :: {}", i, tb)
        }
    }

    pub(crate) fn is_done(&mut self) -> bool {
        self.type_bindings.iter().all(|t| match t {
            TyBinding::Bound(_) => true,
            TyBinding::Unbound => false,
        })
    }
}

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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum TyBinding {
    Bound(Ty),
    Unbound,
}

impl fmt::Display for TyBinding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TyBinding::Bound(t) => t.to_string(),
                TyBinding::Unbound => "unbound".to_string(),
            }
        )
    }
}
