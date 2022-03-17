use chili_ast::ty::*;
use core::fmt;
use std::hash::Hash;

pub struct TyContext {
    type_bindings: Vec<TyBinding>,
}

impl TyContext {
    pub fn new() -> Self {
        Self {
            type_bindings: Default::default(),
        }
    }

    pub fn new_variable(&mut self) -> TyVar {
        let var = TyVar(self.type_bindings.len() as _);
        self.type_bindings.push(TyBinding::Unbound);
        var
    }

    pub fn new_bound_variable(&mut self, ty: Ty) -> TyVar {
        let var = TyVar(self.type_bindings.len() as _);
        self.type_bindings.push(TyBinding::Bound(ty));
        var
    }

    pub fn find_type_binding(&self, var: TyVar) -> TyBinding {
        match self.type_bindings.get(var.0 as usize) {
            Some(ty) => ty.clone(),
            None => TyBinding::Unbound,
        }
    }

    pub fn bind(&mut self, var: TyVar, ty: Ty) {
        self.type_bindings[var.0 as usize] = TyBinding::Bound(ty);
    }

    pub fn print_type_bindings(&mut self) {
        for (i, tb) in self.type_bindings.iter().enumerate() {
            println!("'{} :: {}", i, tb)
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TyBinding {
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
