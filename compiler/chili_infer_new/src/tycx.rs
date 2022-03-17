use chili_ast::ty::*;
use core::fmt;
use slab::Slab;
use std::hash::Hash;

pub struct TyContext {
    type_bindings: Slab<TyBinding>,
}

impl TyContext {
    pub fn new() -> Self {
        Self {
            type_bindings: Default::default(),
        }
    }

    #[inline]
    fn insert(&mut self, binding: TyBinding) -> TyVar {
        TyVar(self.type_bindings.insert(binding))
    }

    #[inline]
    pub fn new_variable(&mut self) -> TyVar {
        self.insert(TyBinding::Unbound)
    }

    #[inline]
    pub fn new_bound_variable(&mut self, ty: Ty) -> TyVar {
        self.insert(TyBinding::Bound(ty))
    }

    #[inline]
    pub fn find_type_binding(&self, var: TyVar) -> TyBinding {
        match self.type_bindings.get(var.0 as usize) {
            Some(ty) => ty.clone(),
            None => TyBinding::Unbound,
        }
    }

    #[inline]
    pub fn bind(&mut self, var: TyVar, ty: Ty) {
        self.type_bindings[var.0 as usize] = TyBinding::Bound(ty);
    }

    pub fn print_type_bindings(&mut self) {
        for (i, tb) in self.type_bindings.iter() {
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
