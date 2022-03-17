use chili_ast::ty::*;
use core::fmt;
use slab::Slab;
use std::{collections::HashMap, hash::Hash};

use crate::normalize::NormalizeTy;

pub struct TyContext {
    type_bindings: Slab<TyBinding>,
    primitive_types: HashMap<TyKind, Ty>,
    str_ty: Ty,
}

impl TyContext {
    pub fn new() -> Self {
        let mut inst = Self {
            type_bindings: Default::default(),
            primitive_types: Default::default(),
            str_ty: Default::default(),
        };
        inst.create_primitive_types();
        inst.str_ty = inst.new_bound_variable(TyKind::str());
        inst
    }

    #[inline]
    fn insert(&mut self, binding: TyBinding) -> Ty {
        Ty(self.type_bindings.insert(binding))
    }

    #[inline]
    pub fn new_variable(&mut self) -> Ty {
        self.insert(TyBinding::Unbound)
    }

    #[inline]
    pub fn new_bound_variable(&mut self, kind: TyKind) -> Ty {
        match kind {
            TyKind::Var(ty) | TyKind::AnyInt(ty) | TyKind::AnyFloat(ty) => ty,
            _ => self.insert(TyBinding::Bound(kind)),
        }
    }

    #[inline]
    pub fn find_type_binding(&self, var: Ty) -> TyBinding {
        match self.type_bindings.get(var.0 as usize) {
            Some(ty) => ty.clone(),
            None => TyBinding::Unbound,
        }
    }

    #[inline]
    pub fn bind(&mut self, var: Ty, ty: TyKind) {
        self.type_bindings[var.0 as usize] = TyBinding::Bound(ty);
    }

    #[inline]
    pub fn primitive(&self, kind: TyKind) -> Ty {
        self.primitive_types[&kind]
    }

    #[inline]
    pub fn str_primitive(&self) -> Ty {
        self.str_ty
    }

    #[inline]
    pub fn ty_kind(&self, ty: Ty) -> TyKind {
        ty.normalize(self)
    }

    pub fn print_type_bindings(&mut self) {
        for (i, tb) in self.type_bindings.iter() {
            println!("'{} :: {}", i, tb)
        }
    }

    fn create_primitive_types(&mut self) {
        let mut create = |kind: TyKind| {
            let ty = self.new_bound_variable(kind.clone());
            self.primitive_types.insert(kind, ty);
        };

        create(TyKind::Unit);

        create(TyKind::Bool);

        create(TyKind::Int(IntTy::I8));
        create(TyKind::Int(IntTy::I16));
        create(TyKind::Int(IntTy::I32));
        create(TyKind::Int(IntTy::I64));
        create(TyKind::Int(IntTy::Isize));

        create(TyKind::UInt(UIntTy::U8));
        create(TyKind::UInt(UIntTy::U16));
        create(TyKind::UInt(UIntTy::U32));
        create(TyKind::UInt(UIntTy::U64));
        create(TyKind::UInt(UIntTy::Usize));

        create(TyKind::Float(FloatTy::F16));
        create(TyKind::Float(FloatTy::F32));
        create(TyKind::Float(FloatTy::F64));
        create(TyKind::Float(FloatTy::Fsize));
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TyBinding {
    Bound(TyKind),
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
