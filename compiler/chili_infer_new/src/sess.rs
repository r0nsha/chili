use std::collections::HashSet;

use chili_ast::ty::*;
use ena::unify::InPlaceUnificationTable;

use crate::{constraint::Constraint, ty::TyVar};

pub(crate) struct InferSess {
    pub(crate) table: InPlaceUnificationTable<TyVar>,

    // the set of type variables which are allowed to default to `()` if ambiguous
    pub(crate) default_unit: HashSet<TyVar>,

    // the target machine's word size
    pub(crate) word_size: usize,
}

impl InferSess {
    pub(crate) fn new(word_size: usize) -> Self {
        Self {
            table: Default::default(),
            default_unit: Default::default(),
            word_size,
        }
    }

    #[inline]
    pub(crate) fn get_table_mut(&mut self) -> &mut InPlaceUnificationTable<TyVar> {
        &mut self.table
    }

    #[inline]
    pub(crate) fn new_key(&mut self, value: Constraint) -> TyVar {
        self.table.new_key(value)
    }

    #[inline]
    pub(crate) fn fresh_type_var(&mut self) -> TyVar {
        self.new_key(Constraint::Unbound)
    }

    #[inline]
    pub(crate) fn fresh_bound_type_var(&mut self, ty: TyKind) -> TyVar {
        self.new_key(ty.into())
    }

    #[inline]
    pub(crate) fn value_of(&mut self, var: TyVar) -> Constraint {
        self.table.probe_value(var)
    }

    #[inline]
    pub(crate) fn ty_of(&mut self, var: TyVar) -> TyKind {
        match self.value_of(var) {
            Constraint::Bound(ty) => ty,
            _ => TyKind::Var(var.index()),
        }
    }

    pub(crate) fn is_integer(&mut self, ty: &TyKind) -> bool {
        match ty {
            TyKind::Int(_) => true,
            TyKind::Var(var) => match self.value_of(TyVar::from(*var)) {
                Constraint::Bound(ty) => self.is_integer(&ty),
                Constraint::AnyInt => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub(crate) fn is_any_integer(&mut self, ty: &TyKind) -> bool {
        match ty {
            TyKind::Int(_) | TyKind::UInt(_) => true,
            TyKind::Var(var) => match self.value_of(TyVar::from(*var)) {
                Constraint::Bound(ty) => self.is_any_integer(&ty),
                Constraint::AnyInt => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub(crate) fn is_float(&mut self, ty: &TyKind) -> bool {
        match ty {
            TyKind::Float(_) => true,
            TyKind::Var(var) => match self.value_of(TyVar::from(*var)) {
                Constraint::Bound(ty) => self.is_float(&ty),
                Constraint::Float => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub(crate) fn is_number(&mut self, ty: &TyKind) -> bool {
        match ty {
            TyKind::Int(_) | TyKind::UInt(_) | TyKind::Float(_) => true,
            TyKind::Var(var) => match self.value_of(TyVar::from(*var)) {
                Constraint::Bound(ty) => self.is_number(&ty),
                Constraint::AnyInt | Constraint::Float => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub(crate) fn is_untyped_integer(&mut self, ty: &TyKind) -> bool {
        match ty {
            TyKind::Var(var) => match self.value_of(TyVar::from(*var)) {
                Constraint::AnyInt => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub(crate) fn is_untyped_float(&mut self, ty: &TyKind) -> bool {
        match ty {
            TyKind::Var(var) => match self.value_of(TyVar::from(*var)) {
                Constraint::Float => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub(crate) fn is_untyped_number(&mut self, ty: &TyKind) -> bool {
        match ty {
            TyKind::Var(var) => match self.value_of(TyVar::from(*var)) {
                Constraint::AnyInt | Constraint::Float => true,
                _ => false,
            },
            _ => false,
        }
    }
}
