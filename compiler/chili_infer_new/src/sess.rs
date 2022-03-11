use chili_ast::ty::*;
use ena::unify::InPlaceUnificationTable;

use crate::{constraint::Constraint, ty::Ty};

pub(crate) struct InferSess {
    pub(crate) table: InPlaceUnificationTable<Ty>,
    pub(crate) word_size: usize,
}

impl InferSess {
    pub(crate) fn new(word_size: usize) -> Self {
        Self {
            table: InPlaceUnificationTable::new(),
            word_size,
        }
    }

    #[inline]
    pub(crate) fn get_table_mut(&mut self) -> &mut InPlaceUnificationTable<Ty> {
        &mut self.table
    }

    #[inline]
    pub(crate) fn new_key(&mut self, value: Constraint) -> Ty {
        self.table.new_key(value)
    }

    #[inline]
    pub(crate) fn fresh_type_var(&mut self) -> Ty {
        self.new_key(Constraint::Unbound)
    }

    #[inline]
    pub(crate) fn fresh_bound_type_var(&mut self, ty: TyKind) -> Ty {
        self.new_key(ty.into())
    }

    #[inline]
    pub(crate) fn value_of(&mut self, var: Ty) -> Constraint {
        self.table.probe_value(var)
    }

    #[inline]
    pub(crate) fn ty_of(&mut self, var: Ty) -> TyKind {
        match self.value_of(var) {
            Constraint::Bound(ty) => ty,
            _ => TyKind::Var(var.index()),
        }
    }

    pub(crate) fn is_integer(&mut self, ty: &TyKind) -> bool {
        match ty {
            TyKind::Int(_) => true,
            TyKind::Var(var) => match self.value_of(Ty::from(*var)) {
                Constraint::Bound(ty) => self.is_integer(&ty),
                Constraint::Int => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub(crate) fn is_any_integer(&mut self, ty: &TyKind) -> bool {
        match ty {
            TyKind::Int(_) | TyKind::UInt(_) => true,
            TyKind::Var(var) => match self.value_of(Ty::from(*var)) {
                Constraint::Bound(ty) => self.is_any_integer(&ty),
                Constraint::Int => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub(crate) fn is_float(&mut self, ty: &TyKind) -> bool {
        match ty {
            TyKind::Float(_) => true,
            TyKind::Var(var) => match self.value_of(Ty::from(*var)) {
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
            TyKind::Var(var) => match self.value_of(Ty::from(*var)) {
                Constraint::Bound(ty) => self.is_number(&ty),
                Constraint::Int | Constraint::Float => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub(crate) fn is_untyped_integer(&mut self, ty: &TyKind) -> bool {
        match ty {
            TyKind::Var(var) => match self.value_of(Ty::from(*var)) {
                Constraint::Int => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub(crate) fn is_untyped_float(&mut self, ty: &TyKind) -> bool {
        match ty {
            TyKind::Var(var) => match self.value_of(Ty::from(*var)) {
                Constraint::Float => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub(crate) fn is_untyped_number(&mut self, ty: &TyKind) -> bool {
        match ty {
            TyKind::Var(var) => match self.value_of(Ty::from(*var)) {
                Constraint::Int | Constraint::Float => true,
                _ => false,
            },
            _ => false,
        }
    }
}
