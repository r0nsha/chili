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
    pub(crate) fn new_variable(&mut self) -> TyVar {
        self.new_key(Constraint::Unbound)
    }

    #[inline]
    pub(crate) fn new_bound_variable(&mut self, ty: TyKind) -> TyVar {
        todo!()
        // self.new_key(ty.into())
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
}
