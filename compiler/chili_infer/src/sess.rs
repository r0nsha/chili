use chili_ast::ty::*;
use ena::unify::{InPlaceUnificationTable, UnifyKey};
use std::fmt::Display;

pub struct InferSess {
    pub(crate) table: InPlaceUnificationTable<TyVar>,
    pub(crate) word_size: usize,
}

impl InferSess {
    pub fn new(word_size: usize) -> Self {
        Self {
            table: InPlaceUnificationTable::new(),
            word_size,
        }
    }

    #[inline]
    pub fn get_table_mut(&mut self) -> &mut InPlaceUnificationTable<TyVar> {
        &mut self.table
    }

    #[inline]
    pub fn new_key(&mut self, value: InferValue) -> TyVar {
        self.table.new_key(value)
    }

    #[inline]
    pub fn fresh_type_var(&mut self) -> TyVar {
        self.new_key(InferValue::Unbound)
    }

    #[inline]
    pub fn fresh_bound_type_var(&mut self, ty: TyKind) -> TyVar {
        self.new_key(ty.into())
    }

    #[inline]
    pub fn value_of(&mut self, var: TyVar) -> InferValue {
        self.table.probe_value(var)
    }

    #[inline]
    pub fn ty_of(&mut self, var: TyVar) -> TyKind {
        match self.value_of(var) {
            InferValue::Bound(ty) => ty,
            _ => TyKind::Var(var.index()),
        }
    }

    pub fn is_integer(&mut self, ty: &TyKind) -> bool {
        match ty {
            TyKind::Int(_) => true,
            TyKind::Var(var) => match self.value_of(TyVar::from(*var)) {
                InferValue::Bound(ty) => self.is_integer(&ty),
                InferValue::UntypedInt => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_any_integer(&mut self, ty: &TyKind) -> bool {
        match ty {
            TyKind::Int(_) | TyKind::UInt(_) => true,
            TyKind::Var(var) => match self.value_of(TyVar::from(*var)) {
                InferValue::Bound(ty) => self.is_any_integer(&ty),
                InferValue::UntypedInt => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_float(&mut self, ty: &TyKind) -> bool {
        match ty {
            TyKind::Float(_) => true,
            TyKind::Var(var) => match self.value_of(TyVar::from(*var)) {
                InferValue::Bound(ty) => self.is_float(&ty),
                InferValue::UntypedFloat => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_number(&mut self, ty: &TyKind) -> bool {
        match ty {
            TyKind::Int(_) | TyKind::UInt(_) | TyKind::Float(_) => true,
            TyKind::Var(var) => match self.value_of(TyVar::from(*var)) {
                InferValue::Bound(ty) => self.is_number(&ty),
                InferValue::UntypedInt | InferValue::UntypedFloat => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_untyped_integer(&mut self, ty: &TyKind) -> bool {
        match ty {
            TyKind::Var(var) => match self.value_of(TyVar::from(*var)) {
                InferValue::UntypedInt => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_untyped_float(&mut self, ty: &TyKind) -> bool {
        match ty {
            TyKind::Var(var) => match self.value_of(TyVar::from(*var)) {
                InferValue::UntypedFloat => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_untyped_number(&mut self, ty: &TyKind) -> bool {
        match ty {
            TyKind::Var(var) => match self.value_of(TyVar::from(*var)) {
                InferValue::UntypedInt | InferValue::UntypedFloat => true,
                _ => false,
            },
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct TyVar(u32);

impl From<u32> for TyVar {
    fn from(index: u32) -> TyVar {
        TyVar(index)
    }
}

impl TyVar {
    #[inline(always)]
    pub fn index(self) -> u32 {
        self.0
    }
}

impl Display for TyVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "v{}", self.index())
    }
}

impl UnifyKey for TyVar {
    type Value = InferValue;

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

#[derive(Clone, Debug, PartialEq)]
pub enum InferValue {
    Bound(TyKind),
    UntypedInt,
    UntypedFloat,
    UntypedNil,
    Unbound,
}

impl From<TyKind> for InferValue {
    fn from(ty: TyKind) -> Self {
        InferValue::Bound(ty)
    }
}

impl From<InferValue> for TyKind {
    fn from(value: InferValue) -> Self {
        match value {
            InferValue::Bound(ty) => ty,
            InferValue::UntypedInt => TyKind::Int(IntTy::default()),
            InferValue::UntypedFloat => TyKind::Float(FloatTy::default()),
            InferValue::UntypedNil => TyKind::raw_pointer(true),
            InferValue::Unbound => panic!("expected type, found InferenceValue::Unbound"),
        }
    }
}

impl From<TyVar> for TyKind {
    fn from(var: TyVar) -> Self {
        TyKind::Var(var.index())
    }
}
