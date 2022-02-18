use chilic_ty::*;
use ena::unify::{InPlaceUnificationTable, UnifyKey};
use std::fmt::Display;

pub struct InferenceContext {
    pub(crate) table: InPlaceUnificationTable<TyVar>,
    pub(crate) word_size: usize,
}

impl InferenceContext {
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
    pub fn new_key(&mut self, value: InferenceValue) -> TyVar {
        self.table.new_key(value)
    }

    #[inline]
    pub fn fresh_type_var(&mut self) -> TyVar {
        self.new_key(InferenceValue::Unbound)
    }

    #[inline]
    pub fn fresh_bound_type_var(&mut self, ty: Ty) -> TyVar {
        self.new_key(ty.into())
    }

    #[inline]
    pub fn value_of(&mut self, var: TyVar) -> InferenceValue {
        self.table.probe_value(var)
    }

    #[inline]
    pub fn ty_of(&mut self, var: TyVar) -> Ty {
        match self.value_of(var) {
            InferenceValue::Bound(ty) => ty,
            _ => Ty::Var(var.index()),
        }
    }

    pub fn is_integer(&mut self, ty: &Ty) -> bool {
        match ty {
            Ty::Int(_) => true,
            Ty::Var(var) => match self.value_of(TyVar::from(*var)) {
                InferenceValue::Bound(ty) => self.is_integer(&ty),
                InferenceValue::UntypedInt => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_any_integer(&mut self, ty: &Ty) -> bool {
        match ty {
            Ty::Int(_) | Ty::UInt(_) => true,
            Ty::Var(var) => match self.value_of(TyVar::from(*var)) {
                InferenceValue::Bound(ty) => self.is_any_integer(&ty),
                InferenceValue::UntypedInt => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_float(&mut self, ty: &Ty) -> bool {
        match ty {
            Ty::Float(_) => true,
            Ty::Var(var) => match self.value_of(TyVar::from(*var)) {
                InferenceValue::Bound(ty) => self.is_float(&ty),
                InferenceValue::UntypedFloat => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_number(&mut self, ty: &Ty) -> bool {
        match ty {
            Ty::Int(_) | Ty::UInt(_) | Ty::Float(_) => true,
            Ty::Var(var) => match self.value_of(TyVar::from(*var)) {
                InferenceValue::Bound(ty) => self.is_number(&ty),
                InferenceValue::UntypedInt | InferenceValue::UntypedFloat => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_untyped_integer(&mut self, ty: &Ty) -> bool {
        match ty {
            Ty::Var(var) => match self.value_of(TyVar::from(*var)) {
                InferenceValue::UntypedInt => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_untyped_float(&mut self, ty: &Ty) -> bool {
        match ty {
            Ty::Var(var) => match self.value_of(TyVar::from(*var)) {
                InferenceValue::UntypedFloat => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_untyped_number(&mut self, ty: &Ty) -> bool {
        match ty {
            Ty::Var(var) => match self.value_of(TyVar::from(*var)) {
                InferenceValue::UntypedInt | InferenceValue::UntypedFloat => true,
                _ => false,
            },
            _ => false,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TyVar(u32);

impl From<u32> for TyVar {
    fn from(index: u32) -> TyVar {
        TyVar(index)
    }
}

impl TyVar {
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
    type Value = InferenceValue;

    #[inline(always)]
    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> Self {
        u.into()
    }

    fn tag() -> &'static str {
        "InferenceVar"
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum InferenceValue {
    Bound(Ty),
    UntypedInt,
    UntypedFloat,
    UntypedNil,
    Unbound,
}

impl From<Ty> for InferenceValue {
    fn from(ty: Ty) -> Self {
        InferenceValue::Bound(ty)
    }
}

impl From<InferenceValue> for Ty {
    fn from(value: InferenceValue) -> Self {
        match value {
            InferenceValue::Bound(ty) => ty,
            InferenceValue::UntypedInt => Ty::Int(IntTy::default()),
            InferenceValue::UntypedFloat => Ty::Float(FloatTy::default()),
            // TODO: i probably need to remove `nil`
            InferenceValue::UntypedNil => Ty::raw_pointer(true),
            InferenceValue::Unbound => panic!("expected type, found InferenceValue::Unbound"),
        }
    }
}

impl From<TyVar> for Ty {
    fn from(var: TyVar) -> Self {
        Ty::Var(var.index())
    }
}
