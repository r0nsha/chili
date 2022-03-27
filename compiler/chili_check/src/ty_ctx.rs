use chili_ast::ty::*;
use core::fmt;
use slab::Slab;
use std::hash::Hash;

use crate::normalize::NormalizeTy;

pub struct TyCtx {
    bindings: Slab<InferenceValue>,
    pub common_types: CommonTypes,
}

impl TyCtx {
    pub fn new() -> Self {
        let mut type_bindings = Default::default();
        let common_types = CommonTypes::new(&mut type_bindings);
        Self {
            bindings: type_bindings,
            common_types,
        }
    }

    #[inline]
    fn insert(&mut self, binding: InferenceValue) -> Ty {
        Ty(self.bindings.insert(binding))
    }

    #[inline]
    pub fn var(&mut self) -> Ty {
        self.insert(InferenceValue::Unbound)
    }

    #[inline]
    pub fn anyint(&mut self) -> Ty {
        self.insert(InferenceValue::AnyInt)
    }

    #[inline]
    pub fn anyfloat(&mut self) -> Ty {
        self.insert(InferenceValue::AnyFloat)
    }

    #[inline]
    pub fn bound(&mut self, kind: TyKind) -> Ty {
        self.insert(InferenceValue::Bound(kind))
    }

    #[inline]
    pub fn value_of(&self, var: Ty) -> &InferenceValue {
        match self.bindings.get(var.0) {
            Some(ty) => ty,
            None => &InferenceValue::Unbound,
        }
    }

    #[inline]
    pub fn ty_kind(&self, ty: Ty) -> TyKind {
        ty.normalize(self)
    }

    #[inline]
    pub fn bind(&mut self, var: Ty, ty: TyKind) {
        self.bindings[var.0] = InferenceValue::Bound(ty);
    }

    pub fn print_type_bindings(&self) {
        for (i, tb) in self.bindings.iter() {
            println!("'{} :: {}", i, tb)
        }
    }

    pub fn print_ty(&self, ty: Ty) {
        println!("{} :: {}", ty, self.bindings[ty.0]);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum InferenceValue {
    Bound(TyKind),
    AnyInt,
    AnyFloat,
    Unbound,
}

impl fmt::Display for InferenceValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                InferenceValue::Bound(t) => t.to_string(),
                InferenceValue::AnyInt => "[anyint]".to_string(),
                InferenceValue::AnyFloat => "[anyfloat]".to_string(),
                InferenceValue::Unbound => "unbound".to_string(),
            }
        )
    }
}

pub struct CommonTypes {
    pub unit: Ty,
    pub bool: Ty,
    pub i8: Ty,
    pub i16: Ty,
    pub i32: Ty,
    pub i64: Ty,
    pub int: Ty,
    pub u8: Ty,
    pub u16: Ty,
    pub u32: Ty,
    pub u64: Ty,
    pub uint: Ty,
    pub f16: Ty,
    pub f32: Ty,
    pub f64: Ty,
    pub float: Ty,
    pub str: Ty,
    pub never: Ty,
}

impl CommonTypes {
    pub fn new(bindings: &mut Slab<InferenceValue>) -> Self {
        use TyKind::*;

        let mut mk = |kind| Ty(bindings.insert(InferenceValue::Bound(kind)));

        Self {
            unit: mk(Unit),
            bool: mk(Bool),
            i8: mk(Int(IntTy::I8)),
            i16: mk(Int(IntTy::I16)),
            i32: mk(Int(IntTy::I32)),
            i64: mk(Int(IntTy::I64)),
            int: mk(Int(IntTy::Int)),
            u8: mk(UInt(UIntTy::U8)),
            u16: mk(UInt(UIntTy::U16)),
            u32: mk(UInt(UIntTy::U32)),
            u64: mk(UInt(UIntTy::U64)),
            uint: mk(UInt(UIntTy::UInt)),
            f16: mk(Float(FloatTy::F16)),
            f32: mk(Float(FloatTy::F32)),
            f64: mk(Float(FloatTy::F64)),
            float: mk(Float(FloatTy::Float)),
            str: mk(TyKind::str()),
            never: mk(Never),
        }
    }
}
