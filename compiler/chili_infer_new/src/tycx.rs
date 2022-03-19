use chili_ast::ty::*;
use core::fmt;
use slab::Slab;
use std::hash::Hash;

use crate::normalize::NormalizeTy;

pub struct TyCtx {
    type_bindings: Slab<TyBinding>,
    pub common_types: CommonTypes,
}

impl TyCtx {
    pub fn new() -> Self {
        let mut type_bindings = Default::default();
        let common_types = CommonTypes::new(&mut type_bindings);
        Self {
            type_bindings,
            common_types,
        }
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
        self.insert(TyBinding::Bound(kind))
    }

    #[inline]
    pub fn get_binding(&self, var: Ty) -> &TyBinding {
        match self.type_bindings.get(var.0 as usize) {
            Some(ty) => ty,
            None => &TyBinding::Unbound,
        }
    }

    #[inline]
    pub fn ty_kind(&self, ty: Ty) -> TyKind {
        ty.normalize(self)
    }

    #[inline]
    pub fn bind(&mut self, var: Ty, ty: TyKind) {
        self.type_bindings[var.0] = TyBinding::Bound(ty);
    }

    pub fn print_type_bindings(&mut self) {
        for (i, tb) in self.type_bindings.iter() {
            println!("'{} :: {}", i, tb)
        }
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
    pub fn new(bindings: &mut Slab<TyBinding>) -> Self {
        use TyKind::*;

        let mut mk = |kind| Ty(bindings.insert(TyBinding::Bound(kind)));

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
