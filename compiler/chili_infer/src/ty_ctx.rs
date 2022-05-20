use chili_ast::ty::*;
use chili_span::Span;
use slab::Slab;

use crate::{inference_value::InferenceValue, normalize::NormalizeTy};

pub struct TyCtx {
    bindings: Slab<InferenceValue>,
    binding_spans: Slab<Option<Span>>,
    pub common_types: CommonTypes,
}

impl Default for TyCtx {
    fn default() -> Self {
        let mut bindings = Default::default();
        let mut binding_spans = Default::default();
        let common_types = CommonTypes::new(&mut bindings, &mut binding_spans);
        Self {
            bindings,
            binding_spans,
            common_types,
        }
    }
}

impl TyCtx {
    #[inline]
    fn insert(&mut self, binding: InferenceValue, span: Option<Span>) -> Ty {
        self.binding_spans.insert(span);
        Ty(self.bindings.insert(binding))
    }

    #[inline]
    pub fn var(&mut self, span: Span) -> Ty {
        self.insert(InferenceValue::Unbound, Some(span))
    }

    #[inline]
    pub fn anyint(&mut self, span: Span) -> Ty {
        self.insert(InferenceValue::AnyInt, Some(span))
    }

    #[inline]
    pub fn anyfloat(&mut self, span: Span) -> Ty {
        self.insert(InferenceValue::AnyFloat, Some(span))
    }

    #[inline]
    pub fn partial_tuple(&mut self, elements: Vec<TyKind>, span: Span) -> Ty {
        self.insert(InferenceValue::PartialTuple(elements), Some(span))
    }

    #[inline]
    pub fn partial_struct(&mut self, partial_struct: PartialStructTy, span: Span) -> Ty {
        self.insert(InferenceValue::PartialStruct(partial_struct), Some(span))
    }

    #[inline]
    pub fn bound_builtin(&mut self, kind: TyKind) -> Ty {
        self.insert(InferenceValue::Bound(kind), None)
    }

    #[inline]
    pub fn bound(&mut self, kind: TyKind, span: Span) -> Ty {
        match kind {
            TyKind::Var(ty) => ty,
            _ => self.insert(InferenceValue::Bound(kind), Some(span)),
        }
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
    pub fn ty_span(&self, ty: Ty) -> Option<Span> {
        self.binding_spans.get(ty.0).cloned().flatten()
    }

    #[inline]
    pub fn bind_ty(&mut self, var: Ty, ty: TyKind) {
        self.bind_value(var, InferenceValue::Bound(ty))
    }

    #[inline]
    pub fn bind_value(&mut self, var: Ty, value: InferenceValue) {
        self.bindings[var.0] = value;
    }

    pub fn print_type_bindings(&self) {
        for (i, tb) in self.bindings.iter() {
            println!("'{} :: {}", i, tb)
        }
    }

    pub fn print_ty(&self, ty: Ty) {
        println!("'{} :: {}", ty.0, self.bindings[ty.0]);
    }
}

pub struct CommonTypes {
    pub unknown: Ty,
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
    pub fn new(
        bindings: &mut Slab<InferenceValue>,
        binding_spans: &mut Slab<Option<Span>>,
    ) -> Self {
        use TyKind::*;

        let mut mk = |kind| {
            binding_spans.insert(None);
            Ty(bindings.insert(InferenceValue::Bound(kind)))
        };

        Self {
            unknown: mk(Unknown),
            unit: mk(Unit),
            bool: mk(Bool),
            i8: mk(Int(IntTy::I8)),
            i16: mk(Int(IntTy::I16)),
            i32: mk(Int(IntTy::I32)),
            i64: mk(Int(IntTy::I64)),
            int: mk(Int(IntTy::Int)),
            u8: mk(Uint(UintTy::U8)),
            u16: mk(Uint(UintTy::U16)),
            u32: mk(Uint(UintTy::U32)),
            u64: mk(Uint(UintTy::U64)),
            uint: mk(Uint(UintTy::Uint)),
            f16: mk(Float(FloatTy::F16)),
            f32: mk(Float(FloatTy::F32)),
            f64: mk(Float(FloatTy::F64)),
            float: mk(Float(FloatTy::Float)),
            str: mk(TyKind::str()),
            never: mk(Never),
        }
    }
}
