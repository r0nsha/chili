use super::{inference_value::InferenceValue, normalize::Normalize};
use crate::ast::ty::*;
use crate::span::Span;
use slab::Slab;

pub struct TyCtx {
    pub bindings: Slab<InferenceValue>,
    pub binding_spans: Slab<Option<Span>>,
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
    fn insert(&mut self, binding: InferenceValue, span: Option<Span>) -> TypeId {
        self.binding_spans.insert(span);
        TypeId(self.bindings.insert(binding))
    }

    #[inline]
    pub fn var(&mut self, span: Span) -> TypeId {
        self.insert(InferenceValue::Unbound, Some(span))
    }

    #[inline]
    pub fn anyint(&mut self, span: Span) -> TypeId {
        self.insert(InferenceValue::AnyInt, Some(span))
    }

    #[inline]
    pub fn anyfloat(&mut self, span: Span) -> TypeId {
        self.insert(InferenceValue::AnyFloat, Some(span))
    }

    #[inline]
    pub fn partial_tuple(&mut self, elements: Vec<Type>, span: Span) -> TypeId {
        self.insert(InferenceValue::PartialTuple(elements), Some(span))
    }

    #[inline]
    pub fn partial_struct(&mut self, partial_struct: PartialStructTy, span: Span) -> TypeId {
        self.insert(InferenceValue::PartialStruct(partial_struct), Some(span))
    }

    #[inline]
    pub fn bound_maybe_spanned(&mut self, kind: Type, span: Option<Span>) -> TypeId {
        match kind {
            Type::Var(ty) | Type::Infer(ty, _) => ty,
            _ => self.insert(InferenceValue::Bound(kind), span),
        }
    }

    #[inline]
    pub fn bound(&mut self, kind: Type, span: Span) -> TypeId {
        match kind {
            Type::Var(ty) | Type::Infer(ty, _) => ty,
            _ => self.insert(InferenceValue::Bound(kind), Some(span)),
        }
    }

    #[inline]
    pub fn value_of(&self, var: TypeId) -> &InferenceValue {
        match self.bindings.get(var.0) {
            Some(ty) => ty,
            None => &InferenceValue::Unbound,
        }
    }

    #[inline]
    pub fn ty_kind(&self, ty: TypeId) -> Type {
        ty.normalize(self)
    }

    #[inline]
    pub fn ty_span(&self, ty: TypeId) -> Option<Span> {
        self.binding_spans.get(ty.0).cloned().flatten()
    }

    #[inline]
    pub fn bind_ty(&mut self, var: TypeId, ty: Type) {
        self.bind_value(var, InferenceValue::Bound(ty))
    }

    #[inline]
    pub fn bind_value(&mut self, var: TypeId, value: InferenceValue) {
        self.bindings[var.0] = value;
    }

    pub fn print_all_bindings(&self, only_concrete: bool) {
        for (i, b) in self.bindings.iter() {
            if !only_concrete || b.is_concrete() {
                println!("'{} :: {}", i, b)
            }
        }
    }

    pub fn print_binding(&self, ty: TypeId) {
        println!("'{} :: {}", ty.0, self.bindings[ty.0]);
    }
}

pub struct CommonTypes {
    pub unknown: TypeId,
    pub unit: TypeId,
    pub bool: TypeId,
    pub i8: TypeId,
    pub i16: TypeId,
    pub i32: TypeId,
    pub i64: TypeId,
    pub int: TypeId,
    pub u8: TypeId,
    pub u16: TypeId,
    pub u32: TypeId,
    pub u64: TypeId,
    pub uint: TypeId,
    pub f16: TypeId,
    pub f32: TypeId,
    pub f64: TypeId,
    pub float: TypeId,
    pub str: TypeId,
    pub never: TypeId,
    pub anytype: TypeId,
}

impl CommonTypes {
    pub fn new(
        bindings: &mut Slab<InferenceValue>,
        binding_spans: &mut Slab<Option<Span>>,
    ) -> Self {
        use Type::*;

        let mut mk = |kind| {
            binding_spans.insert(None);
            TypeId(bindings.insert(InferenceValue::Bound(kind)))
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
            str: mk(Type::str()),
            never: mk(Never),
            anytype: mk(AnyType),
        }
    }
}
