pub mod align;
pub mod display;
pub mod size;

use super::{
    ast::ExternLibrary,
    workspace::{BindingInfoId, ModuleId},
};
use crate::span::Span;
use indexmap::IndexMap;
use std::ops::{Deref, DerefMut};
use ustr::{ustr, Ustr};

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct TypeId(pub usize);

impl Default for TypeId {
    fn default() -> Self {
        Self(usize::MAX)
    }
}

impl From<TypeId> for Type {
    fn from(val: TypeId) -> Self {
        Type::Var(val)
    }
}

impl TypeId {
    pub fn unknown() -> Self {
        Default::default()
    }

    pub fn as_kind(&self) -> Type {
        (*self).into()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Never,
    Unit,
    Bool,
    Int(IntTy),
    Uint(UintTy),
    Float(FloatTy),
    Pointer(Box<Type>, bool),
    MultiPointer(Box<Type>, bool),
    Function(FunctionTy),
    Array(Box<Type>, usize),
    Slice(Box<Type>, bool),
    Tuple(Vec<Type>),
    Struct(StructTy),
    Module(ModuleId),
    Type(Box<Type>),
    AnyType,
    Var(TypeId),
    Infer(TypeId, InferTy),
    Unknown,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InferTy {
    AnyInt,
    AnyFloat,
    PartialStruct(PartialStructTy),
    PartialTuple(Vec<Type>),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum IntTy {
    I8,
    I16,
    I32,
    I64,
    Int,
}

impl Default for IntTy {
    fn default() -> Self {
        IntTy::I32
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum UintTy {
    U8,
    U16,
    U32,
    U64,
    Uint,
}

impl Default for UintTy {
    fn default() -> Self {
        UintTy::U32
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum FloatTy {
    F16,
    F32,
    F64,
    Float,
}

impl Default for FloatTy {
    fn default() -> Self {
        FloatTy::F32
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionTy {
    pub params: Vec<Type>,
    pub ret: Box<Type>,
    pub varargs: Option<Box<FunctionTyVarargs>>,
    pub extern_lib: Option<ExternLibrary>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionTyVarargs {
    pub ty: Option<Type>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructTy {
    pub name: Ustr,
    pub binding_info_id: BindingInfoId,
    pub fields: Vec<StructTyField>,
    pub kind: StructTyKind,
}

impl StructTy {
    pub fn find_field(&self, field: Ustr) -> Option<&StructTyField> {
        self.fields.iter().find(|f| f.symbol == field)
    }

    pub fn find_field_position(&self, field: Ustr) -> Option<usize> {
        self.fields.iter().position(|f| f.symbol == field)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PartialStructTy(pub IndexMap<Ustr, Type>);

impl Deref for PartialStructTy {
    type Target = IndexMap<Ustr, Type>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for PartialStructTy {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl PartialStructTy {
    pub fn into_struct(&self) -> StructTy {
        StructTy {
            name: ustr(""),
            binding_info_id: Default::default(),
            fields: self
                .iter()
                .map(|(&symbol, ty)| StructTyField {
                    symbol,
                    ty: ty.clone(),
                    span: Span::unknown(),
                })
                .collect(),
            kind: StructTyKind::Struct,
        }
    }
}

impl From<StructTy> for Type {
    fn from(ty: StructTy) -> Self {
        Type::Struct(ty)
    }
}

impl StructTy {
    pub fn is_struct(&self) -> bool {
        matches!(self.kind, StructTyKind::Struct)
    }

    pub fn is_packed_struct(&self) -> bool {
        matches!(self.kind, StructTyKind::PackedStruct)
    }

    pub fn is_union(&self) -> bool {
        matches!(self.kind, StructTyKind::Union)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum StructTyKind {
    Struct,
    PackedStruct,
    Union,
}

impl StructTy {
    pub fn opaque(name: Ustr, binding_info_id: BindingInfoId, kind: StructTyKind) -> Self {
        Self {
            name,
            binding_info_id,
            fields: vec![],
            kind,
        }
    }

    pub fn temp(fields: Vec<StructTyField>, kind: StructTyKind) -> Self {
        Self {
            name: ustr(""),
            binding_info_id: Default::default(),
            fields,
            kind,
        }
    }

    pub fn is_anonymous(&self) -> bool {
        self.name.is_empty()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructTyField {
    pub symbol: Ustr,
    pub ty: Type,
    pub span: Span,
}

impl StructTyField {
    pub fn temp(ty: Type) -> Self {
        Self {
            symbol: ustr(""),
            ty,
            span: Span::unknown(),
        }
    }
}

impl From<Type> for String {
    fn from(val: Type) -> Self {
        val.to_string()
    }
}

impl Type {
    pub fn inner(&self) -> &Type {
        match self {
            Type::Pointer(inner, _)
            | Type::MultiPointer(inner, _)
            | Type::Array(inner, _)
            | Type::Slice(inner, _)
            | Type::Type(inner) => inner,
            _ => panic!("type {} doesn't have an inner type", self),
        }
    }

    pub fn is_type(&self) -> bool {
        matches!(self, Type::Type(_) | Type::AnyType)
    }

    pub fn is_anytype(&self) -> bool {
        matches!(self, Type::AnyType)
    }

    pub fn is_module(&self) -> bool {
        matches!(self, Type::Module(_))
    }

    pub fn is_number(&self) -> bool {
        self.is_any_integer() || self.is_float()
    }

    pub fn is_any_integer(&self) -> bool {
        self.is_int() || self.is_uint() || self.is_anyint()
    }

    pub fn is_signed_int(&self) -> bool {
        self.is_int() || self.is_anyint()
    }

    pub fn is_anyint(&self) -> bool {
        matches!(self, Type::Infer(_, InferTy::AnyInt))
    }

    pub fn is_anyfloat(&self) -> bool {
        matches!(self, Type::Infer(_, InferTy::AnyFloat))
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Type::Int(_))
    }

    pub fn is_uint(&self) -> bool {
        matches!(self, Type::Uint(_))
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Type::Float(_))
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, Type::Pointer(..))
    }

    pub fn is_multi_pointer(&self) -> bool {
        matches!(self, Type::MultiPointer(..))
    }

    pub fn is_any_pointer(&self) -> bool {
        matches!(self, Type::Pointer(..) | Type::MultiPointer(..))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Type::Bool)
    }

    pub fn is_fn(&self) -> bool {
        matches!(self, Type::Function(..))
    }

    pub fn is_var(&self) -> bool {
        matches!(self, Type::Var(..))
    }

    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array(..))
    }

    pub fn is_slice(&self) -> bool {
        matches!(self, Type::Slice(..))
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self, Type::Unknown)
    }

    pub fn is_unit(&self) -> bool {
        matches!(self, Type::Unit)
    }

    pub fn is_never(&self) -> bool {
        matches!(self, Type::Never)
    }

    pub fn is_struct(&self) -> bool {
        matches!(self, Type::Struct(_))
    }

    pub fn is_aggregate(&self) -> bool {
        self.is_array() || self.is_struct()
    }

    pub fn as_struct(&self) -> &StructTy {
        match self {
            Type::Struct(ty) => ty,
            _ => panic!("expected struct, got {:?}", self),
        }
    }

    pub fn into_struct(self) -> StructTy {
        match self {
            Type::Struct(ty) => ty,
            _ => panic!("expected struct, got {:?}", self),
        }
    }

    pub fn as_fn(&self) -> &FunctionTy {
        match self {
            Type::Function(ty) => ty,
            _ => panic!("expected fn, got {:?}", self),
        }
    }

    pub fn into_fn(self) -> FunctionTy {
        match self {
            Type::Function(ty) => ty,
            _ => panic!("expected fn, got {:?}", self),
        }
    }

    pub fn raw_pointer(is_mutable: bool) -> Type {
        Type::Pointer(Box::new(Type::Int(IntTy::I8)), is_mutable)
    }

    pub fn str() -> Type {
        Type::Slice(Box::new(Type::char()), false)
    }

    pub fn char() -> Type {
        Type::Uint(UintTy::U8)
    }

    pub fn create_type(self) -> Type {
        Type::Type(Box::new(self))
    }

    pub fn element_type(&self) -> Option<&Type> {
        match self {
            Type::Pointer(inner, _)
            | Type::MultiPointer(inner, _)
            | Type::Array(inner, _)
            | Type::Slice(inner, _)
            | Type::Type(inner) => Some(inner),
            _ => None,
        }
    }

    pub fn pointer_type(self, mutable: bool) -> Type {
        Type::Pointer(Box::new(self), mutable)
    }

    pub fn maybe_deref_once(&self) -> Type {
        match self {
            Type::Pointer(inner, _) => inner.as_ref().clone(),
            _ => self.clone(),
        }
    }
}
