pub mod align;
pub mod display;
pub mod size;

use super::{
    workspace::{BindingId, ModuleId},
    ExternLibrary,
};
use crate::{define_id_type, span::Span};
use indexmap::IndexMap;
use std::ops::{Deref, DerefMut};
use ustr::{ustr, Ustr};

define_id_type!(TypeId);

impl From<TypeId> for Type {
    fn from(val: TypeId) -> Self {
        Type::Var(val)
    }
}

impl TypeId {
    pub fn as_kind(&self) -> Type {
        Type::from(*self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Never,
    Unit,
    Bool,
    Int(IntType),
    Uint(UintType),
    Float(FloatType),
    Pointer(Box<Type>, bool),
    MultiPointer(Box<Type>, bool),
    Function(FunctionType),
    Array(Box<Type>, usize),
    Slice(Box<Type>, bool),
    Tuple(Vec<Type>),
    Struct(StructType),
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
    PartialStruct(PartialStructType),
    PartialTuple(Vec<Type>),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum IntType {
    I8,
    I16,
    I32,
    I64,
    Int,
}

impl Default for IntType {
    fn default() -> Self {
        IntType::I32
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum UintType {
    U8,
    U16,
    U32,
    U64,
    Uint,
}

impl Default for UintType {
    fn default() -> Self {
        UintType::U32
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum FloatType {
    F16,
    F32,
    F64,
    Float,
}

impl Default for FloatType {
    fn default() -> Self {
        FloatType::F32
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionType {
    pub params: Vec<Type>,
    pub ret: Box<Type>,
    pub varargs: Option<Box<FunctionTypeVarargs>>,
    pub kind: FunctionTypeKind,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionTypeVarargs {
    pub ty: Option<Type>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FunctionTypeKind {
    Orphan,
    Extern { lib: Option<ExternLibrary> },
}

impl FunctionTypeKind {
    pub fn is_orphan(&self) -> bool {
        matches!(self, FunctionTypeKind::Orphan)
    }

    pub fn is_extern(&self) -> bool {
        matches!(self, FunctionTypeKind::Extern { .. })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructType {
    pub name: Ustr,
    pub binding_id: BindingId,
    pub fields: Vec<StructTypeField>,
    pub kind: StructTypeKind,
}

impl StructType {
    pub fn find_field(&self, field: Ustr) -> Option<&StructTypeField> {
        self.fields.iter().find(|f| f.symbol == field)
    }

    pub fn find_field_position(&self, field: Ustr) -> Option<usize> {
        self.fields.iter().position(|f| f.symbol == field)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PartialStructType(pub IndexMap<Ustr, Type>);

impl Deref for PartialStructType {
    type Target = IndexMap<Ustr, Type>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for PartialStructType {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl PartialStructType {
    pub fn into_struct(&self) -> StructType {
        StructType {
            name: ustr(""),
            binding_id: Default::default(),
            fields: self
                .iter()
                .map(|(&symbol, ty)| StructTypeField {
                    symbol,
                    ty: ty.clone(),
                    span: Span::unknown(),
                })
                .collect(),
            kind: StructTypeKind::Struct,
        }
    }
}

impl From<StructType> for Type {
    fn from(ty: StructType) -> Self {
        Type::Struct(ty)
    }
}

impl StructType {
    pub fn is_struct(&self) -> bool {
        matches!(self.kind, StructTypeKind::Struct)
    }

    pub fn is_packed_struct(&self) -> bool {
        matches!(self.kind, StructTypeKind::PackedStruct)
    }

    pub fn is_union(&self) -> bool {
        matches!(self.kind, StructTypeKind::Union)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum StructTypeKind {
    Struct,
    PackedStruct,
    Union,
}

impl StructType {
    pub fn opaque(name: Ustr, binding_id: BindingId, kind: StructTypeKind) -> Self {
        Self {
            name,
            binding_id,
            fields: vec![],
            kind,
        }
    }

    pub fn temp(fields: Vec<StructTypeField>, kind: StructTypeKind) -> Self {
        Self {
            name: ustr(""),
            binding_id: Default::default(),
            fields,
            kind,
        }
    }

    pub fn is_anonymous(&self) -> bool {
        self.name.is_empty()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructTypeField {
    pub symbol: Ustr,
    pub ty: Type,
    pub span: Span,
}

impl StructTypeField {
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

    pub fn as_struct(&self) -> &StructType {
        match self {
            Type::Struct(ty) => ty,
            _ => panic!("expected struct, got {:?}", self),
        }
    }

    pub fn into_struct(self) -> StructType {
        match self {
            Type::Struct(ty) => ty,
            _ => panic!("expected struct, got {:?}", self),
        }
    }

    pub fn as_fn(&self) -> &FunctionType {
        match self {
            Type::Function(ty) => ty,
            _ => panic!("expected fn, got {:?}", self),
        }
    }

    pub fn into_fn(self) -> FunctionType {
        match self {
            Type::Function(ty) => ty,
            _ => panic!("expected fn, got {:?}", self),
        }
    }

    pub fn raw_pointer(is_mutable: bool) -> Type {
        Type::Pointer(Box::new(Type::Int(IntType::I8)), is_mutable)
    }

    pub fn str() -> Type {
        Type::Slice(Box::new(Type::char()), false)
    }

    pub fn char() -> Type {
        Type::Uint(UintType::U8)
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
