pub mod align_of;
pub mod display;
pub mod is_sized;
pub mod offset_of;
pub mod size_of;

use crate::{
    ast::ExternLibrary,
    define_id_type,
    span::Span,
    workspace::{BindingId, ModuleId},
};
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
    Function(FunctionType),
    Array(Box<Type>, usize),
    Slice(Box<Type>),
    Tuple(Vec<Type>),
    Struct(StructType),
    Module(ModuleId),
    Type(Box<Type>),
    AnyType,
    Var(TypeId),
    Infer(TypeId, InferType),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InferType {
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
    pub params: Vec<FunctionTypeParam>,
    pub return_type: Box<Type>,
    pub varargs: Option<Box<FunctionTypeVarargs>>,
    pub kind: FunctionTypeKind,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionTypeParam {
    pub name: Ustr,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionTypeVarargs {
    pub name: Ustr,
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
        self.fields.iter().find(|f| f.name == field)
    }

    pub fn find_field_position(&self, field: Ustr) -> Option<usize> {
        self.fields.iter().position(|f| f.name == field)
    }

    pub fn find_field_full(&self, field: Ustr) -> Option<(usize, &StructTypeField)> {
        self.fields
            .iter()
            .enumerate()
            .find(|(_, f)| f.name == field)
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
                    name: symbol,
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
    pub fn empty(name: Ustr, binding_id: BindingId, kind: StructTypeKind) -> Self {
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
    pub name: Ustr,
    pub ty: Type,
    pub span: Span,
}

impl StructTypeField {
    pub fn temp(ty: Type) -> Self {
        Self {
            name: ustr(""),
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
    pub fn as_inner(&self) -> &Type {
        match self {
            Type::Pointer(inner, _)
            | Type::Array(inner, _)
            | Type::Slice(inner)
            | Type::Type(inner) => inner,
            _ => panic!("type {} doesn't have an inner type", self),
        }
    }

    pub fn into_inner(self) -> Type {
        match self {
            Type::Pointer(inner, _)
            | Type::Array(inner, _)
            | Type::Slice(inner)
            | Type::Type(inner) => *inner,
            _ => panic!("type {} doesn't have an inner type", self),
        }
    }

    pub fn element_type(&self) -> Option<&Type> {
        match self {
            Type::Pointer(inner, _) => match inner.as_ref() {
                Type::Slice(inner) => Some(inner),
                inner => Some(inner),
            },
            Type::Array(inner, _) | Type::Type(inner) => Some(inner),
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
        matches!(self, Type::Infer(_, InferType::AnyInt))
    }

    pub fn is_anyfloat(&self) -> bool {
        matches!(self, Type::Infer(_, InferType::AnyFloat))
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

    pub fn is_bool(&self) -> bool {
        matches!(self, Type::Bool)
    }

    pub fn is_function(&self) -> bool {
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

    pub fn as_function(&self) -> &FunctionType {
        match self {
            Type::Function(ty) => ty,
            _ => panic!("expected fn, got {:?}", self),
        }
    }

    pub fn into_function(self) -> FunctionType {
        match self {
            Type::Function(ty) => ty,
            _ => panic!("expected fn, got {:?}", self),
        }
    }

    pub fn as_type(&self) -> &Type {
        match self {
            Type::Type(ty) => ty,
            _ => panic!("expected type, got {:?}", self),
        }
    }

    pub fn into_type(self) -> Type {
        match self {
            Type::Type(ty) => *ty,
            _ => panic!("expected type, got {:?}", self),
        }
    }

    pub fn create_type(self) -> Type {
        Type::Type(Box::new(self))
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

    pub fn is_extern(&self) -> bool {
        match self {
            Type::Never
            | Type::Unit
            | Type::Bool
            | Type::Int(_)
            | Type::Uint(_)
            | Type::Float(_) => true,

            Type::Module(_)
            | Type::Slice(_)
            | Type::Type(_)
            | Type::AnyType
            | Type::Var(_)
            | Type::Infer(_, _) => false,

            Type::Pointer(inner, _) | Type::Array(inner, _) => inner.is_extern(),

            Type::Function(f) => {
                f.return_type.is_extern()
                    && f.varargs
                        .as_ref()
                        .map_or(true, |v| v.ty.as_ref().map_or(true, Self::is_extern))
                    && f.params.iter().map(|p| &p.ty).all(Self::is_extern)
            }

            Type::Tuple(tys) => tys.iter().all(Self::is_extern),

            Type::Struct(st) => st.fields.iter().all(|f| f.ty.is_extern()),
        }
    }

    #[inline]
    pub fn unit() -> Type {
        Type::Unit
    }

    #[inline]
    pub fn never() -> Type {
        Type::Never
    }

    #[inline]
    pub fn bool() -> Type {
        Type::Bool
    }

    #[inline]
    pub fn i8() -> Type {
        Type::Int(IntType::I8)
    }

    #[inline]
    pub fn i16() -> Type {
        Type::Int(IntType::I16)
    }

    #[inline]
    pub fn i32() -> Type {
        Type::Int(IntType::I32)
    }

    #[inline]
    pub fn i64() -> Type {
        Type::Int(IntType::I64)
    }

    #[inline]
    pub fn int() -> Type {
        Type::Int(IntType::Int)
    }

    #[inline]
    pub fn u8() -> Type {
        Type::Uint(UintType::U8)
    }

    #[inline]
    pub fn u16() -> Type {
        Type::Uint(UintType::U16)
    }

    #[inline]
    pub fn u32() -> Type {
        Type::Uint(UintType::U32)
    }

    #[inline]
    pub fn u64() -> Type {
        Type::Uint(UintType::U64)
    }

    #[inline]
    pub fn uint() -> Type {
        Type::Uint(UintType::Uint)
    }

    #[inline]
    pub fn f16() -> Type {
        Type::Float(FloatType::F16)
    }

    #[inline]
    pub fn f32() -> Type {
        Type::Float(FloatType::F32)
    }

    #[inline]
    pub fn f64() -> Type {
        Type::Float(FloatType::F64)
    }

    #[inline]
    pub fn float() -> Type {
        Type::Float(FloatType::Float)
    }

    #[inline]
    pub fn raw_pointer(is_mutable: bool) -> Type {
        Type::Pointer(Box::new(Type::i8()), is_mutable)
    }

    #[inline]
    pub fn str() -> Type {
        Type::Pointer(Box::new(Type::Slice(Box::new(Type::char()))), false)
    }

    #[inline]
    pub fn char() -> Type {
        Type::Uint(UintType::U8)
    }
}
