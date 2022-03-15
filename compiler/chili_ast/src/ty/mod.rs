pub mod align;
pub mod display;
pub mod hash;
pub mod size;

use std::hash::{Hash, Hasher};

use chili_span::Span;
use ustr::{ustr, Ustr};

use crate::workspace::{BindingInfoId, ModuleId};

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Ty {
    Never,
    Unit,
    Bool,
    Int(IntTy),
    UInt(UIntTy),
    Float(FloatTy),
    Pointer(Box<Ty>, bool),
    MultiPointer(Box<Ty>, bool),
    Fn(FnTy),
    Array(Box<Ty>, usize),
    Slice(Box<Ty>, bool),
    Tuple(Vec<Ty>),
    Struct(StructTy),
    Module(ModuleId),
    Type(Box<Ty>),
    Var(u32),
    AnyInt,
    AnyFloat,
    Unknown,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum IntTy {
    I8,
    I16,
    I32,
    I64,
    Isize,
}

impl Default for IntTy {
    fn default() -> Self {
        IntTy::I32
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum UIntTy {
    U8,
    U16,
    U32,
    U64,
    Usize,
}

impl Default for UIntTy {
    fn default() -> Self {
        UIntTy::U32
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum FloatTy {
    F16,
    F32,
    F64,
    Fsize,
}

impl Default for FloatTy {
    fn default() -> Self {
        FloatTy::F32
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FnTy {
    pub params: Vec<FnTyParam>,
    pub ret: Box<Ty>,
    pub variadic: bool,
    pub lib_name: Option<Ustr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FnTyParam {
    pub symbol: Ustr,
    pub ty: Ty,
}

impl FnTyParam {
    pub fn named(symbol: impl Into<Ustr>, ty: Ty) -> Self {
        Self {
            symbol: symbol.into(),
            ty,
        }
    }

    pub fn unnamed(ty: Ty) -> Self {
        Self {
            symbol: ustr(""),
            ty,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructTy {
    pub name: Ustr,
    pub qualified_name: Ustr,
    pub binding_info_id: Option<BindingInfoId>,
    pub fields: Vec<StructTyField>,
    pub kind: StructTyKind,
}

impl From<StructTy> for Ty {
    fn from(ty: StructTy) -> Self {
        Ty::Struct(ty)
    }
}

impl StructTy {
    pub fn is_struct(&self) -> bool {
        match self.kind {
            StructTyKind::Struct => true,
            _ => false,
        }
    }

    pub fn is_packed_struct(&self) -> bool {
        match self.kind {
            StructTyKind::PackedStruct => true,
            _ => false,
        }
    }

    pub fn is_union(&self) -> bool {
        match self.kind {
            StructTyKind::Union => true,
            _ => false,
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum StructTyKind {
    Struct,
    PackedStruct,
    Union,
}

impl StructTy {
    pub fn opaque(name: Ustr, qualified_name: Ustr, kind: StructTyKind) -> Self {
        Self {
            name,
            qualified_name,
            binding_info_id: None,
            fields: vec![],
            kind,
        }
    }

    pub fn temp(fields: Vec<StructTyField>, kind: StructTyKind) -> Self {
        Self {
            name: ustr(""),
            qualified_name: ustr(""),
            binding_info_id: None,
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
    pub ty: Ty,
    pub span: Span,
}

impl StructTyField {
    pub fn temp(ty: Ty) -> Self {
        Self {
            symbol: ustr(""),
            ty,
            span: Span::unknown(),
        }
    }
}

impl Into<String> for Ty {
    fn into(self) -> String {
        self.to_string()
    }
}

impl Ty {
    pub fn raw_pointer(is_mutable: bool) -> Ty {
        Ty::Pointer(Box::new(Ty::Int(IntTy::I8)), is_mutable)
    }

    pub fn str() -> Ty {
        Ty::Slice(Box::new(Ty::char()), false)
    }

    pub fn char() -> Ty {
        Ty::UInt(UIntTy::U8)
    }

    pub fn create_type(self) -> Ty {
        Ty::Type(Box::new(self))
    }

    pub fn anytype() -> Ty {
        Ty::Unknown.create_type()
    }

    pub fn is_type(&self) -> bool {
        match self {
            Ty::Type(_) => true,
            _ => false,
        }
    }

    pub fn is_number(&self) -> bool {
        self.is_any_int() || self.is_float()
    }

    pub fn is_any_int(&self) -> bool {
        self.is_int() || self.is_uint()
    }

    pub fn is_int(&self) -> bool {
        match self {
            Ty::Int(_) => true,
            _ => false,
        }
    }

    pub fn is_uint(&self) -> bool {
        match self {
            Ty::UInt(_) => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Ty::Float(_) => true,
            _ => false,
        }
    }

    pub fn is_pointer(&self) -> bool {
        match self {
            Ty::Pointer(..) => true,
            _ => false,
        }
    }

    pub fn is_multi_pointer(&self) -> bool {
        match self {
            Ty::MultiPointer(..) => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Ty::Bool => true,
            _ => false,
        }
    }

    pub fn is_fn(&self) -> bool {
        match self {
            Ty::Fn { .. } => true,
            _ => false,
        }
    }

    pub fn is_var(&self) -> bool {
        match self {
            Ty::Var(..) => true,
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            Ty::Array(..) => true,
            _ => false,
        }
    }

    pub fn is_slice(&self) -> bool {
        match self {
            Ty::Slice(..) => true,
            _ => false,
        }
    }

    pub fn is_unknown(&self) -> bool {
        match self {
            Ty::Unknown => true,
            _ => false,
        }
    }

    pub fn is_unit(&self) -> bool {
        match self {
            Ty::Unit => true,
            _ => false,
        }
    }

    pub fn is_never(&self) -> bool {
        match self {
            Ty::Never => true,
            _ => false,
        }
    }

    pub fn is_struct(&self) -> bool {
        match self {
            Ty::Struct(_) => true,
            _ => false,
        }
    }

    pub fn is_aggregate(&self) -> bool {
        self.is_array() || self.is_struct()
    }

    pub fn into_struct(&self) -> &StructTy {
        match self {
            Ty::Struct(ty) => ty,
            _ => panic!("expected struct, got {:?}", self),
        }
    }

    pub fn as_fn(&self) -> &FnTy {
        match self {
            Ty::Fn(ty) => ty,
            _ => panic!("expected fn, got {:?}", self),
        }
    }

    pub fn element_type(&self) -> Result<&Ty, ()> {
        match self {
            Ty::Pointer(inner, _)
            | Ty::MultiPointer(inner, _)
            | Ty::Array(inner, _)
            | Ty::Slice(inner, _)
            | Ty::Type(inner) => Ok(&inner),
            _ => Err(()),
        }
    }

    pub fn pointer_type(self, mutable: bool) -> Ty {
        Ty::Pointer(Box::new(self), mutable)
    }

    pub fn maybe_deref_once(&self) -> Ty {
        match self {
            Ty::Pointer(inner, _) => inner.as_ref().clone(),
            _ => self.clone(),
        }
    }
}
