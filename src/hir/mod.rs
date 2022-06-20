use enum_as_inner::EnumAsInner;

use crate::{
    ast::{
        ast::{Function, FunctionId},
        const_value::ConstValue,
        ty::TypeId,
        workspace::BindingId,
    },
    common::id_cache::IdCache,
    span::Span,
};

pub struct Cache {
    pub bindings: IdCache<BindingId, Binding>,
    pub functions: IdCache<FunctionId, Function>,
}

impl Cache {
    pub fn new() -> Self {
        Self {
            bindings: IdCache::new(),
            functions: IdCache::new(),
        }
    }
}

#[derive(Debug, EnumAsInner, Clone)]
pub enum Node {
    Const(Const),
    Binding(Binding),
    Id(Id),
    Assignment(Assignment),
    MemberAccess(MemberAccess),
    Call(Call),
    Sequence(Sequence),
    Control(Control),
}

#[derive(Debug, Clone)]
pub struct Const {
    value: ConstValue,
    ty: TypeId,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct Binding {
    id: BindingId,
    value: Box<Node>,
    ty: TypeId,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct Id {
    id: BindingId,
    ty: TypeId,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    lhs: Box<Node>,
    rhs: Box<Node>,
    ty: TypeId,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct MemberAccess {
    value: Box<Node>,
    index: u32,
    ty: TypeId,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct Call {
    value: Box<Node>,
    args: Vec<Node>,
    ty: TypeId,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct Sequence {
    statements: Vec<Node>,
    ty: TypeId,
    span: Span,
}

#[derive(Debug, Clone)]
pub enum Control {
    If(If),
    While(While),
    Return(Return),
    Break(Break),
    Continue(Continue),
}

#[derive(Debug, Clone)]
pub struct If {
    condition: Box<Node>,
    then: Box<Node>,
    otherwise: Option<Box<Node>>,
    ty: TypeId,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct While {
    condition: Box<Node>,
    then: Box<Node>,
    otherwise: Option<Box<Node>>,
    ty: TypeId,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct Return {
    value: Box<Node>,
    ty: TypeId,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct Break {
    ty: TypeId,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct Continue {
    ty: TypeId,
    span: Span,
}

#[derive(Debug, Clone)]
pub enum Builtin {
    AddInt(Binary),
    AddFloat(Binary),

    SubInt(Binary),
    SubFloat(Binary),

    MulInt(Binary),
    MulFloat(Binary),

    DivInt(Binary),
    DivUint(Binary),
    DivFloat(Binary),

    ModSigned(Binary),
    ModUnsigned(Binary),
    ModFloat(Binary),

    LtInt(Binary),
    LtUint(Binary),
    LtFloat(Binary),

    LeInt(Binary),
    LeUint(Binary),
    LeFloat(Binary),

    GtInt(Binary),
    GtUint(Binary),
    GtFloat(Binary),

    GeInt(Binary),
    GeUint(Binary),
    GeFloat(Binary),

    EqInt(Binary),
    EqFloat(Binary),
    EqBool(Binary),

    IntToFloat(Cast),
    UintToFloat(Cast),
    FloatToInt(Cast),
    FloatToUint(Cast),

    BitwiseAnd(Binary),
    BitwiseOr(Binary),
    BitwiseXor(Binary),
    BitwiseNot(Box<Node>),

    Deref(Deref),
    Offset(Offset),
    Transmute(Transmute),
}

#[derive(Debug, Clone)]
pub struct Binary {
    lhs: Box<Node>,
    rhs: Box<Node>,
    ty: TypeId,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct Cast {
    value: Box<Node>,
    ty: TypeId,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct Deref {
    value: Box<Node>,
    ty: TypeId,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct Offset {
    value: Box<Node>,
    offset: Box<Node>,
    ty: TypeId,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct Transmute {
    value: Box<Node>,
    ty: TypeId,
    span: Span,
}

impl Node {
    pub fn ty(&self) -> TypeId {
        match self {
            Self::Const(x) => x.ty,
            Self::Binding(x) => x.ty,
            Self::Id(x) => x.ty,
            Self::Assignment(x) => x.ty,
            Self::MemberAccess(x) => x.ty,
            Self::Call(x) => x.ty,
            Self::Sequence(x) => x.ty,
            Self::Control(x) => x.ty(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Const(c) => c.span,
            Self::Binding(c) => c.span,
            Self::Id(x) => x.span,
            Self::Assignment(x) => x.span,
            Self::MemberAccess(x) => x.span,
            Self::Call(x) => x.span,
            Self::Sequence(x) => x.span,
            Self::Control(x) => x.span(),
        }
    }
}

impl Control {
    pub fn ty(&self) -> TypeId {
        match self {
            Control::If(x) => x.ty,
            Control::While(x) => x.ty,
            Control::Return(x) => x.ty,
            Control::Break(x) => x.ty,
            Control::Continue(x) => x.ty,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Control::If(x) => x.span,
            Control::While(x) => x.span,
            Control::Return(x) => x.span,
            Control::Break(x) => x.span,
            Control::Continue(x) => x.span,
        }
    }
}
