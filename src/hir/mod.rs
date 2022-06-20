use enum_as_inner::EnumAsInner;
use slab::Slab;

use crate::{
    ast::{
        ast::{Function, FunctionId},
        const_value::ConstValue,
        ty::TypeId,
        workspace::BindingInfoId,
    },
    span::Span,
};

pub struct Cache {
    pub bindings: Slab<Binding>,
    pub functions: Slab<Function>,
}

impl Cache {
    pub fn new() -> Self {
        Self {
            bindings: Slab::new(),
            functions: Slab::new(),
        }
    }

    pub fn get_binding(&self, id: BindingInfoId) -> Option<&Binding> {
        self.bindings.get(id.inner())
    }

    pub fn get_binding_mut(&mut self, id: BindingInfoId) -> Option<&mut Binding> {
        self.bindings.get_mut(id.inner())
    }

    pub fn push_binding(&mut self, id: BindingInfoId, mut binding: Binding) -> BindingInfoId {
        let vacant_entry = self.bindings.vacant_entry();

        let id = BindingInfoId::from(vacant_entry.key());

        binding.id = id;
        vacant_entry.insert(binding);

        id
    }

    pub fn get_function(&self, id: FunctionId) -> Option<&Function> {
        self.functions.get(id.inner())
    }

    pub fn get_function_mut(&mut self, id: FunctionId) -> Option<&mut Function> {
        self.functions.get_mut(id.inner())
    }

    pub fn push_function(&mut self, mut function: Function) -> FunctionId {
        let vacant_entry = self.functions.vacant_entry();

        let id = FunctionId::from(vacant_entry.key());

        function.id = id;
        vacant_entry.insert(function);

        id
    }
}

#[derive(Debug, EnumAsInner, Clone)]
pub enum Node {
    Const(Const),
    Binding(Binding),
    Ident(Ident),
    Assign(Assign),
    MemberAccess(MemberAccess),
    Call(Call),
    Block(Block),
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
    id: BindingInfoId,
    value: Box<Node>,
    ty: TypeId,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct Ident {
    id: BindingInfoId,
    ty: TypeId,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct Assign {
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
pub struct Block {
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
            Self::Ident(x) => x.ty,
            Self::Assign(x) => x.ty,
            Self::MemberAccess(x) => x.ty,
            Self::Call(x) => x.ty,
            Self::Block(x) => x.ty,
            Self::Control(x) => x.ty(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Const(c) => c.span,
            Self::Binding(c) => c.span,
            Self::Ident(x) => x.span,
            Self::Assign(x) => x.span,
            Self::MemberAccess(x) => x.span,
            Self::Call(x) => x.span,
            Self::Block(x) => x.span,
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
