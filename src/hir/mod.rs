use crate::{
    ast::{const_value::ConstValue, ty::TypeId, workspace::BindingId, Function, FunctionId},
    common::id_cache::IdCache,
    span::Span,
};
use enum_as_inner::EnumAsInner;

macro_rules! node_struct {
    ($name:ident) => {
        #[derive(Debug, Clone)]
        pub struct $name {
            ty: TypeId,
            span: Span,
        }
    };

    ($name:ident, { $($field:ident : $ty:ty) , + $(,)? }) => {
        #[derive(Debug, Clone)]
        pub struct $name {
            ty: TypeId,
            span: Span,
            $(
                $field: $ty
            ),+
        }
    };
}

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

#[derive(Debug, Clone, EnumAsInner)]
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

node_struct!(Empty);
node_struct!(Const, { value: ConstValue });
node_struct!(Binding, { id: BindingId, value: Box<Node> });
node_struct!(Id, { id: BindingId });
node_struct!(Assignment, { lhs: Box<Node>, rhs: Box<Node> });
node_struct!(MemberAccess, { value: Box<Node>, index: u32 });
node_struct!(Call, { value: Box<Node>, args: Vec<Node> });
node_struct!(Sequence, { statements: Vec<Node> });
node_struct!(If, { condition: Box<Node>, then: Box<Node>, otherwise: Option<Box<Node>> });
node_struct!(While, { condition: Box<Node>, body: Box<Node> });
node_struct!(Return, { condition: Box<Node>, value: Box<Node> });
node_struct!(Binary, { lhs: Box<Node>, rhs: Box<Node> });
node_struct!(Cast, { value: Box<Node> });
node_struct!(Deref, { value: Box<Node> });
node_struct!(Offset, { value: Box<Node>, offset: Box<Node> });
// node_struct!(Transmute, { value: Box<Node> });

#[derive(Debug, Clone, EnumAsInner)]
pub enum Control {
    If(If),
    While(While),
    Return(Return),
    Break(Empty),
    Continue(Empty),
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Builtin {
    AddSigned(Binary),
    AddFloat(Binary),

    SubSigned(Binary),
    SubFloat(Binary),

    MulSigned(Binary),
    MulFloat(Binary),

    DivSigned(Binary),
    DivUnsigned(Binary),
    DivFloat(Binary),

    ModSigned(Binary),
    ModUnsigned(Binary),
    ModFloat(Binary),

    LtSigned(Binary),
    LtUnsigned(Binary),
    LtFloat(Binary),

    LeSigned(Binary),
    LeUnsigned(Binary),
    LeFloat(Binary),

    GtSigned(Binary),
    GtUnsigned(Binary),
    GtFloat(Binary),

    GeSigned(Binary),
    GeUnsigned(Binary),
    GeFloat(Binary),

    EqSigned(Binary),
    EqUnsigned(Binary),
    EqFloat(Binary),
    EqBool(Binary),

    NeSigned(Binary),
    NeUnsigned(Binary),
    NeFloat(Binary),
    NeBool(Binary),

    SignedToFloat(Cast),
    UnsignedToFloat(Cast),
    FloatToSigned(Cast),
    FloatToUnsigned(Cast),

    BitwiseAnd(Binary),
    BitwiseOr(Binary),
    BitwiseXor(Binary),
    BitwiseNot(Box<Node>),

    Deref(Deref),
    Offset(Offset),
    // TODO: Transmute(Transmute),
}

macro_rules! node_field_dispatch {
    ($field:ident, $ty:ty) => {
        impl Node {
            pub fn $field(&self) -> $ty {
                match self {
                    Self::Const(x) => x.$field,
                    Self::Binding(x) => x.$field,
                    Self::Id(x) => x.$field,
                    Self::Assignment(x) => x.$field,
                    Self::MemberAccess(x) => x.$field,
                    Self::Call(x) => x.$field,
                    Self::Sequence(x) => x.$field,
                    Self::Control(x) => x.$field(),
                }
            }
        }
    };
}

node_field_dispatch!(ty, TypeId);
node_field_dispatch!(span, Span);

macro_rules! control_field_dispatch {
    ($field:ident, $ty:ty) => {
        impl Control {
            pub fn $field(&self) -> $ty {
                match self {
                    Self::If(x) => x.$field,
                    Self::While(x) => x.$field,
                    Self::Return(x) => x.$field,
                    Self::Break(x) => x.$field,
                    Self::Continue(x) => x.$field,
                }
            }
        }
    };
}

control_field_dispatch!(ty, TypeId);
control_field_dispatch!(span, Span);
