pub mod const_value;
pub mod pretty;

use crate::{
    ast::{
        ty::TypeId,
        workspace::{BindingId, ModuleId},
        Block, ExternLibrary, Intrinsic,
    },
    common::id_cache::{IdCache, WithId},
    define_id_type,
    span::Span,
};
use enum_as_inner::EnumAsInner;
use ustr::Ustr;

use self::const_value::{ConstFunction, ConstValue};

macro_rules! node_struct {
    ($name:ident) => {
        #[derive(Debug, Clone)]
        pub struct $name {
            pub ty: TypeId,
            pub span: Span,
        }
    };

    ($name:ident, { $($field:ident : $ty:ty) , + $(,)? }) => {
        #[derive(Debug, Clone)]
        pub struct $name {
            pub ty: TypeId,
            pub span: Span,
            $(
                pub $field: $ty
            ),+
        }
    };
}

define_id_type!(FunctionId);

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

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub id: FunctionId,
    pub module_id: ModuleId,
    pub name: Ustr,
    pub kind: FunctionKind,
    pub ty: TypeId,
    pub span: Span,
}

impl WithId<FunctionId> for Function {
    fn id(&self) -> &FunctionId {
        &self.id
    }

    fn id_mut(&mut self) -> &mut FunctionId {
        &mut self.id
    }
}

#[derive(Debug, PartialEq, Clone, EnumAsInner)]
pub enum FunctionKind {
    Orphan {
        body: Option<Block>, // The body will be filled after the function is fully checked
    },
    Extern {
        lib: Option<ExternLibrary>,
    },
    Intrinsic(Intrinsic),
}

impl Function {
    pub fn as_const_function(&self) -> ConstFunction {
        ConstFunction {
            id: self.id,
            name: self.name,
        }
    }

    /// This is a noop if the function doesn't have a body.
    /// Returns whether the function has a body
    pub fn set_body(&mut self, block: Block) -> bool {
        match &mut self.kind {
            FunctionKind::Orphan { body, .. } => {
                *body = Some(block);
                true
            }
            FunctionKind::Extern { .. } => false,
            FunctionKind::Intrinsic { .. } => false,
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
    Builtin(Builtin),
    Literal(Literal),
}

node_struct!(Empty);
node_struct!(Const, { value: ConstValue });
node_struct!(Binding, { module_id: ModuleId, id: BindingId, name: Ustr, value: Box<Node> });
node_struct!(Id, { id: BindingId });
node_struct!(Assignment, { lhs: Box<Node>, rhs: Box<Node> });
node_struct!(MemberAccess, { value: Box<Node>, index: u32 });
node_struct!(Call, { value: Box<Node>, args: Vec<Node> });
node_struct!(Sequence, { statements: Vec<Node> });
node_struct!(If, { condition: Box<Node>, then: Box<Node>, otherwise: Option<Box<Node>> });
node_struct!(While, { condition: Box<Node>, body: Box<Node> });
node_struct!(Return, { condition: Box<Node>, value: Box<Node> });
node_struct!(Binary, { lhs: Box<Node>, rhs: Box<Node> });
node_struct!(Unary, { value: Box<Node> });
node_struct!(Cast, { value: Box<Node> });
node_struct!(Deref, { value: Box<Node> });
node_struct!(Offset, { value: Box<Node>, offset: Box<Node> });
node_struct!(StructLiteral, { fields: Vec<StructLiteralField> });
node_struct!(StructLiteralField, { name: Ustr, value: Box<Node> });
node_struct!(TupleLiteral, { elements: Vec<Node> });

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
    BitwiseNot(Unary),

    Deref(Deref),
    Offset(Offset),
    // TODO: Transmute(Transmute),
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Literal {
    Struct(StructLiteral),
    Tuple(TupleLiteral),
}

macro_rules! node_field_dispatch {
    ($field:ident, $ty:ty) => {
        impl Node {
            #[inline(always)]
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
                    Self::Builtin(x) => x.$field(),
                    Self::Literal(x) => x.$field(),
                }
            }
        }
    };

    () => {
        node_field_dispatch!(ty, TypeId);
        node_field_dispatch!(span, Span);
    };
}

node_field_dispatch!();

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

    () => {
        control_field_dispatch!(ty, TypeId);
        control_field_dispatch!(span, Span);
    };
}

control_field_dispatch!();

macro_rules! builtin_field_dispatch {
    ($field:ident, $ty:ty) => {
        impl Builtin {
            pub fn $field(&self) -> $ty {
                match self {
                    Self::AddSigned(x) => x.$field,
                    Self::AddFloat(x) => x.$field,
                    Self::SubSigned(x) => x.$field,
                    Self::SubFloat(x) => x.$field,
                    Self::MulSigned(x) => x.$field,
                    Self::MulFloat(x) => x.$field,
                    Self::DivSigned(x) => x.$field,
                    Self::DivUnsigned(x) => x.$field,
                    Self::DivFloat(x) => x.$field,
                    Self::ModSigned(x) => x.$field,
                    Self::ModUnsigned(x) => x.$field,
                    Self::ModFloat(x) => x.$field,
                    Self::LtSigned(x) => x.$field,
                    Self::LtUnsigned(x) => x.$field,
                    Self::LtFloat(x) => x.$field,
                    Self::LeSigned(x) => x.$field,
                    Self::LeUnsigned(x) => x.$field,
                    Self::LeFloat(x) => x.$field,
                    Self::GtSigned(x) => x.$field,
                    Self::GtUnsigned(x) => x.$field,
                    Self::GtFloat(x) => x.$field,
                    Self::GeSigned(x) => x.$field,
                    Self::GeUnsigned(x) => x.$field,
                    Self::GeFloat(x) => x.$field,
                    Self::EqSigned(x) => x.$field,
                    Self::EqUnsigned(x) => x.$field,
                    Self::EqFloat(x) => x.$field,
                    Self::EqBool(x) => x.$field,
                    Self::NeSigned(x) => x.$field,
                    Self::NeUnsigned(x) => x.$field,
                    Self::NeFloat(x) => x.$field,
                    Self::NeBool(x) => x.$field,
                    Self::SignedToFloat(x) => x.$field,
                    Self::UnsignedToFloat(x) => x.$field,
                    Self::FloatToSigned(x) => x.$field,
                    Self::FloatToUnsigned(x) => x.$field,
                    Self::BitwiseAnd(x) => x.$field,
                    Self::BitwiseOr(x) => x.$field,
                    Self::BitwiseXor(x) => x.$field,
                    Self::BitwiseNot(x) => x.$field,
                    Self::Deref(x) => x.$field,
                    Self::Offset(x) => x.$field,
                }
            }
        }
    };

    () => {
        builtin_field_dispatch!(ty, TypeId);
        builtin_field_dispatch!(span, Span);
    };
}

builtin_field_dispatch!();

macro_rules! literal_field_dispatch {
    ($field:ident, $ty:ty) => {
        impl Literal {
            pub fn $field(&self) -> $ty {
                match self {
                    Self::Struct(x) => x.$field,
                    Self::Tuple(x) => x.$field,
                }
            }
        }
    };

    () => {
        literal_field_dispatch!(ty, TypeId);
        literal_field_dispatch!(span, Span);
    };
}

literal_field_dispatch!();

impl Node {
    pub fn is_const(&self) -> bool {
        matches!(self, Self::Const(_))
    }

    pub fn into_const_value(self) -> Option<ConstValue> {
        match self {
            Self::Const(c) => Some(c.value),
            _ => None,
        }
    }

    pub fn as_const_value(&self) -> Option<&ConstValue> {
        match self {
            Self::Const(c) => Some(&c.value),
            _ => None,
        }
    }
}
