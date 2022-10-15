pub mod attrs;
pub mod const_value;
pub mod pretty;

use self::const_value::ConstValue;
use crate::{
    ast::ExternLibrary,
    common::id_cache::{IdCache, WithId},
    define_id_type,
    span::Span,
    types::TypeId,
    workspace::{BindingId, ModuleId},
};
use enum_as_inner::EnumAsInner;
use std::{collections::HashMap, fmt::Display};
use ustr::Ustr;

macro_rules! node_struct {
    ($name:ident) => {
        #[derive(Debug, PartialEq, Clone)]
        pub struct $name {
            pub ty: TypeId,
            pub span: Span,
        }
    };

    ($name:ident, { $($field:ident : $ty:ty) , + $(,)? }) => {
        #[derive(Debug, PartialEq, Clone)]
        pub struct $name {
            $(
                pub $field: $ty
            ),+,
            pub ty: TypeId,
            pub span: Span,
        }
    };
}

define_id_type!(FunctionId);

pub struct Cache {
    pub bindings: HashMap<BindingId, Binding>,
    pub functions: IdCache<FunctionId, Function>,

    // The entry point function's id (usually named "main"). Resolved during semantic analysis
    pub entry_point_function_id: Option<FunctionId>,
}

impl Cache {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            functions: IdCache::new(),
            entry_point_function_id: None,
        }
    }

    pub fn entry_point_function(&self) -> Option<&Function> {
        self.entry_point_function_id.and_then(|id| self.functions.get(id))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub id: FunctionId,
    pub module_id: ModuleId,
    pub name: Ustr,
    pub qualified_name: Ustr,
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
        params: Vec<FunctionParam>,
        inferred_return_type_span: Option<Span>, // This span will be filled when the function's return type is elided
        body: Option<Sequence>,                  // The body will be filled after the function is fully checked
    },
    Extern {
        lib: Option<ExternLibrary>,
        dylib: Option<ExternLibrary>,
        link_name: Ustr,
    },
    Intrinsic(Intrinsic),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionParam {
    pub id: BindingId,
    pub ty: TypeId,
    pub span: Span,
}

impl Function {
    /// This is a noop if the function doesn't have a body.
    /// Returns whether the function has a body
    pub fn set_body(&mut self, block: Sequence) -> bool {
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

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum Intrinsic {
    StartWorkspace,
    Location,
    CallerLocation,
    Os,
    Arch,
    CompilerError,
    CompilerWarning,
}

pub const INTRINSIC_NAME_START_WORKSPACE: &str = "start_workspace";
pub const INTRINSIC_NAME_LOCATION: &str = "location";
pub const INTRINSIC_NAME_CALLER_LOCATION: &str = "caller_location";
pub const INTRINSIC_NAME_OS: &str = "os";
pub const INTRINSIC_NAME_ARCH: &str = "arch";
pub const INTRINSIC_NAME_COMPILER_ERROR: &str = "compiler_error";
pub const INTRINSIC_NAME_COMPILER_WARNING: &str = "compiler_warning";

impl TryFrom<&str> for Intrinsic {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            INTRINSIC_NAME_START_WORKSPACE => Ok(Intrinsic::StartWorkspace),
            INTRINSIC_NAME_LOCATION => Ok(Intrinsic::Location),
            INTRINSIC_NAME_CALLER_LOCATION => Ok(Intrinsic::CallerLocation),
            INTRINSIC_NAME_OS => Ok(Intrinsic::Os),
            INTRINSIC_NAME_ARCH => Ok(Intrinsic::Arch),
            INTRINSIC_NAME_COMPILER_ERROR => Ok(Intrinsic::CompilerError),
            INTRINSIC_NAME_COMPILER_WARNING => Ok(Intrinsic::CompilerWarning),
            _ => Err(()),
        }
    }
}

impl Display for Intrinsic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Intrinsic::StartWorkspace => INTRINSIC_NAME_START_WORKSPACE,
                Intrinsic::Location => INTRINSIC_NAME_LOCATION,
                Intrinsic::CallerLocation => INTRINSIC_NAME_CALLER_LOCATION,
                Intrinsic::Os => INTRINSIC_NAME_OS,
                Intrinsic::Arch => INTRINSIC_NAME_ARCH,
                Intrinsic::CompilerError => INTRINSIC_NAME_COMPILER_ERROR,
                Intrinsic::CompilerWarning => INTRINSIC_NAME_COMPILER_WARNING,
            }
        )
    }
}

#[derive(Debug, PartialEq, Clone, EnumAsInner)]
pub enum Node {
    Const(Const),
    Binding(Binding),
    Id(Id),
    Assign(Assign),
    MemberAccess(MemberAccess),
    Call(Call),
    Cast(Cast),
    Sequence(Sequence),
    Control(Control),
    Builtin(Builtin),
    Literal(Literal),
}

node_struct!(Empty);

node_struct!(Const, { value: ConstValue });

node_struct!(Binding, { module_id: ModuleId, id: BindingId, name: Ustr, value: Box<Node> });
node_struct!(Id, { id: BindingId });
node_struct!(Assign, { lhs: Box<Node>, rhs: Box<Node> });
node_struct!(MemberAccess, { value: Box<Node>, member_name: Ustr, member_index: u32 });

node_struct!(Call, { callee: Box<Node>, args: Vec<Node> });
node_struct!(Cast, { value: Box<Node> });

node_struct!(Sequence, { statements: Vec<Node>, is_scope: bool });

node_struct!(If, { condition: Box<Node>, then: Box<Node>, otherwise: Option<Box<Node>> });
node_struct!(While, { condition: Box<Node>, body: Box<Node> });
node_struct!(Return, { value: Box<Node> });

node_struct!(Binary, { lhs: Box<Node>, rhs: Box<Node> });
node_struct!(Unary, { value: Box<Node> });
node_struct!(Ref, { value: Box<Node>, is_mutable: bool });

node_struct!(Offset, { value: Box<Node>, index: Box<Node> });
node_struct!(Slice, { value: Box<Node>, low: Box<Node>, high: Box<Node> });
// node_struct!(Transmute, { value: Box<Node> });

node_struct!(StructLiteral, { fields: Vec<StructLiteralField> });
node_struct!(StructLiteralField, { name: Ustr, value: Box<Node> });
node_struct!(TupleLiteral, { elements: Vec<Node> });
node_struct!(ArrayLiteral, { elements: Vec<Node> });
node_struct!(ArrayFillLiteral, { value: Box<Node>, len: usize });

#[derive(Debug, PartialEq, Clone, EnumAsInner)]
pub enum Control {
    If(If),
    While(While),
    Return(Return),
    Break(Empty),
    Continue(Empty),
}

#[derive(Debug, PartialEq, Clone, EnumAsInner)]
pub enum Builtin {
    Add(Binary),
    Sub(Binary),
    Mul(Binary),
    Div(Binary),
    Rem(Binary),

    Shl(Binary),
    Shr(Binary),

    And(Binary),
    Or(Binary),

    Lt(Binary),
    Le(Binary),
    Gt(Binary),
    Ge(Binary),
    Eq(Binary),
    Ne(Binary),

    BitAnd(Binary),
    BitOr(Binary),
    BitXor(Binary),

    Not(Unary),
    Neg(Unary),
    Deref(Unary),

    Ref(Ref),
    Offset(Offset),
    Slice(Slice),
    // TODO: Transmute(Transmute),
}

#[derive(Debug, PartialEq, Clone, EnumAsInner)]
pub enum Literal {
    Struct(StructLiteral),
    Tuple(TupleLiteral),
    Array(ArrayLiteral),
    ArrayFill(ArrayFillLiteral),
}

impl Node {
    #[inline(always)]
    pub fn ty(&self) -> TypeId {
        match self {
            Self::Const(x) => x.ty,
            Self::Binding(x) => x.ty,
            Self::Id(x) => x.ty,
            Self::Assign(x) => x.ty,
            Self::MemberAccess(x) => x.ty,
            Self::Call(x) => x.ty,
            Self::Cast(x) => x.ty,
            Self::Sequence(x) => x.ty,
            Self::Control(x) => x.ty(),
            Self::Builtin(x) => x.ty(),
            Self::Literal(x) => x.ty(),
        }
    }

    #[inline(always)]
    pub fn span(&self) -> Span {
        match self {
            Self::Const(x) => x.span,
            Self::Binding(x) => x.span,
            Self::Id(x) => x.span,
            Self::Assign(x) => x.span,
            Self::MemberAccess(x) => x.span,
            Self::Call(x) => x.span,
            Self::Cast(x) => x.span,
            Self::Sequence(x) => x.span,
            Self::Control(x) => x.span(),
            Self::Builtin(x) => x.span(),
            Self::Literal(x) => x.span(),
        }
    }
}

impl Control {
    pub fn ty(&self) -> TypeId {
        match self {
            Self::If(x) => x.ty,
            Self::While(x) => x.ty,
            Self::Return(x) => x.ty,
            Self::Break(x) => x.ty,
            Self::Continue(x) => x.ty,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::If(x) => x.span,
            Self::While(x) => x.span,
            Self::Return(x) => x.span,
            Self::Break(x) => x.span,
            Self::Continue(x) => x.span,
        }
    }
}

impl Builtin {
    pub fn ty(&self) -> TypeId {
        match self {
            Self::Add(x) => x.ty,
            Self::Sub(x) => x.ty,
            Self::Mul(x) => x.ty,
            Self::Div(x) => x.ty,
            Self::Rem(x) => x.ty,
            Self::Shl(x) => x.ty,
            Self::Shr(x) => x.ty,
            Self::And(x) => x.ty,
            Self::Or(x) => x.ty,
            Self::Lt(x) => x.ty,
            Self::Le(x) => x.ty,
            Self::Gt(x) => x.ty,
            Self::Ge(x) => x.ty,
            Self::Eq(x) => x.ty,
            Self::Ne(x) => x.ty,
            Self::BitAnd(x) => x.ty,
            Self::BitOr(x) => x.ty,
            Self::BitXor(x) => x.ty,
            Self::Not(x) => x.ty,
            Self::Neg(x) => x.ty,
            Self::Deref(x) => x.ty,
            Self::Ref(x) => x.ty,
            Self::Offset(x) => x.ty,
            Self::Slice(x) => x.ty,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Add(x) => x.span,
            Self::Sub(x) => x.span,
            Self::Mul(x) => x.span,
            Self::Div(x) => x.span,
            Self::Rem(x) => x.span,
            Self::Shl(x) => x.span,
            Self::Shr(x) => x.span,
            Self::And(x) => x.span,
            Self::Or(x) => x.span,
            Self::Lt(x) => x.span,
            Self::Le(x) => x.span,
            Self::Gt(x) => x.span,
            Self::Ge(x) => x.span,
            Self::Eq(x) => x.span,
            Self::Ne(x) => x.span,
            Self::BitAnd(x) => x.span,
            Self::BitOr(x) => x.span,
            Self::BitXor(x) => x.span,
            Self::Not(x) => x.span,
            Self::Neg(x) => x.span,
            Self::Deref(x) => x.span,
            Self::Ref(x) => x.span,
            Self::Offset(x) => x.span,
            Self::Slice(x) => x.span,
        }
    }
}

impl Literal {
    pub fn ty(&self) -> TypeId {
        match self {
            Self::Struct(x) => x.ty,
            Self::Tuple(x) => x.ty,
            Self::Array(x) => x.ty,
            Self::ArrayFill(x) => x.ty,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Struct(x) => x.span,
            Self::Tuple(x) => x.span,
            Self::Array(x) => x.span,
            Self::ArrayFill(x) => x.span,
        }
    }
}

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

    pub fn noop(ty: TypeId, span: Span) -> Self {
        Self::Sequence(Sequence {
            ty,
            span,
            statements: vec![],
            is_scope: false,
        })
    }

    pub fn force_into_sequence(self) -> Sequence {
        match self {
            Self::Sequence(x) => x,
            _ => {
                let ty = self.ty();
                let span = self.span();

                Sequence {
                    statements: vec![self],
                    is_scope: false,
                    ty,
                    span,
                }
            }
        }
    }
}
