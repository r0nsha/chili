pub mod pat;

use crate::{
    common::path::{resolve_relative_path, try_resolve_relative_path, RelativeTo},
    define_id_type,
    error::DiagnosticResult,
    span::{FileId, Span},
    types::*,
    workspace::{ModuleId, ModuleInfo},
};
use pat::{NamePat, Pat};
use std::{
    ffi::OsStr,
    fmt::{self, Display},
    ops::Deref,
    path::{Path, PathBuf},
};
use ustr::{ustr, Ustr};

#[derive(Debug, Clone)]
pub struct Module {
    pub id: ModuleId,
    pub info: ModuleInfo,
    pub file_id: FileId,
    pub bindings: Vec<Binding>,
    pub comptime_blocks: Vec<Comptime>,
}

impl Module {
    pub fn new(file_id: FileId, module_info: ModuleInfo) -> Self {
        Self {
            file_id,
            id: module_info.id,
            info: module_info,
            bindings: vec![],
            comptime_blocks: vec![],
        }
    }

    pub fn find_binding(&self, name: Ustr) -> Option<(usize, &Binding)> {
        self.bindings
            .iter()
            .enumerate()
            .find(|(_, binding)| match &binding.kind {
                BindingKind::Let { pat, .. } => pat.iter().any(|pat| pat.name == name),
                BindingKind::Function {
                    name: NameAndSpan { name: binding_name, .. },
                    ..
                }
                | BindingKind::ExternFunction {
                    name: NameAndSpan { name: binding_name, .. },
                    ..
                }
                | BindingKind::ExternVariable {
                    name: NameAndSpan { name: binding_name, .. },
                    ..
                }
                | BindingKind::Type {
                    name: NameAndSpan { name: binding_name, .. },
                    ..
                } => *binding_name == name,
            })
    }
}

define_id_type!(FunctionId);

#[derive(Debug, PartialEq, Clone)]
pub enum Ast {
    Binding(Binding),
    Cast(Cast),
    Import(Import),
    Builtin(Builtin),
    Comptime(Comptime),
    Function(Function),
    Loop(Loop),
    While(While),
    For(For),
    Break(Empty),
    Continue(Empty),
    Return(Return),
    If(If),
    Block(Block),
    Binary(Binary),
    Unary(Unary),
    Subscript(Subscript),
    Slice(Slice),
    Call(Call),
    MemberAccess(MemberAccess),
    Ident(Ident),
    ArrayLiteral(ArrayLiteral),
    TupleLiteral(TupleLiteral),
    StructLiteral(StructLiteral),
    Literal(Literal),
    PointerType(PointerType),
    ArrayType(ArrayType),
    SliceType(SliceType),
    StructType(StructType),
    FunctionType(FunctionSig),
    SelfType(Empty),
    Placeholder(Empty),

    #[allow(unused)]
    Error(Empty),
}

impl Ast {
    #[inline(always)]
    pub fn span(&self) -> Span {
        match self {
            Self::Binding(x) => x.span,
            Self::Cast(x) => x.span,
            Self::Import(x) => x.span,
            Self::Builtin(x) => x.span,
            Self::Comptime(x) => x.span,
            Self::Function(x) => x.span,
            Self::Loop(x) => x.span,
            Self::While(x) => x.span,
            Self::For(x) => x.span,
            Self::Break(x) => x.span,
            Self::Continue(x) => x.span,
            Self::Return(x) => x.span,
            Self::If(x) => x.span,
            Self::Block(x) => x.span,
            Self::Binary(x) => x.span,
            Self::Unary(x) => x.span,
            Self::Subscript(x) => x.span,
            Self::Slice(x) => x.span,
            Self::Call(x) => x.span,
            Self::MemberAccess(x) => x.span,
            Self::Ident(x) => x.span,
            Self::ArrayLiteral(x) => x.span,
            Self::TupleLiteral(x) => x.span,
            Self::StructLiteral(x) => x.span,
            Self::Literal(x) => x.span,
            Self::PointerType(x) => x.span,
            Self::ArrayType(x) => x.span,
            Self::SliceType(x) => x.span,
            Self::StructType(x) => x.span,
            Self::FunctionType(x) => x.span,
            Self::SelfType(x) => x.span,
            Self::Placeholder(x) => x.span,
            Self::Error(x) => x.span,
        }
    }

    #[inline(always)]
    pub fn span_mut(&mut self) -> &mut Span {
        match self {
            Self::Binding(x) => &mut x.span,
            Self::Cast(x) => &mut x.span,
            Self::Import(x) => &mut x.span,
            Self::Builtin(x) => &mut x.span,
            Self::Comptime(x) => &mut x.span,
            Self::Function(x) => &mut x.span,
            Self::Loop(x) => &mut x.span,
            Self::While(x) => &mut x.span,
            Self::For(x) => &mut x.span,
            Self::Break(x) => &mut x.span,
            Self::Continue(x) => &mut x.span,
            Self::Return(x) => &mut x.span,
            Self::If(x) => &mut x.span,
            Self::Block(x) => &mut x.span,
            Self::Binary(x) => &mut x.span,
            Self::Unary(x) => &mut x.span,
            Self::Subscript(x) => &mut x.span,
            Self::Slice(x) => &mut x.span,
            Self::Call(x) => &mut x.span,
            Self::MemberAccess(x) => &mut x.span,
            Self::Ident(x) => &mut x.span,
            Self::ArrayLiteral(x) => &mut x.span,
            Self::TupleLiteral(x) => &mut x.span,
            Self::StructLiteral(x) => &mut x.span,
            Self::Literal(x) => &mut x.span,
            Self::PointerType(x) => &mut x.span,
            Self::ArrayType(x) => &mut x.span,
            Self::SliceType(x) => &mut x.span,
            Self::StructType(x) => &mut x.span,
            Self::FunctionType(x) => &mut x.span,
            Self::SelfType(x) => &mut x.span,
            Self::Placeholder(x) => &mut x.span,
            Self::Error(x) => &mut x.span,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Empty {
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayLiteral {
    pub kind: ArrayLiteralKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TupleLiteral {
    pub elements: Vec<Ast>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayType {
    pub inner: Box<Ast>,
    pub size: Box<Ast>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SliceType {
    pub inner: Box<Ast>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct PointerType {
    pub inner: Box<Ast>,
    pub is_mutable: bool,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Return {
    pub expr: Option<Box<Ast>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct If {
    pub condition: Box<Ast>,
    pub then: Box<Ast>,
    pub otherwise: Option<Box<Ast>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub statements: Vec<Ast>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Binary {
    pub lhs: Box<Ast>,
    pub rhs: Box<Ast>,
    pub op: BinaryOp,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Unary {
    pub op: UnaryOp,
    pub value: Box<Ast>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Subscript {
    pub expr: Box<Ast>,
    pub index: Box<Ast>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Slice {
    pub expr: Box<Ast>,
    pub low: Option<Box<Ast>>,
    pub high: Option<Box<Ast>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructType {
    pub name: Ustr,
    pub fields: Vec<StructTypeField>,
    pub kind: StructTypeKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructTypeField {
    pub name: Ustr,
    pub ty: Ast,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructLiteralField {
    pub name: Ustr,
    pub expr: Ast,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Call {
    pub callee: Box<Ast>,
    pub args: Vec<CallArg>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallArg {
    pub value: Ast,
    pub spread: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct MemberAccess {
    pub expr: Box<Ast>,
    pub member: Ustr,
    pub member_span: Span,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ident {
    pub name: Ustr,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructLiteral {
    pub type_expr: Option<Box<Ast>>,
    pub fields: Vec<StructLiteralField>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ArrayLiteralKind {
    List(Vec<Ast>),
    Fill { len: Box<Ast>, expr: Box<Ast> },
}

#[derive(Debug, PartialEq, Clone)]
pub struct Literal {
    pub kind: LiteralKind,
    pub span: Span,
}

#[derive(strum_macros::IntoStaticStr, Debug, PartialEq, Clone, Copy)]
pub enum LiteralKind {
    Nil,
    Bool(bool),
    Int(i128),
    Float(f64),
    Str(Ustr),
    Char(char),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Import {
    pub path: PathBuf,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Builtin {
    pub kind: BuiltinKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Comptime {
    pub expr: Box<Ast>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BuiltinKind {
    SizeOf(Box<Ast>),
    AlignOf(Box<Ast>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Assign {
    pub lhs: Box<Ast>,
    pub rhs: Box<Ast>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Cast {
    pub expr: Box<Ast>,
    pub target_type: Box<Ast>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Loop {
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct While {
    pub condition: Box<Ast>,
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct For {
    pub iter_binding: NameAndSpan,
    pub index_binding: Option<NameAndSpan>,
    pub iterator: ForIter,
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ForIter {
    Range(Box<Ast>, Box<Ast>),
    Value(Box<Ast>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub sig: FunctionSig,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionSig {
    pub name: Option<Ustr>,
    pub params: Vec<FunctionParam>,
    pub return_type: Option<Box<Ast>>,
    pub varargs: Option<FunctionVarargs>,
    pub kind: FunctionTypeKind,
    pub span: Span,
}

impl FunctionSig {
    pub fn name_or_anonymous(&self) -> Ustr {
        self.name.unwrap_or_else(|| ustr("anonymous"))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionVarargs {
    pub name: NamePat,
    pub type_expr: Option<Box<Ast>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionParam {
    pub pat: Pat,
    pub type_expr: Option<Box<Ast>>,
    pub default_value: Option<Box<Ast>>,
}

impl ToString for FunctionParam {
    fn to_string(&self) -> String {
        self.pat.to_string()
    }
}

#[derive(Hash, Debug, Eq, PartialEq, Clone)]
pub enum ExternLibrary {
    System(String),
    Path(ExternLibraryPath),
}

#[derive(Hash, Debug, Eq, PartialEq, Clone)]
pub struct ExternLibraryPath {
    path: PathBuf,
}

impl Deref for ExternLibraryPath {
    type Target = PathBuf;

    fn deref(&self) -> &Self::Target {
        &self.path
    }
}

impl ToString for ExternLibraryPath {
    fn to_string(&self) -> String {
        self.path.to_str().unwrap().to_string()
    }
}

impl ExternLibraryPath {
    pub fn lib_dir(&self) -> &Path {
        self.path.parent().unwrap()
    }

    pub fn lib_name(&self) -> &OsStr {
        self.path.file_name().unwrap()
    }
}

impl ExternLibrary {
    pub fn try_from_str(s: &str, relative_to: &RelativeTo<'_>, span: Span) -> DiagnosticResult<Self> {
        let path = Path::new(s);

        if s.starts_with(&['.', '/', '\\']) || path.has_root() || path.extension().is_some() {
            try_resolve_relative_path(path, relative_to, Some(span)).map(|path| Self::Path(ExternLibraryPath { path }))
        } else {
            Ok(Self::System(s.to_string()))
        }
    }

    #[allow(unused)]
    pub fn from_str(s: &str, relative_to: &RelativeTo<'_>) -> Self {
        let path = Path::new(s);

        if s.starts_with(&['.', '/', '\\']) || path.has_root() || path.extension().is_some() {
            Self::Path(ExternLibraryPath {
                path: resolve_relative_path(path, relative_to),
            })
        } else {
            Self::System(s.to_string())
        }
    }

    pub fn path(&self) -> String {
        match self {
            ExternLibrary::System(lib) => lib.clone(),
            ExternLibrary::Path(path) => path.to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Binding {
    pub attrs: Vec<Attr>,
    pub vis: Vis,
    pub kind: BindingKind,
    pub span: Span,
}

impl Binding {
    #[allow(unused)]
    pub fn debug_name(&self) -> String {
        match &self.kind {
            BindingKind::Let { pat, .. } => pat.to_string(),
            BindingKind::Function { name, .. }
            | BindingKind::ExternFunction { name, .. }
            | BindingKind::ExternVariable { name, .. }
            | BindingKind::Type { name, .. } => name.name.to_string(),
        }
    }

    pub fn pat_span(&self) -> Span {
        match &self.kind {
            BindingKind::Let { pat, .. } => pat.span(),
            BindingKind::Function {
                name: NameAndSpan { span, .. },
                ..
            }
            | BindingKind::ExternFunction {
                name: NameAndSpan { span, .. },
                ..
            }
            | BindingKind::ExternVariable {
                name: NameAndSpan { span, .. },
                ..
            }
            | BindingKind::Type {
                name: NameAndSpan { span, .. },
                ..
            } => *span,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum BindingKind {
    Let {
        pat: Pat,
        type_expr: Option<Box<Ast>>,
        value: Box<Ast>,
    },
    Function {
        name: NameAndSpan,
        sig: FunctionSig,
        body: Box<Ast>,
    },
    ExternFunction {
        name: NameAndSpan,
        sig: FunctionSig,
    },
    ExternVariable {
        name: NameAndSpan,
        is_mutable: bool,
        type_expr: Box<Ast>,
    },
    Type {
        name: NameAndSpan,
        type_expr: Box<Ast>,
    },
}

impl Display for BindingKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BindingKind::Let { .. } => "orphan",
                BindingKind::Function { .. } => "function",
                BindingKind::ExternFunction { .. } => "extern function",
                BindingKind::ExternVariable { .. } => "extern variable",
                BindingKind::Type { .. } => "type",
            }
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct NameAndSpan {
    pub name: Ustr,
    pub span: Span,
}

impl NameAndSpan {
    pub fn new(name: Ustr, span: Span) -> Self {
        Self { name, span }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Vis {
    Private,
    Public,
}

impl Default for Vis {
    fn default() -> Self {
        Self::Private
    }
}

impl Display for Vis {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Vis::Private => write!(f, "private"),
            Vis::Public => write!(f, "public"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Attr {
    pub name: NameAndSpan,
    pub value: Option<Box<Ast>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    Shl,
    Shr,
    BitAnd,
    BitOr,
    BitXor,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    AndAssign,
    OrAssign,
    ShlAssign,
    ShrAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use BinaryOp::*;
        write!(
            f,
            "{}",
            match self {
                Add => "+",
                Sub => "-",
                Mul => "*",
                Div => "/",
                Rem => "%",
                Eq => "==",
                Ne => "!=",
                Lt => "<",
                Le => "<=",
                Gt => ">",
                Ge => ">=",
                And => "&&",
                Or => "||",
                Shl => "<<",
                Shr => ">>",
                BitAnd => "&",
                BitOr => "|",
                BitXor => "^",
                Assign => "=",
                AddAssign => "+=",
                SubAssign => "-=",
                MulAssign => "*=",
                DivAssign => "/=",
                RemAssign => "%=",
                AndAssign => "&&=",
                OrAssign => "||=",
                ShlAssign => "<<=",
                ShrAssign => ">>=",
                BitAndAssign => "&=",
                BitOrAssign => "|=",
                BitXorAssign => "^=",
            }
        )
    }
}

impl BinaryOp {
    pub fn is_assignment(&self) -> bool {
        match self {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::Rem
            | BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Lt
            | BinaryOp::Le
            | BinaryOp::Gt
            | BinaryOp::Ge
            | BinaryOp::And
            | BinaryOp::Or
            | BinaryOp::Shl
            | BinaryOp::Shr
            | BinaryOp::BitAnd
            | BinaryOp::BitOr
            | BinaryOp::BitXor => false,

            BinaryOp::Assign
            | BinaryOp::AddAssign
            | BinaryOp::SubAssign
            | BinaryOp::MulAssign
            | BinaryOp::DivAssign
            | BinaryOp::RemAssign
            | BinaryOp::AndAssign
            | BinaryOp::OrAssign
            | BinaryOp::ShlAssign
            | BinaryOp::ShrAssign
            | BinaryOp::BitAndAssign
            | BinaryOp::BitOrAssign
            | BinaryOp::BitXorAssign => true,
        }
    }

    pub fn precedence(&self) -> usize {
        match self {
            Self::Mul | Self::Div | Self::Rem => 100,

            Self::Add | Self::Sub => 90,

            Self::Shl | Self::Shr => 85,

            Self::Eq | Self::Ne | Self::Lt | Self::Le | Self::Gt | Self::Ge => 80,

            Self::BitAnd => 73,
            Self::BitXor => 72,
            Self::BitOr => 71,
            Self::Or => 70,
            Self::And => 69,

            Self::Assign
            | Self::AddAssign
            | Self::SubAssign
            | Self::MulAssign
            | Self::DivAssign
            | Self::RemAssign
            | Self::AndAssign
            | Self::OrAssign
            | Self::ShlAssign
            | Self::ShrAssign
            | Self::BitAndAssign
            | Self::BitOrAssign
            | Self::BitXorAssign => 50,
        }
    }

    pub fn inner(&self) -> Self {
        match self {
            Self::AddAssign => Self::Add,
            Self::SubAssign => Self::Sub,
            Self::MulAssign => Self::Mul,
            Self::DivAssign => Self::Div,
            Self::RemAssign => Self::Rem,
            Self::AndAssign => Self::And,
            Self::OrAssign => Self::Or,
            Self::ShlAssign => Self::Shl,
            Self::ShrAssign => Self::Shr,
            Self::BitAndAssign => Self::BitAnd,
            Self::BitOrAssign => Self::BitOr,
            Self::BitXorAssign => Self::BitXor,
            op => panic!("non-compound {} doesn't have an inner operator", op),
        }
    }
}

#[derive(strum_macros::IntoStaticStr, Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp {
    Ref(bool),
    Deref,
    Neg,
    Plus,
    Not,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnaryOp::Ref(is_mutable) => format!("&{}", if *is_mutable { "mut " } else { "" }),
                UnaryOp::Deref => "*".to_string(),
                UnaryOp::Neg => "-".to_string(),
                UnaryOp::Plus => "+".to_string(),
                UnaryOp::Not => "!".to_string(),
            }
        )
    }
}

impl fmt::Display for FunctionSig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fn: {} ({}{})",
            self.name_or_anonymous(),
            self.params
                .iter()
                .map(|p| p.to_string())
                .collect::<Vec<String>>()
                .join(", "),
            match &self.varargs {
                Some(v) => format!(", ..{}", v.name),
                None => "".to_string(),
            }
        )
    }
}

impl fmt::Display for LiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LiteralKind::Nil => "nil".to_string(),
                LiteralKind::Bool(v) => v.to_string(),
                LiteralKind::Int(v) => v.to_string(),
                LiteralKind::Float(v) => v.to_string(),
                LiteralKind::Str(v) => format!("\"{}\"", v),
                LiteralKind::Char(v) => format!("'{}'", v),
            }
        )
    }
}
