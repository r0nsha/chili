pub mod pattern;

use crate::{
    common::path::{try_resolve_relative_path, RelativeTo},
    define_id_type,
    error::DiagnosticResult,
    span::{FileId, Span},
    token::TokenKind,
    types::*,
    workspace::{ModuleId, ModuleInfo},
};
use paste::paste;
use pattern::Pattern;
use std::{
    ffi::OsStr,
    fmt::{self, Display},
    ops::Deref,
    path::{Path, PathBuf},
};
use ustr::Ustr;

#[derive(Debug, Clone)]
pub struct Module {
    pub id: ModuleId,
    pub info: ModuleInfo,
    pub file_id: FileId,
    pub bindings: Vec<Binding>,
    pub run_exprs: Vec<Ast>,
}

impl Module {
    pub fn new(file_id: FileId, module_info: ModuleInfo) -> Self {
        Self {
            file_id,
            id: Default::default(),
            info: module_info,
            bindings: vec![],
            run_exprs: vec![],
        }
    }
}

define_id_type!(FunctionId);

#[derive(Debug, PartialEq, Clone)]
pub enum Ast {
    Binding(Binding),
    Assignment(Assignment),
    Cast(Cast),
    Builtin(Builtin),
    Function(Function),
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
    PointerType(ExprAndMut),
    ArrayType(ArrayType),
    SliceType(ExprAndMut),
    StructType(StructType),
    FunctionType(FunctionSig),
    SelfType(Empty),
    Placeholder(Empty),
    Error(Empty),
}

macro_rules! ast_field_dispatch {
    ($field:ident, $ty:ty) => {
        impl Ast {
            #[inline(always)]
            pub fn $field(&self) -> $ty {
                match self {
                    Self::Binding(x) => x.$field,
                    Self::Assignment(x) => x.$field,
                    Self::Cast(x) => x.$field,
                    Self::Builtin(x) => x.$field,
                    Self::Function(x) => x.$field,
                    Self::While(x) => x.$field,
                    Self::For(x) => x.$field,
                    Self::Break(x) => x.$field,
                    Self::Continue(x) => x.$field,
                    Self::Return(x) => x.$field,
                    Self::If(x) => x.$field,
                    Self::Block(x) => x.$field,
                    Self::Binary(x) => x.$field,
                    Self::Unary(x) => x.$field,
                    Self::Subscript(x) => x.$field,
                    Self::Slice(x) => x.$field,
                    Self::Call(x) => x.$field,
                    Self::MemberAccess(x) => x.$field,
                    Self::Ident(x) => x.$field,
                    Self::ArrayLiteral(x) => x.$field,
                    Self::TupleLiteral(x) => x.$field,
                    Self::StructLiteral(x) => x.$field,
                    Self::Literal(x) => x.$field,
                    Self::PointerType(x) => x.$field,
                    Self::ArrayType(x) => x.$field,
                    Self::SliceType(x) => x.$field,
                    Self::StructType(x) => x.$field,
                    Self::FunctionType(x) => x.$field,
                    Self::SelfType(x) => x.$field,
                    Self::Placeholder(x) => x.$field,
                    Self::Error(x) => x.$field,
                }
            }

            paste! {
                #[inline(always)]
                pub fn [< $field:snake _mut >](&mut self) -> &mut $ty {
                    match self {
                        Self::Binding(x) => &mut x.$field,
                        Self::Assignment(x) => &mut x.$field,
                        Self::Cast(x) => &mut x.$field,
                        Self::Builtin(x) => &mut x.$field,
                        Self::Function(x) => &mut x.$field,
                        Self::While(x) => &mut x.$field,
                        Self::For(x) => &mut x.$field,
                        Self::Break(x) => &mut x.$field,
                        Self::Continue(x) => &mut x.$field,
                        Self::Return(x) => &mut x.$field,
                        Self::If(x) => &mut x.$field,
                        Self::Block(x) => &mut x.$field,
                        Self::Binary(x) => &mut x.$field,
                        Self::Unary(x) => &mut x.$field,
                        Self::Subscript(x) => &mut x.$field,
                        Self::Slice(x) => &mut x.$field,
                        Self::Call(x) => &mut x.$field,
                        Self::MemberAccess(x) => &mut x.$field,
                        Self::Ident(x) => &mut x.$field,
                        Self::ArrayLiteral(x) => &mut x.$field,
                        Self::TupleLiteral(x) => &mut x.$field,
                        Self::StructLiteral(x) => &mut x.$field,
                        Self::Literal(x) => &mut x.$field,
                        Self::PointerType(x) => &mut x.$field,
                        Self::ArrayType(x) => &mut x.$field,
                        Self::SliceType(x) => &mut x.$field,
                        Self::StructType(x) => &mut x.$field,
                        Self::FunctionType(x) => &mut x.$field,
                        Self::SelfType(x) => &mut x.$field,
                        Self::Placeholder(x) => &mut x.$field,
                        Self::Error(x) => &mut x.$field,
                    }
                }
            }
        }
    };
}

ast_field_dispatch!(span, Span);

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
pub struct ExprAndMut {
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
    pub yields: bool,
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
    pub args: Vec<Ast>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct MemberAccess {
    pub expr: Box<Ast>,
    pub member: Ustr,
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
    Int(i64),
    Float(f64),
    Str(Ustr),
    Char(char),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Builtin {
    pub kind: BuiltinKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BuiltinKind {
    Import(PathBuf),
    SizeOf(Box<Ast>),
    AlignOf(Box<Ast>),
    Panic(Option<Box<Ast>>),
    Run(Box<Ast>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Assignment {
    pub lhs: Box<Ast>,
    pub rhs: Box<Ast>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Cast {
    pub expr: Box<Ast>,
    pub target: Option<Box<Ast>>,
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
    pub name: Ustr,
    pub params: Vec<FunctionParam>,
    pub return_type: Option<Box<Ast>>,
    pub varargs: Option<FunctionVarargs>,
    pub kind: FunctionTypeKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionVarargs {
    pub name: Ustr,
    pub type_expr: Option<Box<Ast>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionParam {
    pub pattern: Pattern,
    pub type_expr: Option<Box<Ast>>,
}

impl ToString for FunctionParam {
    fn to_string(&self) -> String {
        self.pattern.to_string()
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
    pub fn try_from_str(
        s: &str,
        relative_to: RelativeTo<'_>,
        span: Span,
    ) -> DiagnosticResult<Self> {
        let path = Path::new(s);

        if path.extension().is_some() {
            let path = try_resolve_relative_path(Path::new(s), relative_to, Some(span))?;
            Ok(ExternLibrary::Path(ExternLibraryPath { path }))
        } else {
            let lib = s.to_string();
            Ok(ExternLibrary::System(lib))
        }
    }

    pub fn from_str(from: &str, relative_to: RelativeTo<'_>) -> Option<Self> {
        Self::try_from_str(from, relative_to, Span::unknown()).ok()
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
    pub module_id: ModuleId,
    pub visibility: Visibility,
    pub kind: BindingKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BindingKind {
    Orphan {
        pattern: Pattern,
        type_expr: Option<Box<Ast>>,
        value: Box<Ast>,
    },
    ExternFunction {
        name: NameAndSpan,
        lib: Option<ExternLibrary>,
        function_type: FunctionSig,
    },
    ExternVariable {
        name: NameAndSpan,
        lib: Option<ExternLibrary>,
        is_mutable: bool,
        type_expr: Box<Ast>,
    },
    Intrinsic {
        name: NameAndSpan,
        intrinsic: Intrinsic,
        function_type: FunctionSig,
    },
}

impl Display for BindingKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BindingKind::Orphan { .. } => "orphan",
                BindingKind::ExternFunction { .. } => "extern function",
                BindingKind::ExternVariable { .. } => "extern variable",
                BindingKind::Intrinsic { .. } => "intrinsic",
            }
        )
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum Intrinsic {
    StartWorkspace,
}

impl Intrinsic {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "start_workspace" => Some(Intrinsic::StartWorkspace),
            _ => None,
        }
    }

    pub fn to_str(&self) -> &str {
        match self {
            Intrinsic::StartWorkspace => "start_workspace",
        }
    }
}

impl Display for Intrinsic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_str())
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Visibility {
    Private,
    Public,
}

impl Visibility {
    pub fn is_private(&self) -> bool {
        matches!(self, Visibility::Private)
    }
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
            }
        )
    }
}

impl From<TokenKind> for BinaryOp {
    fn from(kind: TokenKind) -> Self {
        use TokenKind::*;
        match kind {
            Plus | PlusEq => BinaryOp::Add,
            Minus | MinusEq => BinaryOp::Sub,
            Star | StarEq => BinaryOp::Mul,
            FwSlash | FwSlashEq => BinaryOp::Div,
            Percent | PercentEq => BinaryOp::Rem,
            EqEq => BinaryOp::Eq,
            BangEq => BinaryOp::Ne,
            Lt => BinaryOp::Lt,
            LtEq => BinaryOp::Le,
            Gt => BinaryOp::Gt,
            GtEq => BinaryOp::Ge,
            AmpAmp | AmpAmpEq => BinaryOp::And,
            BarBar | BarBarEq => BinaryOp::Or,
            LtLt | LtLtEq => BinaryOp::Shl,
            GtGt | GtGtEq => BinaryOp::Shr,
            Amp | AmpEq => BinaryOp::BitAnd,
            Bar | BarEq => BinaryOp::BitOr,
            Caret | CaretEq => BinaryOp::BitXor,
            _ => panic!("{} is not a binary op", kind),
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

impl From<TokenKind> for UnaryOp {
    fn from(kind: TokenKind) -> Self {
        use TokenKind::*;
        match kind {
            Amp => UnaryOp::Ref(false),
            Minus => UnaryOp::Neg,
            Plus => UnaryOp::Plus,
            Bang => UnaryOp::Not,
            _ => panic!("{} is not a unary op", kind),
        }
    }
}

impl fmt::Display for FunctionSig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fn: {} ({}{})",
            self.name,
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
