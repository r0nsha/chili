use super::{
    const_value::ConstValue,
    path::{try_resolve_relative_path, RelativeTo},
    pattern::Pattern,
    ty::*,
    workspace::{BindingInfoId, ModuleId, ModuleInfo},
};
use crate::error::DiagnosticResult;
use crate::span::{FileId, Span};
use crate::token::TokenKind;
use slab::Slab;
use std::{
    collections::HashMap,
    ffi::OsStr,
    fmt::{self, Display},
    ops::Deref,
    path::{Path, PathBuf},
};
use ustr::Ustr;

#[derive(Debug, Clone)]
pub struct Ast {
    pub file_id: FileId,
    pub module_id: ModuleId,
    pub module_info: ModuleInfo,
    pub bindings: Vec<Binding>,
    pub run_exprs: Vec<Expr>,
}

impl Ast {
    pub fn new(file_id: FileId, module_info: ModuleInfo) -> Self {
        Self {
            file_id,
            module_id: Default::default(),
            module_info,
            bindings: vec![],
            run_exprs: vec![],
        }
    }
}

#[derive(Default)]
pub struct TypedAst {
    pub bindings: Slab<Binding>,
    // pub functions: Slab<Function>,
    // pub extern_functions: Slab<ExternFunction>,
    ids_to_bindings: HashMap<BindingInfoId, usize>,
}

impl TypedAst {
    pub fn get_binding(&self, id: BindingInfoId) -> Option<&Binding> {
        self.ids_to_bindings
            .get(&id)
            .map(|idx| &self.bindings[*idx])
    }

    #[allow(unused)]
    pub fn has_binding(&self, id: BindingInfoId) -> bool {
        self.ids_to_bindings.contains_key(&id)
    }

    pub fn push_binding(&mut self, ids: &[BindingInfoId], binding: Binding) {
        self.bindings.insert(binding);
        let idx = self.bindings.len() - 1;

        for id in ids {
            self.ids_to_bindings.insert(*id, idx);
        }
    }

    // pub fn get_function(&mut self, id: FunctionId) -> Option<&Function> {
    //     self.functions.get(id.inner())
    // }

    // pub fn push_function(&mut self, function: Function) -> FunctionId {
    //     FunctionId(self.functions.insert(function))
    // }

    // pub fn get_extern_function(&mut self, id: FunctionId) -> Option<&ExternFunction> {
    //     self.extern_functions.get(id.inner())
    // }

    // pub fn push_extern_function(&mut self, function: ExternFunction) -> ExternFunctionId {
    //     ExternFunctionId(self.extern_functions.insert(function))
    // }
}

define_id_type!(FunctionId);
define_id_type!(ExternFunctionId);

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: TypeId,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self {
            kind,
            ty: Default::default(),
            span,
        }
    }

    pub fn typed(kind: ExprKind, ty: TypeId, span: Span) -> Self {
        Self { kind, ty, span }
    }

    pub fn is_function(&self) -> bool {
        matches!(&self.kind, ExprKind::Function(..))
    }

    pub fn as_function(&self) -> &Function {
        match &self.kind {
            ExprKind::Function(func) => func,
            _ => panic!(),
        }
    }

    pub fn as_function_mut(&mut self) -> &mut Function {
        match &mut self.kind {
            ExprKind::Function(func) => func,
            _ => panic!(),
        }
    }

    pub fn is_function_type(&self) -> bool {
        matches!(&self.kind, ExprKind::FunctionType(..))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprKind {
    Binding(Box<Binding>),
    Defer(Defer),
    Assign(Assign),
    Cast(Cast),
    Builtin(Builtin),
    Function(Function),
    While(While),
    For(For),
    Break(Terminator),
    Continue(Terminator),
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
    MultiPointerType(ExprAndMut),
    ArrayType(ArrayType),
    SliceType(ExprAndMut),
    StructType(StructType),
    FunctionType(FunctionSig),
    SelfType,
    Placeholder,
    ConstValue(ConstValue),
    Error(Span),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayLiteral {
    pub kind: ArrayLiteralKind,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Defer {
    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TupleLiteral {
    pub elements: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayType {
    pub inner: Box<Expr>,
    pub size: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExprAndMut {
    pub inner: Box<Expr>,
    pub is_mutable: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Terminator {
    pub deferred: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Return {
    pub expr: Option<Box<Expr>>,
    pub deferred: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct If {
    pub cond: Box<Expr>,
    pub then: Box<Expr>,
    pub otherwise: Option<Box<Expr>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub exprs: Vec<Expr>,
    pub deferred: Vec<Expr>,
    pub yields: bool,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Binary {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: BinaryOp,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Unary {
    pub op: UnaryOp,
    pub lhs: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Subscript {
    pub expr: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Slice {
    pub expr: Box<Expr>,
    pub low: Option<Box<Expr>>,
    pub high: Option<Box<Expr>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructType {
    pub name: Ustr,
    pub fields: Vec<StructTypeField>,
    pub kind: StructTypeKind,
    pub binding_info_id: BindingInfoId,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructTypeField {
    pub name: Ustr,
    pub ty: Expr,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructLiteralField {
    pub symbol: Ustr,
    pub expr: Expr,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Call {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct MemberAccess {
    pub expr: Box<Expr>,
    pub member: Ustr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ident {
    pub symbol: Ustr,
    pub binding_info_id: BindingInfoId,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructLiteral {
    pub type_expr: Option<Box<Expr>>,
    pub fields: Vec<StructLiteralField>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ArrayLiteralKind {
    List(Vec<Expr>),
    Fill { len: Box<Expr>, expr: Box<Expr> },
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

impl LiteralKind {
    pub fn into_expr(self, ty: TypeId, span: Span) -> Expr {
        Expr {
            kind: ExprKind::Literal(Literal { kind: self, span }),
            ty,
            span,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Builtin {
    Import(PathBuf),
    SizeOf(Box<Expr>),
    AlignOf(Box<Expr>),
    Panic(Option<Box<Expr>>),
    Run(Box<Expr>, Option<ConstValue>), // 1. expression to run | 2. the expression's result
}

#[derive(Debug, PartialEq, Clone)]
pub struct Assign {
    pub lvalue: Box<Expr>,
    pub rvalue: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Cast {
    pub ty_expr: Option<Box<Expr>>,
    pub expr: Box<Expr>,
    pub target_ty: TypeId,
}

#[derive(Debug, PartialEq, Clone)]
pub struct While {
    pub cond: Box<Expr>,
    pub block: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub struct For {
    pub iter_binding: NameAndId,
    pub index_binding: Option<NameAndId>,
    pub iterator: ForIter,
    pub block: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ForIter {
    Range(Box<Expr>, Box<Expr>),
    Value(Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub sig: FunctionSig,
    pub body: Block,
    pub id: Option<BindingInfoId>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionSig {
    pub name: Ustr,
    pub params: Vec<FunctionParam>,
    pub ret: Option<Box<Expr>>,
    pub varargs: Option<FunctionVarargs>,
    pub kind: FunctionKind,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionVarargs {
    pub name: Ustr,
    pub ty: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionParam {
    pub pattern: Pattern,
    pub ty_expr: Option<Box<Expr>>,
    pub ty: TypeId,
}

impl ToString for FunctionParam {
    fn to_string(&self) -> String {
        self.pattern.to_string()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FunctionKind {
    Orphan,
    Extern { lib: Option<ExternLibrary> },
    Intrinsic(Intrinsic),
}

impl FunctionKind {
    pub fn is_orphan(&self) -> bool {
        matches!(self, FunctionKind::Orphan)
    }

    pub fn is_extern(&self) -> bool {
        matches!(self, FunctionKind::Extern { .. })
    }

    pub fn is_intrinsic(&self) -> bool {
        matches!(self, FunctionKind::Intrinsic(_))
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
    pub pattern: Pattern,
    pub ty_expr: Option<Expr>,
    pub ty: TypeId,
    pub expr: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BindingKind {
    Normal,
    Intrinsic(Intrinsic),
    Extern(Option<ExternLibrary>),
}

impl BindingKind {
    pub fn is_normal(&self) -> bool {
        matches!(self, BindingKind::Normal)
    }

    pub fn is_extern(&self) -> bool {
        matches!(self, BindingKind::Extern(_))
    }
}

impl Display for BindingKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BindingKind::Normal => "normal",
                BindingKind::Intrinsic(_) => "intrinsic",
                BindingKind::Extern(_) => "extern",
            }
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
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
}

impl Display for Intrinsic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Intrinsic::StartWorkspace => "start_workspace",
            }
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct NameAndId {
    pub name: Ustr,
    pub id: BindingInfoId,
}

impl NameAndId {
    pub fn new(name: Ustr) -> Self {
        Self {
            name,
            id: BindingInfoId::unknown(),
        }
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

    pub fn is_public(&self) -> bool {
        matches!(self, Visibility::Public)
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
    Neq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    And,
    Or,
    Shl,
    Shr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
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
                Neq => "!=",
                Lt => "<",
                LtEq => "<=",
                Gt => ">",
                GtEq => ">=",
                And => "&&",
                Or => "||",
                Shl => "<<",
                Shr => ">>",
                BitwiseAnd => "&",
                BitwiseOr => "|",
                BitwiseXor => "^",
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
            BangEq => BinaryOp::Neq,
            Lt => BinaryOp::Lt,
            LtEq => BinaryOp::LtEq,
            Gt => BinaryOp::Gt,
            GtEq => BinaryOp::GtEq,
            AmpAmp | AmpAmpEq => BinaryOp::And,
            BarBar | BarBarEq => BinaryOp::Or,
            LtLt | LtLtEq => BinaryOp::Shl,
            GtGt | GtGtEq => BinaryOp::Shr,
            Amp | AmpEq => BinaryOp::BitwiseAnd,
            Bar | BarEq => BinaryOp::BitwiseOr,
            Caret | CaretEq => BinaryOp::BitwiseXor,
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
                .map(|a| a.to_string())
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
