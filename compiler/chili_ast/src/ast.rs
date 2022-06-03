use crate::{
    const_value::ConstValue,
    path::{try_resolve_relative_path, RelativeTo},
    pattern::Pattern,
    ty::*,
    workspace::{BindingInfoId, ModuleId, ModuleInfo},
};
use chili_error::DiagnosticResult;
use chili_span::{Span, Spanned};
use chili_token::TokenKind;
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
    pub module_id: ModuleId,
    pub module_info: ModuleInfo,
    pub imports: Vec<Import>,
    pub bindings: Vec<Binding>,
    pub run_exprs: Vec<Expr>,
}

impl Ast {
    pub fn new(module_info: ModuleInfo) -> Self {
        Self {
            module_id: Default::default(),
            module_info,
            imports: vec![],
            bindings: vec![],
            run_exprs: vec![],
        }
    }
}

#[derive(Default)]
pub struct TypedAst {
    pub bindings: Vec<Binding>,
    pub imports: Vec<Import>,
    ids_to_decls: HashMap<BindingInfoId, AstDeclIndex>,
}

enum AstDeclIndex {
    Binding(usize),
    Import(usize),
}

#[derive(Debug, Clone)]
pub enum AstDecl<'a> {
    Binding(&'a Binding),
    Import(&'a Import),
}

impl TypedAst {
    pub fn get_decl(&self, id: BindingInfoId) -> Option<AstDecl> {
        self.ids_to_decls.get(&id).map(|decl| match decl {
            AstDeclIndex::Import(idx) => AstDecl::Import(&self.imports[*idx]),
            AstDeclIndex::Binding(idx) => AstDecl::Binding(&self.bindings[*idx]),
        })
    }

    pub fn push_binding(&mut self, ids: &[BindingInfoId], binding: Binding) {
        self.bindings.push(binding);
        let idx = self.bindings.len() - 1;
        for id in ids {
            self.ids_to_decls.insert(*id, AstDeclIndex::Binding(idx));
        }
    }

    pub fn push_import(&mut self, ids: &[BindingInfoId], import: Import) {
        self.imports.push(import);
        let idx = self.imports.len() - 1;
        for id in ids {
            self.ids_to_decls.insert(*id, AstDeclIndex::Import(idx));
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Ty,
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

    pub fn typed(kind: ExprKind, ty: Ty, span: Span) -> Self {
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
    Import(Vec<Import>),
    Extern(Vec<Binding>),
    Binding(Box<Binding>),
    Defer(Defer),
    Assign(Assign),
    Cast(Cast),
    Builtin(BuiltinKind),
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
    Error,
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
    pub kind: StructTyKind,
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
    pub fn into_expr(self, ty: Ty, span: Span) -> Expr {
        Expr {
            kind: ExprKind::Literal(Literal { kind: self, span }),
            ty,
            span,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum BuiltinKind {
    Import(PathBuf),
    LangItem(Ustr),
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
    pub target_ty: Ty,
}

#[derive(Debug, PartialEq, Clone)]
pub struct While {
    pub cond: Box<Expr>,
    pub block: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub struct For {
    pub iter_name: Ustr,
    pub iter_id: BindingInfoId,
    pub iter_index_name: Ustr,
    pub iter_index_id: BindingInfoId,
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
    pub binding_info_id: Option<BindingInfoId>,
    pub is_entry_point: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionSig {
    pub name: Ustr,
    pub params: Vec<FunctionParam>,
    pub variadic: bool,
    pub ret: Option<Box<Expr>>,
    pub kind: FunctionKind,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionParam {
    pub pattern: Pattern,
    pub ty_expr: Option<Box<Expr>>,
    pub ty: Ty,
}

impl ToString for FunctionParam {
    fn to_string(&self) -> String {
        self.pattern.to_string()
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum FunctionKind {
    Orphan,
    Extern { lib: Ustr },
}

impl FunctionKind {
    pub fn is_orphan(&self) -> bool {
        matches!(self, FunctionKind::Orphan)
    }
    pub fn is_extern(&self) -> bool {
        matches!(self, FunctionKind::Extern { .. })
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
        from: &str,
        relative_to: RelativeTo<'_>,
        span: Span,
    ) -> DiagnosticResult<Self> {
        const SYSTEM_PREFIX: &str = "system:";

        if from.starts_with(SYSTEM_PREFIX) {
            let split: Vec<&str> = from.split(SYSTEM_PREFIX).collect();
            let lib = split[1].to_string();
            Ok(ExternLibrary::System(lib))
        } else {
            let path = try_resolve_relative_path(Path::new(from), relative_to, Some(span))?;
            Ok(ExternLibrary::Path(ExternLibraryPath { path }))
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
    pub ty: Ty,
    pub expr: Option<Expr>,
    pub lib_name: Option<Ustr>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BindingKind {
    Value,
    Import,
}

impl BindingKind {
    pub fn is_value(&self) -> bool {
        matches!(self, BindingKind::Value)
    }

    pub fn is_import(&self) -> bool {
        matches!(self, BindingKind::Import)
    }
}

impl Display for BindingKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BindingKind::Value => "value",
                BindingKind::Import => "import",
            }
        )
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

#[derive(Debug, PartialEq, Clone)]
pub struct Import {
    // The import binding module id, during `resolve` phase
    pub module_id: ModuleId,

    // The binding info id assigned during `check` phase, binds to `alias`
    pub binding_info_id: BindingInfoId,

    // The resolved module id, during `check` phase
    pub target_module_id: ModuleId,

    // The module info resolved when generating the ast
    pub target_module_info: ModuleInfo,

    // If this import contains a path, this is the resolved binding info id, during `check` phase
    pub target_binding_info_id: Option<BindingInfoId>,
    pub alias: Ustr,
    pub import_path: ImportPath,
    pub visibility: Visibility,
    pub span: Span,
}

impl Import {
    pub fn path_span(&self) -> Span {
        if self.import_path.is_empty() {
            self.span
        } else {
            self.import_path.last().unwrap().span
        }
    }

    pub fn is_glob(&self) -> bool {
        if self.import_path.is_empty() {
            false
        } else {
            self.import_path.last().unwrap().value.is_glob()
        }
    }

    pub fn import_path_str(&self) -> String {
        if self.import_path.is_empty() {
            self.target_module_info.name.to_string()
        } else {
            format!(
                "{}.{}",
                self.target_module_info.name,
                self.import_path
                    .iter()
                    .map(|p| p.value.to_string())
                    .collect::<Vec<String>>()
                    .join(".")
            )
        }
    }
}

pub type ImportPath = Vec<Spanned<ImportPathNode>>;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ImportPathNode {
    Symbol(Ustr),
    Glob,
}

impl ImportPathNode {
    pub fn as_symbol(&self) -> Ustr {
        match self {
            ImportPathNode::Symbol(s) => *s,
            _ => panic!(),
        }
    }

    pub fn is_glob(&self) -> bool {
        matches!(self, ImportPathNode::Glob)
    }
}

impl ToString for ImportPathNode {
    fn to_string(&self) -> String {
        match self {
            ImportPathNode::Symbol(s) => s.to_string(),
            ImportPathNode::Glob => String::from("?"),
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
            if self.variadic { ", .." } else { "" }
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
