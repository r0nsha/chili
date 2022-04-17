use crate::{
    path::resolve_relative_path,
    pattern::Pattern,
    ty::*,
    workspace::{BindingInfoId, ModuleId, ModuleInfo},
};
use chili_error::DiagnosticResult;
use chili_span::{Span, Spanned};
use chili_token::TokenKind;
use std::{
    collections::HashMap,
    fmt::{self, Display},
    path::Path,
};
use ustr::Ustr;

#[derive(Debug, Clone)]
pub struct Ast {
    pub module_id: ModuleId,
    pub module_info: ModuleInfo,
    pub imports: Vec<Import>,
    pub bindings: Vec<Binding>,
}

impl Ast {
    pub fn new(module_info: ModuleInfo) -> Self {
        Self {
            module_id: Default::default(),
            module_info,
            imports: Default::default(),
            bindings: Default::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedAst {
    pub imports: HashMap<BindingInfoId, Import>,
    pub bindings: HashMap<BindingInfoId, Binding>,
}

impl TypedAst {
    pub fn new() -> Self {
        Self {
            imports: Default::default(),
            bindings: Default::default(),
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

    pub fn is_fn(&self) -> bool {
        match &self.kind {
            ExprKind::Fn(..) => true,
            _ => false,
        }
    }

    pub fn as_fn(&self) -> &Fn {
        match &self.kind {
            ExprKind::Fn(func) => func,
            _ => panic!(),
        }
    }

    pub fn as_fn_mut(&mut self) -> &mut Fn {
        match &mut self.kind {
            ExprKind::Fn(func) => func,
            _ => panic!(),
        }
    }

    pub fn is_fn_type(&self) -> bool {
        match &self.kind {
            ExprKind::FnType(..) => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprKind {
    Import(Vec<Import>),
    Foreign(Vec<Binding>),
    Binding(Box<Binding>),
    Defer(Box<Expr>),
    Assign(Assign),
    Cast(Cast),
    Builtin(Builtin),
    Fn(Fn),
    While(While),
    For(For),
    Break(Deferred),
    Continue(Deferred),
    Return(Return),
    If(If),
    Block(Block),
    Binary(Binary),
    Unary(Unary),
    Subscript(Subscript),
    Slice(Slice),
    FnCall(FnCall),
    MemberAccess(MemberAccess),
    Ident(Ident),
    ArrayLiteral(ArrayLiteralKind),
    TupleLiteral(Vec<Expr>),
    StructLiteral(StructLiteral),
    Literal(Literal),
    PointerType(Box<Expr>, bool),
    MultiPointerType(Box<Expr>, bool),
    ArrayType(Box<Expr>, Box<Expr>),
    SliceType(Box<Expr>, bool),
    StructType(StructType),
    FnType(FnSig),
    SelfType,
    NeverType,
    UnitType,
    PlaceholderType,
    Error,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Deferred {
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
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnCall {
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

#[derive(strum_macros::IntoStaticStr, Debug, PartialEq, Clone)]
pub enum Literal {
    Unit,
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),
    Char(char),
}

impl Literal {
    pub fn into_expr(self, ty: Ty, span: Span) -> Expr {
        Expr {
            kind: ExprKind::Literal(self),
            ty,
            span,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Builtin {
    SizeOf(Box<Expr>),
    AlignOf(Box<Expr>),
    Panic(Option<Box<Expr>>),
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
    pub block: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ForIter {
    Range(Box<Expr>, Box<Expr>),
    Value(Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Fn {
    pub sig: FnSig,
    pub body: Block,
    pub binding_info_id: BindingInfoId,
    pub is_entry_point: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnSig {
    pub name: Ustr,
    pub params: Vec<FnParam>,
    pub variadic: bool,
    pub ret: Option<Box<Expr>>,
    pub lib_name: Option<Ustr>,
    pub ty: Ty,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnParam {
    pub pattern: Pattern,
    pub ty_expr: Option<Box<Expr>>,
    pub ty: Ty,
}

impl ToString for FnParam {
    fn to_string(&self) -> String {
        self.pattern.to_string()
    }
}

#[derive(Hash, Debug, Eq, PartialEq, Clone)]
pub enum ForeignLibrary {
    System(String),
    Path { lib_path: String, lib_name: String },
}

impl ForeignLibrary {
    pub fn from_str(string: &str, module_path: Ustr, span: Span) -> DiagnosticResult<Self> {
        const SYSTEM_PREFIX: &str = "system:";

        if string.starts_with(SYSTEM_PREFIX) {
            let split: Vec<&str> = string.split(SYSTEM_PREFIX).collect();
            Ok(ForeignLibrary::System(split[1].to_string()))
        } else {
            let relative_to = Path::new(module_path.as_str())
                .parent()
                .unwrap()
                .to_str()
                .unwrap();

            let path = resolve_relative_path(string, relative_to, Some(span))?;
            let path = Path::new(&path);

            Ok(ForeignLibrary::Path {
                lib_path: path.parent().unwrap().to_str().unwrap().to_string(),
                lib_name: path.file_name().unwrap().to_str().unwrap().to_string(),
            })
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
}

impl Binding {
    pub fn new(
        visibility: Visibility,
        kind: BindingKind,
        pattern: Pattern,
        ty: Ty,
        ty_expr: Option<Expr>,
        value: Option<Expr>,
        lib_name: Option<Ustr>,
    ) -> Self {
        Self {
            module_id: Default::default(),
            visibility,
            kind,
            pattern,
            ty,
            ty_expr,
            expr: value,
            lib_name,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BindingKind {
    Value,
    Type,
    Import,
}

impl BindingKind {
    pub fn is_value(&self) -> bool {
        match self {
            BindingKind::Value => true,
            _ => false,
        }
    }

    pub fn is_type(&self) -> bool {
        match self {
            BindingKind::Type => true,
            _ => false,
        }
    }

    pub fn is_import(&self) -> bool {
        match self {
            BindingKind::Import => true,
            _ => false,
        }
    }
}

impl Display for BindingKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BindingKind::Value => "value",
                BindingKind::Type => "type",
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
        match self {
            Visibility::Private => true,
            _ => false,
        }
    }

    pub fn is_public(&self) -> bool {
        match self {
            Visibility::Public => true,
            _ => false,
        }
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
    NEq,
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
                NEq => "!=",
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
            BangEq => BinaryOp::NEq,
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
    BitwiseNot,
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
                UnaryOp::BitwiseNot => "~".to_string(),
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
            Tilde => UnaryOp::BitwiseNot,
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
    pub fn span(&self) -> Span {
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
        match self {
            ImportPathNode::Glob => true,
            _ => false,
        }
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

impl fmt::Display for FnSig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            format!(
                "fn: {} ({}{})",
                self.name,
                self.params
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                if self.variadic { ", .." } else { "" }
            )
        )
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Literal::Unit => "()".to_string(),
                Literal::Nil => "nil".to_string(),
                Literal::Bool(v) => v.to_string(),
                Literal::Int(v) => v.to_string(),
                Literal::Float(v) => v.to_string(),
                Literal::Str(v) => format!("\"{}\"", v),
                Literal::Char(v) => format!("'{}'", v),
            }
        )
    }
}
