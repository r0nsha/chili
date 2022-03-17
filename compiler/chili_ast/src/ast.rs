use crate::path::resolve_relative_path;
use crate::pattern::Pattern;
use crate::ty::*;
use crate::workspace::{BindingInfoId, ModuleId};
use chili_error::DiagnosticResult;
use chili_span::{MaybeSpanned, Span, Spanned};
use chili_token::TokenKind;
use codespan_reporting::files::SimpleFiles;
use common::compiler_info::STD;
use std::collections::HashSet;
use std::fmt::{self, Display};
use std::path::Path;
use ustr::Ustr;
use ustr::{ustr, UstrMap};

#[derive(Debug, Clone)]
pub struct Ast {
    pub module_id: ModuleId,
    pub module_info: ModuleInfo,
    pub imports: Vec<Import>,
    pub bindings: Vec<Binding>,
    pub foreign_libraries: HashSet<ForeignLibrary>,
}

impl Ast {
    pub fn new(module_info: ModuleInfo) -> Self {
        Self {
            module_id: Default::default(),
            module_info,
            imports: Default::default(),
            bindings: Default::default(),
            foreign_libraries: Default::default(),
        }
    }

    pub fn find_import(&self, id: BindingInfoId) -> Option<&Import> {
        for import in self.imports.iter() {
            if import.binding_info_id == id {
                return Some(import);
            }
        }
        None
    }

    pub fn find_binding(&self, id: BindingInfoId) -> Option<&Binding> {
        for binding in self.bindings.iter() {
            if binding.pattern.as_single_ref().binding_info_id == id {
                return Some(binding);
            }
        }
        None
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Ty,
    pub span: Span,
}

impl Expr {
    pub fn new(data: ExprKind, span: Span) -> Self {
        Self {
            kind: data,
            ty: Default::default(),
            span,
        }
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

    pub fn is_mutable(&self) -> bool {
        match &self.kind {
            ExprKind::MemberAccess { expr, .. } => expr.is_mutable(),

            ExprKind::Id { is_mutable, .. } => *is_mutable,

            ExprKind::MultiPointerType(_, _)
            | ExprKind::ArrayType(_, _)
            | ExprKind::SliceType(_, _)
            | ExprKind::StructType(_)
            | ExprKind::FnType(_) => false,

            _ => true,
        }
    }

    pub fn display_name_and_binding_span(&self) -> MaybeSpanned<String> {
        match &self.kind {
            ExprKind::Builtin(_) => MaybeSpanned::not_spanned("@_(_)".to_string()),
            ExprKind::Fn(_) => MaybeSpanned::not_spanned("fn..".to_string()),
            ExprKind::For { .. } => MaybeSpanned::not_spanned("for..".to_string()),
            ExprKind::Break { .. } => MaybeSpanned::not_spanned("break".to_string()),
            ExprKind::Continue { .. } => MaybeSpanned::not_spanned("continue".to_string()),
            ExprKind::Block { .. } => MaybeSpanned::not_spanned("{..}".to_string()),
            ExprKind::If { .. } => MaybeSpanned::not_spanned("if..".to_string()),
            ExprKind::Binary { op, lhs, rhs } => MaybeSpanned::not_spanned(format!(
                "{} {} {}",
                lhs.display_name_and_binding_span().value,
                op.to_string(),
                rhs.display_name_and_binding_span().value
            )),
            ExprKind::Unary { op, lhs } => {
                let lhs = lhs.display_name_and_binding_span();
                lhs.map(|v| format!("{}{}", op.to_string(), v))
            }
            ExprKind::Subscript { expr, .. } => {
                let expr = expr.display_name_and_binding_span();
                expr.map(|v| format!("{}[_]", v))
            }
            ExprKind::Slice { expr, .. } => {
                let expr = expr.display_name_and_binding_span();
                expr.map(|v| format!("{}[..]", v))
            }
            ExprKind::Call(call) => {
                let callee = call.callee.display_name_and_binding_span();
                callee.map(|v| format!("{}()", v))
            }
            ExprKind::MemberAccess { expr, member } => {
                let expr = expr.display_name_and_binding_span();
                expr.map(|v| format!("{}.{}", v, member))
            }
            ExprKind::Id {
                symbol,
                is_mutable: _,
                binding_span,
                binding_info_id: _,
            } => MaybeSpanned::spanned(symbol.to_string(), *binding_span),
            ExprKind::ArrayLiteral { .. } => MaybeSpanned::not_spanned("[_]{..}".to_string()),
            ExprKind::TupleLiteral(_) => MaybeSpanned::not_spanned("(..)".to_string()),
            ExprKind::StructLiteral { .. } => MaybeSpanned::not_spanned("_{..}".to_string()),
            ExprKind::Literal(kind) => MaybeSpanned::not_spanned(match kind {
                Literal::Unit => "()".to_string(),
                Literal::Nil => "nil".to_string(),
                Literal::Bool(v) => v.to_string(),
                Literal::Int(v) => v.to_string(),
                Literal::Float(v) => v.to_string(),
                Literal::Str(v) => v.to_string(),
                Literal::Char(v) => v.to_string(),
            }),
            _ => MaybeSpanned::not_spanned("_".to_string()),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprKind {
    Import(Vec<Import>),
    Foreign(Vec<Binding>),
    Binding(Box<Binding>),
    Defer(Box<Expr>),
    Assign {
        lvalue: Box<Expr>,
        rvalue: Box<Expr>,
    },
    Cast(Cast),
    Builtin(Builtin),
    Fn(Fn),
    While {
        cond: Box<Expr>,
        expr: Box<Expr>,
    },
    For(For),
    Break {
        deferred: Vec<Expr>,
    },
    Continue {
        deferred: Vec<Expr>,
    },
    Return {
        expr: Option<Box<Expr>>,
        deferred: Vec<Expr>,
    },
    If {
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Option<Box<Expr>>,
    },
    Block(Block),
    Binary {
        lhs: Box<Expr>,
        op: BinaryOp,
        rhs: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        lhs: Box<Expr>,
    },
    Subscript {
        expr: Box<Expr>,
        index: Box<Expr>,
    },
    Slice {
        expr: Box<Expr>,
        low: Option<Box<Expr>>,
        high: Option<Box<Expr>>,
    },
    Call(Call),
    MemberAccess {
        expr: Box<Expr>,
        member: Ustr,
    },
    Id {
        symbol: Ustr,
        is_mutable: bool,
        binding_span: Span,
        binding_info_id: BindingInfoId,
    },
    ArrayLiteral(ArrayLiteralKind),
    TupleLiteral(Vec<Expr>),
    StructLiteral {
        type_expr: Option<Box<Expr>>,
        fields: Vec<StructLiteralField>,
    },
    Literal(Literal),
    PointerType(Box<Expr>, bool),
    MultiPointerType(Box<Expr>, bool),
    ArrayType(Box<Expr>, Box<Expr>),
    SliceType(Box<Expr>, bool),
    StructType(StructType),
    FnType(Proto),
    SelfType,
    NeverType,
    UnitType,
    PlaceholderType,
    Noop,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub exprs: Vec<Expr>,
    pub deferred: Vec<Expr>,
    pub yields: bool,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructType {
    pub name: Ustr,
    pub fields: Vec<StructTypeField>,
    pub kind: StructTyKind,
    pub binding_info_id: Option<BindingInfoId>,
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
pub struct Call {
    pub callee: Box<Expr>,
    pub args: Vec<CallArg>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallArg {
    pub symbol: Option<Spanned<Ustr>>,
    pub value: Expr,
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

#[derive(Debug, PartialEq, Clone)]
pub enum Builtin {
    SizeOf(Box<Expr>),
    AlignOf(Box<Expr>),
    Panic(Option<Box<Expr>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Cast {
    pub ty_expr: Option<Box<Expr>>,
    pub expr: Box<Expr>,
    pub target_ty: Ty,
}

#[derive(Debug, PartialEq, Clone)]
pub struct For {
    pub iter_name: Ustr,
    pub iter_id: BindingInfoId,
    pub iter_index_name: Ustr,
    pub iter_index_id: BindingInfoId,
    pub iterator: ForIter,
    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ForIter {
    Range(Box<Expr>, Box<Expr>),
    Value(Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Fn {
    pub proto: Proto,
    pub body: Block,
    pub is_entry_point: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Proto {
    pub name: Ustr,
    pub params: Vec<FnParam>,
    pub variadic: bool,
    pub ret: Option<Box<Expr>>,
    pub lib_name: Option<Ustr>,
    pub binding_info_id: Option<BindingInfoId>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnParam {
    pub pattern: Pattern,
    pub ty: Option<Box<Expr>>,
}

impl ToString for FnParam {
    fn to_string(&self) -> String {
        self.pattern.to_string()
    }
}

#[derive(Debug, Clone)]
pub struct Ir {
    pub modules: UstrMap<Module>,
    pub startup_fn: Option<Fn>,
    pub foreign_libraries: HashSet<ForeignLibrary>,
    pub files: SimpleFiles<String, String>,
}

impl Ir {
    pub fn new(files: SimpleFiles<String, String>) -> Self {
        Self {
            modules: UstrMap::default(),
            startup_fn: None,
            foreign_libraries: HashSet::new(),
            files,
        }
    }

    #[inline]
    pub fn std_module(&self, m: &str) -> &Module {
        self.module(ustr(&format!("{}.{}", STD, m)))
    }

    #[inline]
    pub fn root_module(&self) -> &Module {
        self.module(common::builtin::root_module())
    }

    #[inline]
    pub fn module(&self, symbol: impl Into<Ustr>) -> &Module {
        let symbol = symbol.into();
        self.modules
            .get(&symbol)
            .expect(&format!("couldn't find `{}`", symbol))
    }

    #[inline]
    pub fn module_info(&self, symbol: impl Into<Ustr>) -> ModuleInfo {
        let symbol = symbol.into();
        self.modules
            .get(&symbol)
            .expect(&format!("couldn't find `{}`", symbol))
            .info
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Module {
    pub info: ModuleInfo,
    pub imports: Vec<Import>,
    pub bindings: Vec<Binding>,
}

impl Module {
    pub fn new(info: ModuleInfo) -> Self {
        Self {
            info,
            imports: vec![],
            bindings: vec![],
        }
    }

    pub fn find_binding(&self, symbol: impl Into<Ustr>) -> Option<&Binding> {
        let symbol = symbol.into();
        self.bindings
            .iter()
            .find(|binding| binding.pattern.into_single().symbol == symbol)
    }

    pub fn find_import(&self, symbol: impl Into<Ustr>) -> Option<&Import> {
        let symbol = symbol.into();
        self.imports.iter().find(|import| import.alias == symbol)
    }
}

#[derive(Debug, Default, PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Hash)]
pub struct ModuleInfo {
    pub name: Ustr,
    pub file_path: Ustr,
}

impl ModuleInfo {
    pub fn new(name: Ustr, file_path: Ustr) -> Self {
        Self { name, file_path }
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
    pub visibility: Visibility,
    pub kind: BindingKind,
    pub pattern: Pattern,
    pub ty_expr: Option<Expr>,
    pub expr: Option<Expr>,
    pub lib_name: Option<Ustr>,
}

impl Binding {
    pub fn new(
        visibility: Visibility,
        kind: BindingKind,
        pattern: Pattern,
        ty_expr: Option<Expr>,
        value: Option<Expr>,
        lib_name: Option<Ustr>,
    ) -> Self {
        Self {
            visibility,
            kind,
            pattern,
            ty_expr,
            expr: value,
            lib_name,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BindingKind {
    Let,
    Type,
    Import,
}

impl Display for BindingKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BindingKind::Let => "value",
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
    pub binding_info_id: BindingInfoId,
    pub module_id: ModuleId,
    pub module_info: ModuleInfo,
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
            self.module_info.name.to_string()
        } else {
            format!(
                "{}.{}",
                self.module_info.name,
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

impl fmt::Display for Proto {
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
