use chilic_span::{MaybeSpanned, Span, Spanned};
use chilic_ty::{StructTyKind, Ty, UIntTy};
use ustr::Ustr;

use crate::{
    func::{Fn, Proto},
    op::{BinaryOp, UnaryOp},
    stmt::Stmt,
};

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Ty,
    pub span: Span,
}

impl Expr {
    pub fn typed(data: ExprKind, ty: Ty, span: Span) -> Self {
        Self {
            kind: data,
            ty,
            span,
        }
    }

    pub fn untyped(data: ExprKind, span: Span) -> Self {
        Self {
            kind: data,
            ty: Ty::Unknown,
            span,
        }
    }

    pub fn new(data: ExprKind, span: Span) -> Self {
        Expr::untyped(data, span)
    }

    pub fn is_fn(&self) -> bool {
        match &self.kind {
            ExprKind::Fn(..) => true,
            _ => false,
        }
    }

    pub fn into_fn(&self) -> &Fn {
        match &self.kind {
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
            ExprKind::FieldAccess { expr, .. } => expr.is_mutable(),

            ExprKind::Id { is_mutable, .. } => *is_mutable,

            ExprKind::MultiPointerType(_, _)
            | ExprKind::ArrayType(_, _)
            | ExprKind::SliceType(_, _)
            | ExprKind::StructType(_)
            | ExprKind::FnType(_) => false,

            _ => true,
        }
    }

    pub fn display_name_and_entity_span(&self) -> MaybeSpanned<String> {
        match &self.kind {
            ExprKind::Builtin(_) => {
                MaybeSpanned::not_spanned("@_(_)".to_string())
            }
            ExprKind::Fn(_) => MaybeSpanned::not_spanned("fn..".to_string()),
            ExprKind::For { .. } => {
                MaybeSpanned::not_spanned("for..".to_string())
            }
            ExprKind::Break { .. } => {
                MaybeSpanned::not_spanned("break".to_string())
            }
            ExprKind::Continue { .. } => {
                MaybeSpanned::not_spanned("continue".to_string())
            }
            ExprKind::Block { .. } => {
                MaybeSpanned::not_spanned("{..}".to_string())
            }
            ExprKind::If { .. } => {
                MaybeSpanned::not_spanned("if..".to_string())
            }
            ExprKind::Binary { op, lhs, rhs } => {
                MaybeSpanned::not_spanned(format!(
                    "{} {} {}",
                    lhs.display_name_and_entity_span().value,
                    op.to_string(),
                    rhs.display_name_and_entity_span().value
                ))
            }
            ExprKind::Unary { op, lhs } => {
                let lhs = lhs.display_name_and_entity_span();
                lhs.map(|v| format!("{}{}", op.to_string(), v))
            }
            ExprKind::Subscript { expr, .. } => {
                let expr = expr.display_name_and_entity_span();
                expr.map(|v| format!("{}[_]", v))
            }
            ExprKind::Slice { expr, .. } => {
                let expr = expr.display_name_and_entity_span();
                expr.map(|v| format!("{}[..]", v))
            }
            ExprKind::Call(call) => {
                let callee = call.callee.display_name_and_entity_span();
                callee.map(|v| format!("{}()", v))
            }
            ExprKind::FieldAccess { expr, field } => {
                let expr = expr.display_name_and_entity_span();
                expr.map(|v| format!("{}.{}", v, field))
            }
            ExprKind::Id {
                symbol,
                is_mutable: _,
                entity_span,
            } => MaybeSpanned::spanned(symbol.to_string(), entity_span.clone()),
            ExprKind::ArrayLiteral { .. } => {
                MaybeSpanned::not_spanned("[_]{..}".to_string())
            }
            ExprKind::TupleLiteral(_) => {
                MaybeSpanned::not_spanned("(..)".to_string())
            }
            ExprKind::StructLiteral { .. } => {
                MaybeSpanned::not_spanned("_{..}".to_string())
            }
            ExprKind::Literal(kind) => MaybeSpanned::not_spanned(match kind {
                LiteralKind::Unit => "()".to_string(),
                LiteralKind::Nil => "nil".to_string(),
                LiteralKind::Bool(v) => v.to_string(),
                LiteralKind::Int(v) => v.to_string(),
                LiteralKind::Float(v) => v.to_string(),
                LiteralKind::Str(v) => v.to_string(),
                LiteralKind::Char(v) => v.to_string(),
            }),
            _ => MaybeSpanned::not_spanned("_".to_string()),
        }
    }
}

#[derive(
    strum_macros::IntoStaticStr, strum_macros::Display, Debug, PartialEq, Clone,
)]
pub enum ExprKind {
    Assign {
        lvalue: Box<Expr>,
        rvalue: Box<Expr>,
    },
    Cast(TypeCastInfo),
    Builtin(Builtin),
    Fn(Fn),
    While {
        cond: Box<Expr>,
        expr: Box<Expr>,
    },
    For {
        iter_name: Ustr,
        iter_index_name: Ustr,
        iterator: ForIter,
        expr: Box<Expr>,
    },
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
    Block {
        stmts: Vec<Stmt>,
        deferred: Vec<Expr>,
    },
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
    FieldAccess {
        expr: Box<Expr>,
        field: Ustr,
    },
    Id {
        symbol: Ustr,
        is_mutable: bool,
        entity_span: Span,
    },
    ArrayLiteral(ArrayLiteralKind),
    TupleLiteral(Vec<Expr>),
    StructLiteral(Vec<StructLiteralField>),
    Literal(LiteralKind),
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
pub struct StructType {
    pub name: Ustr,
    pub qualified_name: Ustr,
    pub fields: Vec<StructTypeField>,
    pub kind: StructTyKind,
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
pub enum LiteralKind {
    Unit,
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),
    Char(char),
}

impl LiteralKind {
    pub fn ty(&self) -> Ty {
        match self {
            LiteralKind::Unit => Ty::Unit,
            LiteralKind::Bool(_) => Ty::Bool,
            LiteralKind::Str(_) => Ty::str(),
            LiteralKind::Char(_) => Ty::UInt(UIntTy::U8),
            LiteralKind::Nil | LiteralKind::Int(_) | LiteralKind::Float(_) => {
                Ty::Unknown
            }
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
pub struct TypeCastInfo {
    pub type_expr: Option<Box<Expr>>,
    pub expr: Box<Expr>,
    pub target_ty: Ty,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ForIter {
    Range(Box<Expr>, Box<Expr>),
    Value(Box<Expr>),
}
