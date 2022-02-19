use chilic_span::Span;

use crate::{
    entity::Entity,
    expr::{Expr, ExprKind},
    r#use::Use,
};

#[derive(Debug, PartialEq, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

impl Stmt {
    pub fn new(kind: StmtKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(
    strum_macros::IntoStaticStr, strum_macros::Display, Debug, PartialEq, Clone,
)]
pub enum StmtKind {
    Use(Use),
    Entity(Entity),
    Defer(Expr),
    Expr { expr: Expr, terminated: bool },
}

impl StmtKind {
    pub fn is_terminator(&self) -> bool {
        match self {
            StmtKind::Expr { expr, terminated } => {
                if !terminated {
                    true
                } else {
                    match expr.kind {
                        ExprKind::Break { .. }
                        | ExprKind::Continue { .. }
                        | ExprKind::Return { .. } => true,
                        _ => false,
                    }
                }
            }
            _ => false,
        }
    }
}
