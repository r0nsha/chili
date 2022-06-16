use crate::ast::ast;

#[derive(Debug, Clone)]
pub struct DeferStack {
    pub deferred: Vec<ast::Expr>,
}

impl DeferStack {
    pub fn new() -> Self {
        Self { deferred: vec![] }
    }
}
