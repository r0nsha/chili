use chili_ast::ast;

#[derive(Debug, Clone)]
pub(crate) struct DeferStack {
    pub(crate) deferred: Vec<ast::Expr>,
}

impl DeferStack {
    pub(crate) fn new() -> Self {
        Self { deferred: vec![] }
    }
}
