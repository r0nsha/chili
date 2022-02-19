use chilic_ast::expr::Expr;

#[derive(Clone)]
pub(crate) struct DeferContext {
    stacks: Vec<DeferStack>,
}

impl DeferContext {
    pub fn new() -> Self {
        Self { stacks: vec![] }
    }

    pub fn stacks(&self) -> &Vec<DeferStack> {
        &self.stacks
    }

    pub fn push_stack(&mut self, kind: DeferStackKind) {
        self.stacks.push(DeferStack::new(kind));
    }

    pub fn pop_stack(&mut self) {
        self.stacks.pop();
    }

    pub fn current_stack(&self) -> &DeferStack {
        self.stacks.last().unwrap()
    }

    pub fn current_stack_mut(&mut self) -> &mut DeferStack {
        self.stacks.last_mut().unwrap()
    }

    pub fn current_deferred(&self) -> Vec<Expr> {
        self.current_stack()
            .deferred
            .iter()
            .rev()
            .map(|e| e.clone())
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum DeferStackKind {
    Fn,
    Loop,
    Block,
}
#[derive(Debug, Clone)]
pub(crate) struct DeferStack {
    pub(crate) deferred: Vec<Expr>,
    pub(crate) kind: DeferStackKind,
}

impl DeferStack {
    fn new(kind: DeferStackKind) -> Self {
        Self {
            deferred: vec![],
            kind,
        }
    }
}
