use chili_ast::ast::{
    ArrayLiteralKind, Binding, Block, Builtin, Cast, Expr, ExprKind, Fn,
    ForIter, Proto,
};

#[derive(Clone)]
pub(crate) struct DeferSess {
    stacks: Vec<DeferStack>,
}

impl DeferSess {
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

    pub fn collect_deferred(&self) -> Vec<Expr> {
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

pub(crate) trait SolveDefer {
    fn solve_defer(&mut self, sess: &mut DeferSess);
}

impl<T: SolveDefer> SolveDefer for Vec<T> {
    fn solve_defer(&mut self, sess: &mut DeferSess) {
        self.iter_mut().for_each(|a| a.solve_defer(sess));
    }
}

impl<T: SolveDefer> SolveDefer for Option<T> {
    fn solve_defer(&mut self, sess: &mut DeferSess) {
        if let Some(s) = self {
            s.solve_defer(sess);
        }
    }
}

impl<T: SolveDefer> SolveDefer for Box<T> {
    fn solve_defer(&mut self, sess: &mut DeferSess) {
        self.as_mut().solve_defer(sess);
    }
}

impl SolveDefer for Fn {
    fn solve_defer(&mut self, sess: &mut DeferSess) {
        self.proto.solve_defer(sess);
        self.body.solve_defer(sess);
    }
}

impl SolveDefer for Block {
    fn solve_defer(&mut self, sess: &mut DeferSess) {
        sess.push_stack(DeferStackKind::Block);

        self.exprs.solve_defer(sess);
        self.deferred = sess.collect_deferred();

        sess.pop_stack();
    }
}

impl SolveDefer for Binding {
    fn solve_defer(&mut self, sess: &mut DeferSess) {
        self.ty_expr.solve_defer(sess);
        self.value.solve_defer(sess);
    }
}

impl SolveDefer for Expr {
    fn solve_defer(&mut self, sess: &mut DeferSess) {
        match &mut self.kind {
            ExprKind::Import(..) => (),
            ExprKind::Foreign(bindings) => {
                bindings.solve_defer(sess);
            }
            ExprKind::Binding(binding) => {
                binding.solve_defer(sess);
            }
            ExprKind::Defer(expr) => {
                sess.current_stack_mut().deferred.push(*expr.clone());
            }
            ExprKind::Assign { lvalue, rvalue } => {
                lvalue.solve_defer(sess);
                rvalue.solve_defer(sess);
            }
            ExprKind::Cast(cast) => cast.solve_defer(sess),
            ExprKind::Builtin(builtin) => match builtin {
                Builtin::SizeOf(expr) | Builtin::AlignOf(expr) => {
                    expr.solve_defer(sess)
                }
                Builtin::Panic(expr) => expr.solve_defer(sess),
            },
            ExprKind::Fn(func) => func.solve_defer(sess),
            ExprKind::While { cond, expr } => {
                cond.solve_defer(sess);
                expr.solve_defer(sess);
            }
            ExprKind::For { iterator, expr, .. } => {
                sess.push_stack(DeferStackKind::Loop);

                match iterator {
                    ForIter::Range(start, end) => {
                        start.solve_defer(sess);
                        end.solve_defer(sess);
                    }
                    ForIter::Value(value) => value.solve_defer(sess),
                }

                expr.solve_defer(sess);

                sess.pop_stack();
            }
            ExprKind::Break { deferred } | ExprKind::Continue { deferred } => {
                for stack in sess.stacks().iter().rev() {
                    if let DeferStackKind::Loop = stack.kind {
                        break;
                    }

                    for expr in stack.deferred.iter().rev() {
                        deferred.push(expr.clone())
                    }
                }
            }
            ExprKind::Return { expr, deferred } => {
                for stack in sess.stacks().iter().rev() {
                    for expr in stack.deferred.iter().rev() {
                        deferred.push(expr.clone())
                    }
                }

                expr.solve_defer(sess);
            }
            ExprKind::If {
                cond,
                then_expr,
                else_expr,
            } => {
                cond.solve_defer(sess);
                then_expr.solve_defer(sess);
                else_expr.solve_defer(sess);
            }
            ExprKind::Block(block) => block.solve_defer(sess),
            ExprKind::Binary { lhs, op: _, rhs } => {
                lhs.solve_defer(sess);
                rhs.solve_defer(sess);
            }
            ExprKind::Unary { op: _, lhs } => lhs.solve_defer(sess),
            ExprKind::Subscript { expr, index } => {
                expr.solve_defer(sess);
                index.solve_defer(sess);
            }
            ExprKind::Slice { expr, low, high } => {
                expr.solve_defer(sess);
                low.solve_defer(sess);
                high.solve_defer(sess);
            }
            ExprKind::Call(call) => {
                call.callee.solve_defer(sess);
                for arg in call.args.iter_mut() {
                    arg.value.solve_defer(sess);
                }
            }
            ExprKind::MemberAccess { expr, .. } => {
                expr.solve_defer(sess);
            }
            ExprKind::Id { .. } => (),
            ExprKind::ArrayLiteral(kind) => match kind {
                ArrayLiteralKind::List(elements) => {
                    elements.solve_defer(sess);
                }
                ArrayLiteralKind::Fill { expr, len } => {
                    len.solve_defer(sess);
                    expr.solve_defer(sess);
                }
            },
            ExprKind::TupleLiteral(elements) => elements.solve_defer(sess),
            ExprKind::StructLiteral { type_expr, fields } => {
                type_expr.solve_defer(sess);
                for field in fields.iter_mut() {
                    field.value.solve_defer(sess);
                }
            }
            ExprKind::Literal(..) => (),
            ExprKind::PointerType(expr, ..) => expr.solve_defer(sess),
            ExprKind::MultiPointerType(expr, ..) => {
                expr.solve_defer(sess);
            }
            ExprKind::ArrayType(expr, size) => {
                expr.solve_defer(sess);
                size.solve_defer(sess);
            }
            ExprKind::SliceType(expr, ..) => {
                expr.solve_defer(sess);
            }
            ExprKind::StructType(t) => {
                for field in t.fields.iter_mut() {
                    field.ty.solve_defer(sess);
                }
            }
            ExprKind::FnType(proto) => proto.solve_defer(sess),

            ExprKind::SelfType
            | ExprKind::NeverType
            | ExprKind::UnitType
            | ExprKind::PlaceholderType
            | ExprKind::Noop => (),
        };
    }
}

impl SolveDefer for Proto {
    fn solve_defer(&mut self, sess: &mut DeferSess) {
        for p in self.params.iter_mut() {
            p.ty.solve_defer(sess);
        }
        self.ret.solve_defer(sess);
    }
}

impl SolveDefer for Cast {
    fn solve_defer(&mut self, sess: &mut DeferSess) {
        self.expr.solve_defer(sess);
        self.type_expr.solve_defer(sess);
    }
}
