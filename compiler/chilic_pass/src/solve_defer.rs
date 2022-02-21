use chilic_ast::ast::{
    ArrayLiteralKind, Block, Builtin, Cast, Entity, Expr, ExprKind, Fn,
    ForIter, Proto,
};

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
    fn solve_defer(&mut self, ctx: &mut DeferContext);
}

impl<T: SolveDefer> SolveDefer for Vec<T> {
    fn solve_defer(&mut self, ctx: &mut DeferContext) {
        self.iter_mut().for_each(|a| a.solve_defer(ctx));
    }
}

impl<T: SolveDefer> SolveDefer for Option<T> {
    fn solve_defer(&mut self, ctx: &mut DeferContext) {
        if let Some(s) = self {
            s.solve_defer(ctx);
        }
    }
}

impl<T: SolveDefer> SolveDefer for Box<T> {
    fn solve_defer(&mut self, ctx: &mut DeferContext) {
        self.as_mut().solve_defer(ctx);
    }
}

impl SolveDefer for Fn {
    fn solve_defer(&mut self, ctx: &mut DeferContext) {
        self.proto.solve_defer(ctx);
        self.body.solve_defer(ctx);
    }
}

impl SolveDefer for Block {
    fn solve_defer(&mut self, ctx: &mut DeferContext) {
        ctx.push_stack(DeferStackKind::Block);

        self.exprs.solve_defer(ctx);
        self.deferred = ctx.collect_deferred();

        ctx.pop_stack();
    }
}

impl SolveDefer for Entity {
    fn solve_defer(&mut self, ctx: &mut DeferContext) {
        self.ty_expr.solve_defer(ctx);
        self.value.solve_defer(ctx);
    }
}

impl SolveDefer for Expr {
    fn solve_defer(&mut self, ctx: &mut DeferContext) {
        match &mut self.kind {
            ExprKind::Use(..) => (),
            ExprKind::Foreign(entities) => {
                entities.solve_defer(ctx);
            }
            ExprKind::Entity(entity) => {
                entity.solve_defer(ctx);
            }
            ExprKind::Defer(expr) => {
                ctx.current_stack_mut().deferred.push(*expr.clone());
            }
            ExprKind::Assign { lvalue, rvalue } => {
                lvalue.solve_defer(ctx);
                rvalue.solve_defer(ctx);
            }
            ExprKind::Cast(cast) => cast.solve_defer(ctx),
            ExprKind::Builtin(builtin) => match builtin {
                Builtin::SizeOf(expr) | Builtin::AlignOf(expr) => {
                    expr.solve_defer(ctx)
                }
                Builtin::Panic(expr) => expr.solve_defer(ctx),
            },
            ExprKind::Fn(func) => func.solve_defer(ctx),
            ExprKind::While { cond, expr } => {
                cond.solve_defer(ctx);
                expr.solve_defer(ctx);
            }
            ExprKind::For { iterator, expr, .. } => {
                ctx.push_stack(DeferStackKind::Loop);

                match iterator {
                    ForIter::Range(start, end) => {
                        start.solve_defer(ctx);
                        end.solve_defer(ctx);
                    }
                    ForIter::Value(value) => value.solve_defer(ctx),
                }

                expr.solve_defer(ctx);

                ctx.pop_stack();
            }
            ExprKind::Break { deferred } | ExprKind::Continue { deferred } => {
                for stack in ctx.stacks().iter().rev() {
                    if let DeferStackKind::Loop = stack.kind {
                        break;
                    }

                    for expr in stack.deferred.iter().rev() {
                        deferred.push(expr.clone())
                    }
                }
            }
            ExprKind::Return { expr, deferred } => {
                for stack in ctx.stacks().iter().rev() {
                    for expr in stack.deferred.iter().rev() {
                        deferred.push(expr.clone())
                    }
                }

                expr.solve_defer(ctx);
            }
            ExprKind::If {
                cond,
                then_expr,
                else_expr,
            } => {
                cond.solve_defer(ctx);
                then_expr.solve_defer(ctx);
                else_expr.solve_defer(ctx);
            }
            ExprKind::Block(block) => block.solve_defer(ctx),
            ExprKind::Binary { lhs, op: _, rhs } => {
                lhs.solve_defer(ctx);
                rhs.solve_defer(ctx);
            }
            ExprKind::Unary { op: _, lhs } => lhs.solve_defer(ctx),
            ExprKind::Subscript { expr, index } => {
                expr.solve_defer(ctx);
                index.solve_defer(ctx);
            }
            ExprKind::Slice { expr, low, high } => {
                expr.solve_defer(ctx);
                low.solve_defer(ctx);
                high.solve_defer(ctx);
            }
            ExprKind::Call(call) => {
                call.callee.solve_defer(ctx);
                for arg in call.args.iter_mut() {
                    arg.value.solve_defer(ctx);
                }
            }
            ExprKind::MemberAccess { expr, .. } => {
                expr.solve_defer(ctx);
            }
            ExprKind::Id { .. } => (),
            ExprKind::ArrayLiteral(kind) => match kind {
                ArrayLiteralKind::List(elements) => {
                    elements.solve_defer(ctx);
                }
                ArrayLiteralKind::Fill { expr, len } => {
                    len.solve_defer(ctx);
                    expr.solve_defer(ctx);
                }
            },
            ExprKind::TupleLiteral(elements) => elements.solve_defer(ctx),
            ExprKind::StructLiteral { type_expr, fields } => {
                type_expr.solve_defer(ctx);
                for field in fields.iter_mut() {
                    field.value.solve_defer(ctx);
                }
            }
            ExprKind::Literal(..) => (),
            ExprKind::PointerType(expr, ..) => expr.solve_defer(ctx),
            ExprKind::MultiPointerType(expr, ..) => {
                expr.solve_defer(ctx);
            }
            ExprKind::ArrayType(expr, size) => {
                expr.solve_defer(ctx);
                size.solve_defer(ctx);
            }
            ExprKind::SliceType(expr, ..) => {
                expr.solve_defer(ctx);
            }
            ExprKind::StructType(t) => {
                for field in t.fields.iter_mut() {
                    field.ty.solve_defer(ctx);
                }
            }
            ExprKind::FnType(proto) => proto.solve_defer(ctx),

            ExprKind::SelfType
            | ExprKind::NeverType
            | ExprKind::UnitType
            | ExprKind::PlaceholderType
            | ExprKind::Noop => (),
        };
    }
}

impl SolveDefer for Proto {
    fn solve_defer(&mut self, ctx: &mut DeferContext) {
        for p in self.params.iter_mut() {
            p.ty.solve_defer(ctx);
        }
        self.ret.solve_defer(ctx);
    }
}

impl SolveDefer for Cast {
    fn solve_defer(&mut self, ctx: &mut DeferContext) {
        self.expr.solve_defer(ctx);
        self.type_expr.solve_defer(ctx);
    }
}
