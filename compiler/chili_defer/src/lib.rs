use chili_ast::ast;

pub fn solve_defers(ast: &mut ast::TypedAst) {
    let mut sess = DeferSess { stacks: vec![] };
    for binding in ast.bindings.values_mut() {
        binding.solve_defer(&mut sess);
    }
}

#[derive(Clone)]
struct DeferSess {
    stacks: Vec<DeferStack>,
}

impl DeferSess {
    fn stacks(&self) -> &Vec<DeferStack> {
        &self.stacks
    }

    fn push_stack(&mut self, kind: DeferStackKind) {
        self.stacks.push(DeferStack::new(kind));
    }

    fn pop_stack(&mut self) {
        self.stacks.pop();
    }

    fn current_stack(&self) -> &DeferStack {
        self.stacks.last().unwrap()
    }

    fn current_stack_mut(&mut self) -> &mut DeferStack {
        self.stacks.last_mut().unwrap()
    }

    fn collect_deferred(&self) -> Vec<ast::Expr> {
        self.current_stack()
            .deferred
            .iter()
            .rev()
            .cloned()
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum DeferStackKind {
    Loop,
    Block,
}
#[derive(Debug, Clone)]
struct DeferStack {
    deferred: Vec<ast::Expr>,
    kind: DeferStackKind,
}

impl DeferStack {
    fn new(kind: DeferStackKind) -> Self {
        Self {
            deferred: vec![],
            kind,
        }
    }
}

trait SolveDefer {
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

impl SolveDefer for ast::Fn {
    fn solve_defer(&mut self, sess: &mut DeferSess) {
        self.sig.solve_defer(sess);
        self.body.solve_defer(sess);
    }
}

impl SolveDefer for ast::Block {
    fn solve_defer(&mut self, sess: &mut DeferSess) {
        sess.push_stack(DeferStackKind::Block);

        self.exprs.solve_defer(sess);
        self.deferred = sess.collect_deferred();

        sess.pop_stack();
    }
}

impl SolveDefer for ast::Binding {
    fn solve_defer(&mut self, sess: &mut DeferSess) {
        self.ty_expr.solve_defer(sess);
        self.expr.solve_defer(sess);
    }
}

impl SolveDefer for ast::Expr {
    fn solve_defer(&mut self, sess: &mut DeferSess) {
        match &mut self.kind {
            ast::ExprKind::Import(..) => (),
            ast::ExprKind::Foreign(bindings) => bindings.solve_defer(sess),
            ast::ExprKind::Binding(binding) => binding.solve_defer(sess),
            ast::ExprKind::Defer(defer) => sess
                .current_stack_mut()
                .deferred
                .push(defer.expr.as_ref().clone()),
            ast::ExprKind::Assign(assign) => {
                assign.lvalue.solve_defer(sess);
                assign.rvalue.solve_defer(sess);
            }
            ast::ExprKind::Cast(cast) => cast.solve_defer(sess),
            ast::ExprKind::Builtin(builtin) => match builtin {
                ast::Builtin::SizeOf(expr) | ast::Builtin::AlignOf(expr) => expr.solve_defer(sess),
                ast::Builtin::Panic(expr) => expr.solve_defer(sess),
            },
            ast::ExprKind::Fn(func) => func.solve_defer(sess),
            ast::ExprKind::While(while_) => {
                while_.cond.solve_defer(sess);
                while_.block.solve_defer(sess);
            }
            ast::ExprKind::For(for_) => {
                sess.push_stack(DeferStackKind::Loop);

                match &mut for_.iterator {
                    ast::ForIter::Range(start, end) => {
                        start.solve_defer(sess);
                        end.solve_defer(sess);
                    }
                    ast::ForIter::Value(value) => value.solve_defer(sess),
                }

                for_.block.solve_defer(sess);

                sess.pop_stack();
            }
            ast::ExprKind::Break(term) | ast::ExprKind::Continue(term) => {
                for stack in sess.stacks().iter().rev() {
                    if let DeferStackKind::Loop = stack.kind {
                        break;
                    }

                    for expr in stack.deferred.iter().rev() {
                        term.deferred.push(expr.clone())
                    }
                }
            }
            ast::ExprKind::Return(ret) => {
                for stack in sess.stacks().iter().rev() {
                    for expr in stack.deferred.iter().rev() {
                        ret.deferred.push(expr.clone())
                    }
                }

                ret.expr.solve_defer(sess);
            }
            ast::ExprKind::If(if_) => {
                if_.cond.solve_defer(sess);
                if_.then.solve_defer(sess);
                if_.otherwise.solve_defer(sess);
            }
            ast::ExprKind::Block(block) => block.solve_defer(sess),
            ast::ExprKind::Unary(unary) => unary.lhs.solve_defer(sess),
            ast::ExprKind::Binary(binary) => {
                binary.lhs.solve_defer(sess);
                binary.rhs.solve_defer(sess);
            }
            ast::ExprKind::Subscript(sub) => {
                sub.expr.solve_defer(sess);
                sub.index.solve_defer(sess);
            }
            ast::ExprKind::Slice(slice) => {
                slice.expr.solve_defer(sess);
                slice.low.solve_defer(sess);
                slice.high.solve_defer(sess);
            }
            ast::ExprKind::FnCall(call) => {
                call.callee.solve_defer(sess);
                for arg in call.args.iter_mut() {
                    arg.solve_defer(sess);
                }
            }
            ast::ExprKind::MemberAccess(access) => access.expr.solve_defer(sess),
            ast::ExprKind::ArrayLiteral(lit) => match &mut lit.kind {
                ast::ArrayLiteralKind::List(elements) => elements.solve_defer(sess),
                ast::ArrayLiteralKind::Fill { expr, len } => {
                    len.solve_defer(sess);
                    expr.solve_defer(sess);
                }
            },
            ast::ExprKind::TupleLiteral(lit) => lit.elements.solve_defer(sess),
            ast::ExprKind::StructLiteral(lit) => {
                lit.type_expr.solve_defer(sess);
                for field in lit.fields.iter_mut() {
                    field.value.solve_defer(sess);
                }
            }
            ast::ExprKind::PointerType(e)
            | ast::ExprKind::MultiPointerType(e)
            | ast::ExprKind::SliceType(e) => e.inner.solve_defer(sess),
            ast::ExprKind::ArrayType(at) => {
                at.inner.solve_defer(sess);
                at.size.solve_defer(sess);
            }
            ast::ExprKind::StructType(t) => {
                for field in t.fields.iter_mut() {
                    field.ty.solve_defer(sess);
                }
            }
            ast::ExprKind::FnType(sig) => sig.solve_defer(sess),
            ast::ExprKind::Ident(_)
            | ast::ExprKind::Literal(_)
            | ast::ExprKind::SelfType
            | ast::ExprKind::NeverType
            | ast::ExprKind::UnitType
            | ast::ExprKind::PlaceholderType => (),
            ast::ExprKind::Error => panic!("unexpected error node"),
        };
    }
}

impl SolveDefer for ast::FnSig {
    fn solve_defer(&mut self, sess: &mut DeferSess) {
        for p in self.params.iter_mut() {
            p.ty_expr.solve_defer(sess);
        }
        self.ret.solve_defer(sess);
    }
}

impl SolveDefer for ast::Cast {
    fn solve_defer(&mut self, sess: &mut DeferSess) {
        self.expr.solve_defer(sess);
        self.ty_expr.solve_defer(sess);
    }
}
