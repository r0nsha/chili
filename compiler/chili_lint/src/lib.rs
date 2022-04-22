mod access;
mod lvalue_access;
mod ref_access;
mod sess;
mod type_limits;

use chili_ast::{
    ast::{self, TypedAst},
    pattern::Pattern,
    workspace::Workspace,
};
use chili_error::{
    diagnostic::{Diagnostic, Label},
    TypeError,
};
use chili_infer::{normalize::NormalizeTy, ty_ctx::TyCtx};
use common::scopes::Scopes;
use sess::{InitState, LintSess};

pub fn lint(workspace: &mut Workspace, tycx: &TyCtx, ast: &TypedAst) {
    let mut sess = LintSess {
        workspace,
        tycx,
        init_scopes: Scopes::default(),
    };

    sess.init_scopes.push_scope();

    for binding in ast.bindings.values() {
        binding.lint(&mut sess);
    }

    sess.init_scopes.pop_scope();
}

trait Lint {
    fn lint(&self, sess: &mut LintSess);
}

impl<T: Lint> Lint for Vec<T> {
    fn lint(&self, sess: &mut LintSess) {
        for element in self {
            element.lint(sess);
        }
    }
}

impl<T: Lint> Lint for Option<T> {
    fn lint(&self, sess: &mut LintSess) {
        if let Some(e) = self {
            e.lint(sess);
        }
    }
}

impl<T: Lint> Lint for Box<T> {
    fn lint(&self, sess: &mut LintSess) {
        self.as_ref().lint(sess)
    }
}

impl Lint for ast::Ast {
    fn lint(&self, sess: &mut LintSess) {
        for binding in self.bindings.iter() {
            binding.lint(sess);
        }
    }
}

impl Lint for ast::Binding {
    fn lint(&self, sess: &mut LintSess) {
        let init_state = if self.expr.is_some() {
            InitState::Init
        } else {
            InitState::NotInit
        };

        for symbol in self.pattern.symbols() {
            sess.init_scopes.insert(symbol.binding_info_id, init_state);
        }

        self.expr.lint(sess);

        if let Some(expr) = &self.expr {
            let is_a_type = expr.ty.normalize(&sess.tycx).is_type();

            match &self.kind {
                ast::BindingKind::Value => {
                    if is_a_type {
                        sess.workspace.diagnostics.push(TypeError::expected(
                            expr.span,
                            expr.ty.to_string(),
                            "a value",
                        ));
                    }
                }
                ast::BindingKind::Type => {
                    if !is_a_type {
                        sess.workspace.diagnostics.push(TypeError::expected(
                            expr.span,
                            expr.ty.to_string(),
                            "a type",
                        ));
                    }
                }
                ast::BindingKind::Import => (),
            }

            // * don't allow types to be bounded to mutable bindings
            if is_a_type {
                match &self.pattern {
                    Pattern::Single(pat) => {
                        if pat.is_mutable {
                            sess.workspace.diagnostics.push(
                                Diagnostic::error()
                                    .with_message("variable of type `type` must be immutable")
                                    .with_label(Label::primary(pat.span, "variable is mutable"))
                                    .with_note("try removing the `mut` from the declaration"),
                            );
                        }
                    }
                    Pattern::StructUnpack(_) | Pattern::TupleUnpack(_) => (),
                }
            }
        }
    }
}

impl Lint for ast::Block {
    fn lint(&self, sess: &mut LintSess) {
        sess.init_scopes.push_scope();
        self.exprs.lint(sess);
        self.deferred.lint(sess);
        sess.init_scopes.pop_scope();
    }
}

impl Lint for ast::Expr {
    fn lint(&self, sess: &mut LintSess) {
        match &self.kind {
            ast::ExprKind::Import(_) => (),
            ast::ExprKind::Foreign(e) => e.lint(sess),
            ast::ExprKind::Binding(e) => e.lint(sess),
            ast::ExprKind::Defer(defer) => defer.expr.lint(sess),
            ast::ExprKind::Assign(assign) => {
                assign.rvalue.lint(sess);

                match &assign.lvalue.kind {
                    ast::ExprKind::Ident(ident) => {
                        sess.check_assign_lvalue_id_access(&assign.lvalue, ident.binding_info_id);
                    }
                    _ => {
                        sess.check_lvalue_access(&assign.lvalue, assign.lvalue.span);
                        assign.lvalue.lint(sess);
                    }
                };
            }
            ast::ExprKind::Cast(t) => t.expr.lint(sess),
            ast::ExprKind::Builtin(b) => match b {
                ast::Builtin::SizeOf(e) | ast::Builtin::AlignOf(e) | ast::Builtin::Run(e) => {
                    e.lint(sess)
                }
                ast::Builtin::Panic(e) => e.lint(sess),
            },
            ast::ExprKind::Fn(f) => {
                // let ty = f.sig.ty.normalize(&sess.tycx).into_fn();

                // if this is the main function, check its type matches a fn() -> [() | !]
                // if f.is_entry_point
                //     && (!(ty.ret.is_unit() || ty.ret.is_never())
                //         || !ty.params.is_empty()
                //         || ty.variadic)
                // {
                //     sess.workspace.diagnostics.add(
                //         Diagnostic::error()
                //             .with_message(format!(
                //                 "entry point function `main` has type `{}`, expected `fn() -> ()`",
                //                 ty
                //             ))
                //             .with_label(Label::primary(
                //                 self.span,
                //                 "invalid type of entry point function",
                //             )),
                //     );
                // }

                f.body.lint(sess);
            }
            ast::ExprKind::While(while_) => {
                while_.cond.lint(sess);
                while_.block.lint(sess);
            }
            ast::ExprKind::For(for_) => {
                match &for_.iterator {
                    ast::ForIter::Range(s, e) => {
                        s.lint(sess);
                        e.lint(sess);
                    }
                    ast::ForIter::Value(v) => {
                        v.lint(sess);
                    }
                }
                for_.block.lint(sess);
            }
            ast::ExprKind::Break(term) | ast::ExprKind::Continue(term) => {
                term.deferred.lint(sess);
            }
            ast::ExprKind::Return(ret) => {
                ret.expr.lint(sess);
                ret.deferred.lint(sess);
            }
            ast::ExprKind::If(if_) => {
                if_.cond.lint(sess);
                if_.then.lint(sess);
                if_.otherwise.lint(sess);
            }
            ast::ExprKind::Block(block) => block.lint(sess),
            ast::ExprKind::Binary(binary) => {
                binary.lhs.lint(sess);
                binary.rhs.lint(sess);
            }
            ast::ExprKind::Unary(unary) => {
                unary.lhs.lint(sess);

                match &unary.op {
                    ast::UnaryOp::Ref(is_mutable_ref) => {
                        if *is_mutable_ref {
                            sess.check_expr_can_be_mutably_referenced(&unary.lhs);
                        }
                    }
                    _ => (),
                }
            }
            ast::ExprKind::Subscript(sub) => {
                sub.expr.lint(sess);
                sub.index.lint(sess);
            }
            ast::ExprKind::Slice(slice) => {
                slice.expr.lint(sess);
                slice.low.lint(sess);
                slice.high.lint(sess);

                if let None = &slice.high {
                    if slice.expr.ty.normalize(&sess.tycx).is_multi_pointer() {
                        sess.workspace.diagnostics.push(Diagnostic::error()
                        .with_message(
                            "multi pointer has unknown length, so you must specify the ending index",
                        )
                        .with_label(Label::primary(
                            slice.expr.span,"multi pointer has unknown length"
                        )));
                    }
                }
            }
            ast::ExprKind::Call(call) => {
                call.callee.lint(sess);
                call.args.lint(sess);
            }
            ast::ExprKind::MemberAccess(access) => access.expr.lint(sess),
            ast::ExprKind::ArrayLiteral(lit) => match &lit.kind {
                ast::ArrayLiteralKind::List(l) => l.lint(sess),
                ast::ArrayLiteralKind::Fill { len, expr } => {
                    len.lint(sess);
                    expr.lint(sess);
                }
            },
            ast::ExprKind::TupleLiteral(lit) => {
                lit.elements.lint(sess);
            }
            ast::ExprKind::StructLiteral(lit) => {
                lit.type_expr.lint(sess);
                for field in &lit.fields {
                    field.value.lint(sess);
                }
            }
            ast::ExprKind::PointerType(e)
            | ast::ExprKind::MultiPointerType(e)
            | ast::ExprKind::SliceType(e) => e.inner.lint(sess),
            ast::ExprKind::ArrayType(at) => at.inner.lint(sess),
            ast::ExprKind::StructType(s) => {
                for f in &s.fields {
                    f.ty.lint(sess);
                }
            }
            ast::ExprKind::FnType(sig) => {
                for p in &sig.params {
                    p.ty_expr.lint(sess);
                }
                sig.ret.lint(sess);
            }
            ast::ExprKind::Ident(ident) => sess.check_id_access(ident.binding_info_id, self.span),
            ast::ExprKind::Literal(_)
            | ast::ExprKind::SelfType
            | ast::ExprKind::NeverType
            | ast::ExprKind::UnitType
            | ast::ExprKind::PlaceholderType => (),
            ast::ExprKind::Error => panic!("unexpected error node"),
        }

        sess.check_type_limits(self);
    }
}
