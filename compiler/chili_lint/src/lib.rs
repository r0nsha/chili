mod access;
mod lvalue_access;
mod ref_access;
mod sess;
mod type_limits;

use chili_ast::{
    ast,
    pattern::{HybridPattern, Pattern},
    workspace::Workspace,
};
use chili_error::diagnostic::{Diagnostic, Label};
use chili_infer::{normalize::Normalize, ty_ctx::TyCtx};
use chili_span::Span;
use common::{build_options::CodegenOptions, scopes::Scopes};
use sess::{InitState, LintSess};

pub fn lint(workspace: &mut Workspace, tycx: &TyCtx, typed_ast: &ast::TypedAst) {
    let mut sess = LintSess {
        workspace,
        tycx,
        init_scopes: Scopes::default(),
    };

    sess.init_scopes.push_scope();

    for binding in typed_ast.bindings.iter() {
        binding.lint(&mut sess);
    }

    sess.init_scopes.pop_scope();

    // Check that an entry point function exists
    // TODO (Ron): This won't be relevant for future targets like WASM, DLLs and libraries
    if !workspace.build_options.need_entry_point_function() {
        if let Some(binding_info) = workspace.entry_point_function() {
            let ty = binding_info.ty.normalize(tycx).into_fn();

            // if this is the main function, check its type matches a fn() -> [unit | never]
            if !(ty.ret.is_unit() || ty.ret.is_never())
                || !ty.params.is_empty()
                || ty.varargs.is_some()
            {
                workspace.diagnostics.push(
                    Diagnostic::error()
                        .with_message(format!(
                            "entry point function `main` has type `{}`, expected `fn() -> ()`",
                            ty
                        ))
                        .with_label(Label::primary(
                            binding_info.span,
                            "invalid type of entry point function",
                        )),
                );
            }
        } else if workspace.build_options.need_entry_point_function() {
            workspace.diagnostics.push(
                Diagnostic::error()
                    .with_message("entry point function `main` is not defined")
                    .with_label(Label::primary(
                        Span::initial(workspace.get_root_module_info().file_id),
                        "",
                    ))
                    .with_note("define function `let main = fn() {}` in your entry file"),
            );
        }
    }
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

        for symbol in self.pattern.iter() {
            sess.init_scopes.insert(symbol.id, init_state);
        }

        self.expr.lint(sess);

        if let Some(expr) = &self.expr {
            let is_a_type = expr.ty.normalize(sess.tycx).is_type();

            // * don't allow types to be bounded to mutable bindings
            if is_a_type {
                match &self.pattern {
                    Pattern::Symbol(symbol) | Pattern::Hybrid(HybridPattern { symbol, .. }) => {
                        if symbol.is_mutable {
                            sess.workspace.diagnostics.push(
                                Diagnostic::error()
                                    .with_message("variable of type `type` must be immutable")
                                    .with_label(Label::primary(symbol.span, "variable is mutable"))
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
            ast::ExprKind::Binding(binding) => binding.lint(sess),
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
                ast::BuiltinKind::Import(_) => (),
                ast::BuiltinKind::LangItem(_) => panic!("unexpected lang_item"),
                ast::BuiltinKind::SizeOf(e)
                | ast::BuiltinKind::AlignOf(e)
                | ast::BuiltinKind::Run(e, _) => e.lint(sess),
                ast::BuiltinKind::Panic(e) => e.lint(sess),
            },
            ast::ExprKind::Function(f) => {
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

                if let ast::UnaryOp::Ref(is_mutable_ref) = &unary.op {
                    if *is_mutable_ref {
                        sess.check_expr_can_be_mutably_referenced(&unary.lhs);
                    }
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

                if slice.high.is_none() {
                    if slice.expr.ty.normalize(sess.tycx).is_multi_pointer() {
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
                    field.expr.lint(sess);
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
            ast::ExprKind::FunctionType(sig) => {
                for p in &sig.params {
                    p.ty_expr.lint(sess);
                }
                sig.ret.lint(sess);
            }
            ast::ExprKind::Ident(ident) => sess.check_id_access(ident.binding_info_id, self.span),
            ast::ExprKind::Literal(_) => {
                panic!("Literal expression should have been lowered to a ConstValue")
            }
            ast::ExprKind::SelfType | ast::ExprKind::ConstValue(_) | ast::ExprKind::Placeholder => {
                ()
            }
            ast::ExprKind::Error => panic!("unexpected error node"),
        }

        sess.check_type_limits(self);
    }
}
