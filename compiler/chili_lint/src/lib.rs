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
use chili_check::{normalize::NormalizeTy, ty_ctx::TyCtx};
use chili_error::{DiagnosticResult, TypeError};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use common::scopes::Scopes;
use sess::{InitState, LintSess};

pub fn lint(workspace: &Workspace, tycx: &TyCtx, ast: &TypedAst) -> DiagnosticResult<()> {
    let mut sess = LintSess {
        workspace,
        tycx,
        init_scopes: Scopes::new(),
    };

    sess.init_scopes.push_scope();

    for binding in ast.bindings.values() {
        binding.lint(&mut sess)?;
    }

    sess.init_scopes.push_scope();

    Ok(())
}

trait Lint {
    fn lint(&self, sess: &mut LintSess) -> DiagnosticResult<()>;
}

impl<T: Lint> Lint for Vec<T> {
    fn lint(&self, sess: &mut LintSess) -> DiagnosticResult<()> {
        for element in self {
            element.lint(sess)?;
        }
        Ok(())
    }
}

impl<T: Lint> Lint for Option<T> {
    fn lint(&self, sess: &mut LintSess) -> DiagnosticResult<()> {
        if let Some(e) = self {
            e.lint(sess)?;
        }
        Ok(())
    }
}

impl<T: Lint> Lint for Box<T> {
    fn lint(&self, sess: &mut LintSess) -> DiagnosticResult<()> {
        self.as_ref().lint(sess)
    }
}

impl Lint for ast::Ast {
    fn lint(&self, sess: &mut LintSess) -> DiagnosticResult<()> {
        for binding in self.bindings.iter() {
            binding.lint(sess)?;
        }
        Ok(())
    }
}

impl Lint for ast::Binding {
    fn lint(&self, sess: &mut LintSess) -> DiagnosticResult<()> {
        let init_state = if self.expr.is_some() {
            InitState::Init
        } else {
            InitState::NotInit
        };

        for symbol in self.pattern.symbols() {
            sess.init_scopes.insert(symbol.binding_info_id, init_state);
        }

        self.expr.lint(sess)?;

        if let Some(expr) = &self.expr {
            let is_a_type = expr.ty.normalize(&sess.tycx).is_type();

            match &self.kind {
                ast::BindingKind::Value => {
                    if is_a_type {
                        return Err(TypeError::expected(
                            expr.span,
                            expr.ty.to_string(),
                            "a value",
                        ));
                    }
                }
                ast::BindingKind::Type => {
                    if !is_a_type {
                        return Err(TypeError::expected(
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
                            return Err(Diagnostic::error()
                                .with_message("variable of type `type` must be immutable")
                                .with_labels(vec![Label::primary(
                                    pat.span.file_id,
                                    pat.span.range(),
                                )])
                                .with_notes(vec![String::from(
                                    "try removing the `mut` from the declaration",
                                )]));
                        }
                    }
                    Pattern::StructUnpack(_) | Pattern::TupleUnpack(_) => (),
                }
            }
        }

        Ok(())
    }
}

impl Lint for ast::Block {
    fn lint(&self, sess: &mut LintSess) -> DiagnosticResult<()> {
        sess.init_scopes.push_scope();
        self.exprs.lint(sess)?;
        self.deferred.lint(sess)?;
        sess.init_scopes.push_scope();
        Ok(())
    }
}

impl Lint for ast::Expr {
    fn lint(&self, sess: &mut LintSess) -> DiagnosticResult<()> {
        match &self.kind {
            ast::ExprKind::Import(_) | ast::ExprKind::Defer(_) => (),
            ast::ExprKind::Foreign(e) => {
                e.lint(sess)?;
            }
            ast::ExprKind::Binding(e) => {
                e.lint(sess)?;
            }
            ast::ExprKind::Assign(assign) => {
                assign.rvalue.lint(sess)?;

                match &assign.lvalue.kind {
                    ast::ExprKind::Ident(ident) => {
                        sess.check_assign_lvalue_id_access(&assign.lvalue, ident.binding_info_id)?;
                    }
                    _ => {
                        sess.check_lvalue_access(&assign.lvalue, assign.lvalue.span)?;
                        assign.lvalue.lint(sess)?;
                    }
                };
            }
            ast::ExprKind::Cast(t) => {
                t.expr.lint(sess)?;
            }
            ast::ExprKind::Builtin(b) => match b {
                ast::Builtin::SizeOf(e) | ast::Builtin::AlignOf(e) => {
                    e.lint(sess)?;
                }
                ast::Builtin::Panic(e) => {
                    e.lint(sess)?;
                }
            },
            ast::ExprKind::Fn(f) => {
                let ty = f.sig.ty.normalize(&sess.tycx).into_fn();

                // if this is the main function, check its type matches a fn() -> [() | !]
                if f.is_entry_point
                    && (!(ty.ret.is_unit() || ty.ret.is_never())
                        || !ty.params.is_empty()
                        || ty.variadic)
                {
                    return Err(Diagnostic::error()
                        .with_message(format!(
                            "entry point function `main` has type `{}`, expected `fn() -> ()`",
                            ty
                        ))
                        .with_labels(vec![Label::primary(self.span.file_id, self.span.range())]));
                }

                f.body.lint(sess)?;
            }
            ast::ExprKind::While(while_) => {
                while_.cond.lint(sess)?;
                while_.block.lint(sess)?;
            }
            ast::ExprKind::For(for_) => {
                match &for_.iterator {
                    ast::ForIter::Range(s, e) => {
                        s.lint(sess)?;
                        e.lint(sess)?;
                    }
                    ast::ForIter::Value(v) => {
                        v.lint(sess)?;
                    }
                }
                for_.block.lint(sess)?;
            }
            ast::ExprKind::Break(e) | ast::ExprKind::Continue(e) => {
                e.deferred.lint(sess)?;
            }
            ast::ExprKind::Return(ret) => {
                ret.expr.lint(sess)?;
                ret.deferred.lint(sess)?;
            }
            ast::ExprKind::If(if_) => {
                if_.cond.lint(sess)?;
                if_.then.lint(sess)?;
                if_.otherwise.lint(sess)?;
            }
            ast::ExprKind::Block(block) => {
                block.lint(sess)?;
            }
            ast::ExprKind::Binary(binary) => {
                binary.lhs.lint(sess)?;
                binary.rhs.lint(sess)?;
            }
            ast::ExprKind::Unary(unary) => {
                unary.lhs.lint(sess)?;

                match &unary.op {
                    ast::UnaryOp::Ref(is_mutable_ref) => {
                        if *is_mutable_ref {
                            sess.check_expr_can_be_mutably_referenced(&unary.lhs)?;
                        }
                    }
                    _ => (),
                }
            }
            ast::ExprKind::Subscript(sub) => {
                sub.expr.lint(sess)?;
                sub.index.lint(sess)?;
            }
            ast::ExprKind::Slice(slice) => {
                slice.expr.lint(sess)?;
                slice.low.lint(sess)?;
                slice.high.lint(sess)?;

                if let None = &slice.high {
                    if slice.expr.ty.normalize(&sess.tycx).is_multi_pointer() {
                        return Err(Diagnostic::error()
                        .with_message(
                            "multi pointer has unknown length, so you must specify the ending index",
                        )
                        .with_labels(vec![Label::primary(
                            slice.expr.span.file_id,
                            slice.expr.span.range(),
                        )]));
                    }
                }
            }
            ast::ExprKind::FnCall(call) => {
                call.callee.lint(sess)?;
                for arg in &call.args {
                    arg.expr.lint(sess)?;
                }
            }
            ast::ExprKind::MemberAccess(access) => {
                access.expr.lint(sess)?;
            }
            ast::ExprKind::ArrayLiteral(k) => match k {
                ast::ArrayLiteralKind::List(l) => {
                    l.lint(sess)?;
                }
                ast::ArrayLiteralKind::Fill { len, expr } => {
                    len.lint(sess)?;
                    expr.lint(sess)?;
                }
            },
            ast::ExprKind::TupleLiteral(l) => {
                l.lint(sess)?;
            }
            ast::ExprKind::StructLiteral(lit) => {
                lit.type_expr.lint(sess)?;
                for field in &lit.fields {
                    field.value.lint(sess)?;
                }
            }
            ast::ExprKind::PointerType(e, _) => {
                e.lint(sess)?;
            }
            ast::ExprKind::MultiPointerType(e, _) => {
                e.lint(sess)?;
            }
            ast::ExprKind::ArrayType(e, _) => {
                e.lint(sess)?;
            }
            ast::ExprKind::SliceType(e, _) => {
                e.lint(sess)?;
            }
            ast::ExprKind::StructType(s) => {
                for f in &s.fields {
                    f.ty.lint(sess)?;
                }
            }
            ast::ExprKind::FnType(sig) => {
                for p in &sig.params {
                    p.ty_expr.lint(sess)?;
                }
                sig.ret.lint(sess)?;
            }

            ast::ExprKind::Ident(ident) => {
                sess.check_id_access(ident.binding_info_id, self.span)?;
            }

            ast::ExprKind::Literal(_)
            | ast::ExprKind::SelfType
            | ast::ExprKind::NeverType
            | ast::ExprKind::UnitType
            | ast::ExprKind::PlaceholderType => (),
        }

        sess.check_type_limits(self)?;

        Ok(())
    }
}
