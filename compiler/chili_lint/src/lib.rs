mod access;
mod lvalue_access;
mod ref_access;
mod sess;
mod type_limits;

use access::{check_assign_lvalue_id_access, check_id_access};
use chili_ast::{ast, pattern::Pattern, workspace::Workspace};
use chili_error::{DiagnosticResult, TypeError};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use common::scopes::Scopes;
use lvalue_access::check_lvalue_access;
use ref_access::check_expr_can_be_mutably_referenced;
use sess::{InitState, LintSess};
use type_limits::check_type_limits;

pub fn lint(workspace: &Workspace, asts: &Vec<ast::Ast>) -> DiagnosticResult<()> {
    let mut sess = LintSess {
        workspace,
        init_scopes: Scopes::new(),
    };

    sess.init_scopes.push_scope();

    for ast in asts.iter() {
        ast.lint(&mut sess)?;
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
        let init_state = if self.value.is_some() {
            InitState::Init
        } else {
            InitState::NotInit
        };

        for symbol in self.pattern.symbols() {
            sess.init_scopes.insert(symbol.binding_info_idx, init_state);
        }

        self.value.lint(sess)?;

        if let Some(value) = &self.value {
            let is_a_type = value.ty.is_type();

            match &self.kind {
                ast::BindingKind::Let => {
                    if is_a_type {
                        return Err(TypeError::expected(
                            value.span,
                            value.ty.to_string(),
                            "a value",
                        ));
                    }
                }
                ast::BindingKind::Type => {
                    if !is_a_type {
                        return Err(TypeError::expected(
                            value.span,
                            value.ty.to_string(),
                            "a type",
                        ));
                    }
                }
                ast::BindingKind::Import => (),
            }

            // * don't allow types to be bounded to mutable bindings
            match &self.pattern {
                Pattern::Single(pat) => {
                    if pat.is_mutable {
                        return Err(Diagnostic::error()
                            .with_message("variable of type `type` must be immutable")
                            .with_labels(vec![Label::primary(
                                pat.span.file_id,
                                pat.span.range().clone(),
                            )])
                            .with_notes(vec![String::from(
                                "try removing the `mut` from the declaration",
                            )]));
                    }
                }
                Pattern::StructDestructor(_) | Pattern::TupleDestructor(_) => (),
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
            ast::ExprKind::Assign { lvalue, rvalue } => {
                lvalue.lint(sess)?;
                rvalue.lint(sess)?;

                match &lvalue.kind {
                    ast::ExprKind::Id {
                        binding_info_idx, ..
                    } => {
                        check_assign_lvalue_id_access(sess, lvalue, *binding_info_idx)?;
                    }
                    _ => {
                        check_lvalue_access(lvalue, lvalue.span)?;
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
                let ty = f.proto.ty.as_fn();

                // if this is the main function, check its type matches a fn() -> [() | !]
                if f.is_entry_point
                    && (!(ty.ret.is_unit() || ty.ret.is_never())
                        || !ty.params.is_empty()
                        || ty.variadic)
                {
                    return Err(Diagnostic::error()
                        .with_message(
                            "entry point function `main` has wrong type, expected `fn() -> ()`",
                        )
                        .with_labels(vec![Label::primary(
                            self.span.file_id,
                            self.span.range().clone(),
                        )]));
                }

                f.body.lint(sess)?;
            }
            ast::ExprKind::While { cond, expr } => {
                cond.lint(sess)?;
                expr.lint(sess)?;
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
                for_.expr.lint(sess)?;
            }
            ast::ExprKind::Break { deferred } | ast::ExprKind::Continue { deferred } => {
                deferred.lint(sess)?;
            }
            ast::ExprKind::Return { expr, deferred } => {
                expr.lint(sess)?;
                deferred.lint(sess)?;
            }
            ast::ExprKind::If {
                cond,
                then_expr,
                else_expr,
            } => {
                cond.lint(sess)?;
                then_expr.lint(sess)?;
                else_expr.lint(sess)?;
            }
            ast::ExprKind::Block(block) => {
                block.lint(sess)?;
            }
            ast::ExprKind::Binary { lhs, op: _, rhs } => {
                lhs.lint(sess)?;
                rhs.lint(sess)?;
            }
            ast::ExprKind::Unary { op, lhs } => {
                lhs.lint(sess)?;

                match op {
                    ast::UnaryOp::Ref(is_mutable_ref) => {
                        if *is_mutable_ref {
                            check_expr_can_be_mutably_referenced(sess, lhs)?;
                        }
                    }
                    _ => (),
                }
            }
            ast::ExprKind::Subscript { expr, index } => {
                expr.lint(sess)?;
                index.lint(sess)?;
            }
            ast::ExprKind::Slice { expr, low, high } => {
                expr.lint(sess)?;
                low.lint(sess)?;
                high.lint(sess)?;
            }
            ast::ExprKind::Call(c) => {
                c.callee.lint(sess)?;
                for a in &c.args {
                    a.value.lint(sess)?;
                }
            }
            ast::ExprKind::MemberAccess { expr, member: _ } => {
                expr.lint(sess)?;
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
            ast::ExprKind::StructLiteral { type_expr, fields } => {
                type_expr.lint(sess)?;
                for f in fields {
                    f.value.lint(sess)?;
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
            ast::ExprKind::FnType(proto) => {
                for p in &proto.params {
                    p.ty.lint(sess)?;
                }
                proto.ret.lint(sess)?;
            }

            ast::ExprKind::Id {
                binding_info_idx, ..
            } => {
                check_id_access(sess, *binding_info_idx, self.span)?;
            }

            ast::ExprKind::Literal(_)
            | ast::ExprKind::SelfType
            | ast::ExprKind::NeverType
            | ast::ExprKind::UnitType
            | ast::ExprKind::PlaceholderType
            | ast::ExprKind::Noop => (),
        }

        check_type_limits(self)?;

        Ok(())
    }
}
