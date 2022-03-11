mod type_limits;

use chili_ast::{ast, workspace::Workspace};
use chili_error::DiagnosticResult;
use type_limits::check_type_limits;

pub fn lint<'w>(workspace: &Workspace<'w>, asts: &Vec<ast::Ast>) -> DiagnosticResult<()> {
    for ast in asts.iter() {
        ast.lint(workspace)?;
    }
    Ok(())
}

trait Lint<'w> {
    fn lint(&self, workspace: &Workspace<'w>) -> DiagnosticResult<()>;
}

impl<'w, T: Lint<'w>> Lint<'w> for Vec<T> {
    fn lint(&self, workspace: &Workspace<'w>) -> DiagnosticResult<()> {
        for element in self {
            element.lint(workspace)?;
        }
        Ok(())
    }
}

impl<'w, T: Lint<'w>> Lint<'w> for Option<T> {
    fn lint(&self, workspace: &Workspace<'w>) -> DiagnosticResult<()> {
        if let Some(e) = self {
            e.lint(workspace)?;
        }
        Ok(())
    }
}

impl<'w, T: Lint<'w>> Lint<'w> for Box<T> {
    fn lint(&self, workspace: &Workspace<'w>) -> DiagnosticResult<()> {
        self.as_ref().lint(workspace)
    }
}

impl<'w> Lint<'w> for ast::Ast {
    fn lint(&self, workspace: &Workspace<'w>) -> DiagnosticResult<()> {
        for binding in self.bindings.iter() {
            binding.lint(workspace)?;
        }
        Ok(())
    }
}

impl<'w> Lint<'w> for ast::Binding {
    fn lint(&self, workspace: &Workspace<'w>) -> DiagnosticResult<()> {
        self.value.lint(workspace)
    }
}

impl<'w> Lint<'w> for ast::Block {
    fn lint(&self, workspace: &Workspace<'w>) -> DiagnosticResult<()> {
        self.exprs.lint(workspace)?;
        self.deferred.lint(workspace)?;
        Ok(())
    }
}

impl<'w> Lint<'w> for ast::Expr {
    fn lint(&self, workspace: &Workspace<'w>) -> DiagnosticResult<()> {
        match &self.kind {
            ast::ExprKind::Import(_) | ast::ExprKind::Defer(_) => (),
            ast::ExprKind::Foreign(e) => {
                e.lint(workspace)?;
            }
            ast::ExprKind::Binding(e) => {
                e.lint(workspace)?;
            }
            ast::ExprKind::Assign { lvalue, rvalue } => {
                lvalue.lint(workspace)?;
                rvalue.lint(workspace)?;
            }
            ast::ExprKind::Cast(t) => {
                t.expr.lint(workspace)?;
            }
            ast::ExprKind::Builtin(b) => match b {
                ast::Builtin::SizeOf(e) | ast::Builtin::AlignOf(e) => {
                    e.lint(workspace)?;
                }
                ast::Builtin::Panic(e) => {
                    e.lint(workspace)?;
                }
            },
            ast::ExprKind::Fn(f) => {
                f.body.lint(workspace)?;
            }
            ast::ExprKind::While { cond, expr } => {
                cond.lint(workspace)?;
                expr.lint(workspace)?;
            }
            ast::ExprKind::For(for_) => {
                match &for_.iterator {
                    ast::ForIter::Range(s, e) => {
                        s.lint(workspace)?;
                        e.lint(workspace)?;
                    }
                    ast::ForIter::Value(v) => {
                        v.lint(workspace)?;
                    }
                }
                for_.expr.lint(workspace)?;
            }
            ast::ExprKind::Break { deferred } | ast::ExprKind::Continue { deferred } => {
                deferred.lint(workspace)?;
            }
            ast::ExprKind::Return { expr, deferred } => {
                expr.lint(workspace)?;
                deferred.lint(workspace)?;
            }
            ast::ExprKind::If {
                cond,
                then_expr,
                else_expr,
            } => {
                cond.lint(workspace)?;
                then_expr.lint(workspace)?;
                else_expr.lint(workspace)?;
            }
            ast::ExprKind::Block(block) => {
                block.lint(workspace)?;
            }
            ast::ExprKind::Binary { lhs, op: _, rhs } => {
                lhs.lint(workspace)?;
                rhs.lint(workspace)?;
            }
            ast::ExprKind::Unary { op: _, lhs } => {
                lhs.lint(workspace)?;
            }
            ast::ExprKind::Subscript { expr, index } => {
                expr.lint(workspace)?;
                index.lint(workspace)?;
            }
            ast::ExprKind::Slice { expr, low, high } => {
                expr.lint(workspace)?;
                low.lint(workspace)?;
                high.lint(workspace)?;
            }
            ast::ExprKind::Call(c) => {
                c.callee.lint(workspace)?;
                for a in &c.args {
                    a.value.lint(workspace)?;
                }
            }
            ast::ExprKind::MemberAccess { expr, member: _ } => {
                expr.lint(workspace)?;
            }
            ast::ExprKind::ArrayLiteral(k) => match k {
                ast::ArrayLiteralKind::List(l) => {
                    l.lint(workspace)?;
                }
                ast::ArrayLiteralKind::Fill { len, expr } => {
                    len.lint(workspace)?;
                    expr.lint(workspace)?;
                }
            },
            ast::ExprKind::TupleLiteral(l) => {
                l.lint(workspace)?;
            }
            ast::ExprKind::StructLiteral { type_expr, fields } => {
                type_expr.lint(workspace)?;
                for f in fields {
                    f.value.lint(workspace)?;
                }
            }
            ast::ExprKind::PointerType(e, _) => {
                e.lint(workspace)?;
            }
            ast::ExprKind::MultiPointerType(e, _) => {
                e.lint(workspace)?;
            }
            ast::ExprKind::ArrayType(e, _) => {
                e.lint(workspace)?;
            }
            ast::ExprKind::SliceType(e, _) => {
                e.lint(workspace)?;
            }
            ast::ExprKind::StructType(s) => {
                for f in &s.fields {
                    f.ty.lint(workspace)?;
                }
            }
            ast::ExprKind::FnType(proto) => {
                for p in &proto.params {
                    p.ty.lint(workspace)?;
                }
                proto.ret.lint(workspace)?;
            }

            ast::ExprKind::Literal(_)
            | ast::ExprKind::Id { .. }
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
