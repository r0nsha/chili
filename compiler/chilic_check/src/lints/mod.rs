mod type_limits;

use chilic_error::DiagnosticResult;
use chilic_ir::{
    entity::Entity,
    expr::{ArrayLiteralKind, Builtin, Expr, ExprKind, ForIter},
    ir::Ir,
    stmt::{Stmt, StmtKind},
};

use self::type_limits::check_type_limits;

pub trait Lint {
    fn lint(&self) -> DiagnosticResult<()>;
}

impl<T: Lint> Lint for Vec<T> {
    fn lint(&self) -> DiagnosticResult<()> {
        for element in self {
            element.lint()?;
        }
        Ok(())
    }
}

impl<T: Lint> Lint for Option<T> {
    fn lint(&self) -> DiagnosticResult<()> {
        if let Some(e) = self {
            e.lint()?;
        }
        Ok(())
    }
}

impl<T: Lint> Lint for Box<T> {
    fn lint(&self) -> DiagnosticResult<()> {
        self.as_ref().lint()
    }
}

impl Lint for Ir {
    fn lint(&self) -> DiagnosticResult<()> {
        for module in self.modules.values() {
            for entity in &module.entitys {
                entity.lint()?;
            }
        }
        Ok(())
    }
}

impl Lint for Entity {
    fn lint(&self) -> DiagnosticResult<()> {
        self.value.lint()
    }
}

impl Lint for Stmt {
    fn lint(&self) -> DiagnosticResult<()> {
        match &self.kind {
            StmtKind::Entity(e) => e.lint(),
            StmtKind::Expr { expr, .. } => expr.lint(),
            StmtKind::UseDecl(_) | StmtKind::Defer(_) => Ok(()),
        }
    }
}

impl Lint for Expr {
    fn lint(&self) -> DiagnosticResult<()> {
        match &self.kind {
            ExprKind::Assign { lvalue, rvalue } => {
                lvalue.lint()?;
                rvalue.lint()?;
            }
            ExprKind::Cast(t) => {
                t.expr.lint()?;
            }
            ExprKind::Builtin(b) => match b {
                Builtin::SizeOf(e) | Builtin::AlignOf(e) => {
                    e.lint()?;
                }
                Builtin::Panic(e) => {
                    e.lint()?;
                }
            },
            ExprKind::Fn(f) => {
                f.body.lint()?;
                f.deferred.lint()?;
            }
            ExprKind::While { cond, expr } => {
                cond.lint()?;
                expr.lint()?;
            }
            ExprKind::For {
                iter_name: _,
                iter_index_name: _,
                iterator,
                expr,
            } => {
                match iterator {
                    ForIter::Range(s, e) => {
                        s.lint()?;
                        e.lint()?;
                    }
                    ForIter::Value(v) => {
                        v.lint()?;
                    }
                }
                expr.lint()?;
            }
            ExprKind::Break { deferred } | ExprKind::Continue { deferred } => {
                deferred.lint()?;
            }
            ExprKind::Return { expr, deferred } => {
                expr.lint()?;
                deferred.lint()?;
            }
            ExprKind::If {
                cond,
                then_expr,
                else_expr,
            } => {
                cond.lint()?;
                then_expr.lint()?;
                else_expr.lint()?;
            }
            ExprKind::Block { stmts, deferred } => {
                stmts.lint()?;
                deferred.lint()?;
            }
            ExprKind::Binary { lhs, op: _, rhs } => {
                lhs.lint()?;
                rhs.lint()?;
            }
            ExprKind::Unary { op: _, lhs } => {
                lhs.lint()?;
            }
            ExprKind::Subscript { expr, index } => {
                expr.lint()?;
                index.lint()?;
            }
            ExprKind::Slice { expr, low, high } => {
                expr.lint()?;
                low.lint()?;
                high.lint()?;
            }
            ExprKind::Call(c) => {
                c.callee.lint()?;
                for a in &c.args {
                    a.value.lint()?;
                }
            }
            ExprKind::MemberAccess { expr, member: _ } => {
                expr.lint()?;
            }
            ExprKind::ArrayLiteral(k) => match k {
                ArrayLiteralKind::List(l) => {
                    l.lint()?;
                }
                ArrayLiteralKind::Fill { len, expr } => {
                    len.lint()?;
                    expr.lint()?;
                }
            },
            ExprKind::TupleLiteral(l) => {
                l.lint()?;
            }
            ExprKind::StructLiteral { type_expr, fields } => {
                type_expr.lint()?;
                for f in fields {
                    f.value.lint()?;
                }
            }
            ExprKind::PointerType(e, _) => {
                e.lint()?;
            }
            ExprKind::MultiPointerType(e, _) => {
                e.lint()?;
            }
            ExprKind::ArrayType(e, _) => {
                e.lint()?;
            }
            ExprKind::SliceType(e, _) => {
                e.lint()?;
            }
            ExprKind::StructType(s) => {
                for f in &s.fields {
                    f.ty.lint()?;
                }
            }
            ExprKind::FnType(proto) => {
                for p in &proto.params {
                    p.ty.lint()?;
                }
                proto.ret.lint()?;
            }

            ExprKind::Literal(_)
            | ExprKind::Id { .. }
            | ExprKind::SelfType
            | ExprKind::NeverType
            | ExprKind::UnitType
            | ExprKind::PlaceholderType
            | ExprKind::Noop => (),
        }

        check_type_limits(self)?;

        Ok(())
    }
}
