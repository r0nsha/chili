use chilic_ast::{
    expr::Block,
    stmt::{Stmt, StmtKind},
};
use chilic_error::DiagnosticResult;
use chilic_ty::*;

use crate::{AnalysisContext, AnalysisFrame, CheckedStmt};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn check_stmt(
        &mut self,
        frame: &mut AnalysisFrame,
        stmt: &Stmt,
    ) -> DiagnosticResult<CheckedStmt> {
        let checked_stmt = match &stmt.kind {
            StmtKind::Use(use_) => {
                let entity_info =
                    self.check_use(frame.module_info.name, use_)?;
                let ty = entity_info.ty.clone();

                frame.insert_entity_info(use_.alias, entity_info);

                CheckedStmt::new(StmtKind::Use(use_.clone()), ty, &stmt.span)
            }
            StmtKind::Entity(entity) => {
                let entity = self.check_entity(frame, entity)?;
                CheckedStmt::new(StmtKind::Entity(entity), Ty::Unit, &stmt.span)
            }
            StmtKind::Defer(expr) => CheckedStmt::new(
                StmtKind::Defer(expr.clone()),
                Ty::Unit,
                &stmt.span,
            ),
            StmtKind::Expr { expr, terminated } => {
                let result = self.check_expr(
                    frame,
                    expr,
                    if *terminated {
                        None
                    } else {
                        frame.expected_return_ty.clone()
                    },
                )?;

                let ty = self.infcx.normalize_ty(&result.ty);

                let ty = if *terminated {
                    match ty {
                        Ty::Never => Ty::Never,
                        _ => Ty::Unit,
                    }
                } else {
                    ty
                };

                CheckedStmt::new(
                    StmtKind::Expr {
                        expr: result.expr,
                        terminated: *terminated,
                    },
                    ty,
                    &stmt.span,
                )
            }
        };

        Ok(checked_stmt)
    }

    pub(crate) fn check_block(
        &mut self,
        frame: &mut AnalysisFrame,
        block: &Block,
        parent_ty: Option<Ty>,
    ) -> DiagnosticResult<(Block, Ty)> {
        let mut new_block = Block {
            exprs: vec![],
            deferred: vec![],
            yields: block.yields,
        };

        let mut result_ty = Ty::Unit;

        if !block.exprs.is_empty() {
            let last_index = block.exprs.len() - 1;

            for (index, expr) in block.exprs.iter().enumerate() {
                let is_last = index == last_index;

                let result = self.check_expr(
                    frame,
                    expr,
                    if is_last { parent_ty.clone() } else { None },
                )?;

                new_block.exprs.push(result.expr);

                if is_last {
                    result_ty = result.ty.into();
                }
            }
        }

        new_block.deferred = self.check_expr_list(frame, &block.deferred)?;

        Ok((new_block, result_ty))
    }
}
