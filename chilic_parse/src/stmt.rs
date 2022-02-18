use crate::Parser;
use chilic_error::{DiagnosticResult, SyntaxError};
use chilic_ir::{
    entity::{EntityKind, Visibility},
    expr::ExprKind,
    stmt::{Stmt, StmtKind},
};
use chilic_span::Span;
use chilic_token::TokenType::*;

impl Parser {
    pub(crate) fn parse_stmt(&mut self) -> DiagnosticResult<Vec<Stmt>> {
        self.skip_redundant_tokens();

        if self.match_one(Use) {
            let uses = self.parse_use(Visibility::Private)?;

            let stmts: Vec<Stmt> = uses
                .into_iter()
                .map(|use_| {
                    let span = use_.span.clone();
                    Stmt::new(StmtKind::UseDecl(use_), span)
                })
                .collect();

            self.consume_line_terminator()?;

            Ok(stmts)
        } else if self.match_one(Defer) {
            let span = self.span();
            let expr = self.parse_expr()?;

            self.consume_line_terminator()?;

            Ok(vec![Stmt::new(StmtKind::Defer(expr), span)])
        } else if self.match_one(Type) {
            let start_span = self.previous().span.clone();
            let entity = self.parse_entity(
                EntityKind::Type,
                Visibility::Private,
                false,
            )?;

            self.consume_line_terminator()?;

            Ok(vec![Stmt::new(
                StmtKind::Entity(entity),
                Span::merge(&start_span, self.previous_span_ref()),
            )])
        } else if self.match_one(Let) {
            let start_span = self.previous().span.clone();

            let entity = if self.match_one(Foreign) {
                self.parse_foreign_single(Visibility::Private)?
            } else {
                self.parse_entity(
                    EntityKind::Value,
                    Visibility::Private,
                    false,
                )?
            };

            self.consume_line_terminator()?;

            Ok(vec![Stmt::new(
                StmtKind::Entity(entity),
                Span::merge(&start_span, self.previous_span_ref()),
            )])
        } else if self.match_one(Foreign) {
            let start_span = self.previous().span.clone();
            let entitys = self.parse_foreign_block()?;

            let stmts = entitys
                .iter()
                .map(|entity| {
                    Stmt::new(
                        StmtKind::Entity(entity.clone()),
                        Span::merge(&start_span, self.previous_span_ref()),
                    )
                })
                .collect();

            Ok(stmts)
        } else {
            let stmt = self.parse_expr_stmt()?;
            Ok(vec![stmt])
        }
    }

    pub(crate) fn parse_expr_stmt(&mut self) -> DiagnosticResult<Stmt> {
        let expr = self.parse_expr()?;
        let span = expr.span.clone();

        let terminated = self.match_line_terminator();

        Ok(Stmt::new(
            StmtKind::Expr { expr, terminated },
            Span::merge(&span, self.previous_span_ref()),
        ))
    }

    pub(crate) fn parse_stmt_list(&mut self) -> DiagnosticResult<Vec<Stmt>> {
        let mut stmts_list: Vec<Stmt> = vec![];

        self.skip_redundant_tokens();

        while !self.match_one(CloseCurly) && !self.is_end() {
            let stmts = self.parse_stmt()?;
            stmts_list.extend(stmts);
            self.skip_redundant_tokens();
        }

        let len = stmts_list.len();
        for (i, stmt) in stmts_list.iter_mut().enumerate() {
            match &mut stmt.kind {
                StmtKind::Expr { expr, terminated } => {
                    if i == len - 1 {
                        // This isn't the last element, so we don't have to
                        // worry about it
                    } else if !*terminated
                        && !matches!(
                            expr.kind,
                            ExprKind::While { .. }
                                | ExprKind::For { .. }
                                | ExprKind::If { .. }
                                | ExprKind::Block { .. }
                        )
                    {
                        return Err(SyntaxError::expected(
                            &expr.span.end(),
                            Semicolon.lexeme(),
                        ));
                    }
                }
                _ => (),
            }
        }

        Ok(stmts_list)
    }
}
