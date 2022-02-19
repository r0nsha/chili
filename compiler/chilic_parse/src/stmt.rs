use crate::*;
use chilic_ast::{
    entity::{EntityKind, Visibility},
    stmt::{Stmt, StmtKind},
};
use chilic_error::{DiagnosticResult, SyntaxError};
use chilic_span::Span;
use chilic_token::TokenType::*;

impl Parser {
    pub(crate) fn parse_stmt(&mut self) -> DiagnosticResult<Vec<Stmt>> {
        self.skip_redundant_tokens();

        if match_token!(self, Use) {
            let uses = self.parse_use(Visibility::Private)?;

            let stmts: Vec<Stmt> = uses
                .into_iter()
                .map(|use_| {
                    let span = use_.span.clone();
                    Stmt::new(StmtKind::Use(use_), span)
                })
                .collect();

            require!(self, Semicolon, ";")?;

            Ok(stmts)
        } else if match_token!(self, Defer) {
            let span = self.span();
            let expr = self.parse_expr()?;

            require!(self, Semicolon, ";")?;

            Ok(vec![Stmt::new(StmtKind::Defer(expr), span)])
        } else if match_token!(self, Type) {
            let start_span = self.previous().span.clone();
            let entity = self.parse_entity(
                EntityKind::Type,
                Visibility::Private,
                false,
            )?;

            require!(self, Semicolon, ";")?;

            Ok(vec![Stmt::new(
                StmtKind::Entity(entity),
                Span::merge(&start_span, self.previous_span_ref()),
            )])
        } else if match_token!(self, Let) {
            let start_span = self.previous().span.clone();

            let entity = if match_token!(self, Foreign) {
                self.parse_foreign_single(Visibility::Private)?
            } else {
                self.parse_entity(
                    EntityKind::Value,
                    Visibility::Private,
                    false,
                )?
            };

            require!(self, Semicolon, ";")?;

            Ok(vec![Stmt::new(
                StmtKind::Entity(entity),
                Span::merge(&start_span, self.previous_span_ref()),
            )])
        } else if match_token!(self, Foreign) {
            let start_span = self.previous().span.clone();
            let entities = self.parse_foreign_block()?;

            let stmts = entities
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

        let terminated = match_token!(self, Semicolon);

        Ok(Stmt::new(
            StmtKind::Expr { expr, terminated },
            Span::merge(&span, self.previous_span_ref()),
        ))
    }
}
