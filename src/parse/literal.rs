use super::*;
use crate::ast::ast::{self, Expr, ExprKind, LiteralKind, StructLiteralField};
use crate::error::*;
use crate::span::{Span, To};
use crate::token::TokenKind::*;

impl Parser {
    pub fn parse_literal(&mut self) -> DiagnosticResult<Expr> {
        let token = self.previous();
        let span = token.span;

        let kind = match &token.kind {
            Nil => LiteralKind::Nil,
            True => LiteralKind::Bool(true),
            False => LiteralKind::Bool(false),
            Int(value) => LiteralKind::Int(*value),
            Float(value) => LiteralKind::Float(*value),
            Str(value) => LiteralKind::Str(*value),
            Char(value) => LiteralKind::Char(*value),
            _ => panic!("unexpected literal `{}`", token.lexeme),
        };

        Ok(Expr::new(
            ExprKind::Literal(ast::Literal { kind, span }),
            span,
        ))
    }

    pub fn parse_array_literal(&mut self, start_span: Span) -> DiagnosticResult<Expr> {
        let mut elements = vec![];
        let mut is_first_el = true;

        while !eat!(self, CloseBracket) && !self.is_end() {
            let expr = self.parse_expr()?;

            if is_first_el {
                if eat!(self, Semicolon) {
                    let len = self.parse_expr()?;
                    require!(self, CloseBracket, "]")?;

                    return Ok(Expr::new(
                        ExprKind::ArrayLiteral(ast::ArrayLiteral {
                            kind: ast::ArrayLiteralKind::Fill {
                                expr: Box::new(expr),
                                len: Box::new(len),
                            },
                        }),
                        start_span.to(self.previous_span()),
                    ));
                }
                is_first_el = false;
            }

            elements.push(expr);

            if eat!(self, Comma) {
                continue;
            } else {
                require!(self, CloseBracket, "]")?;
                break;
            }
        }

        Ok(Expr::new(
            ExprKind::ArrayLiteral(ast::ArrayLiteral {
                kind: ast::ArrayLiteralKind::List(elements),
            }),
            start_span.to(self.previous_span()),
        ))
    }

    pub fn parse_tuple_literal(
        &mut self,
        first_expr: Expr,
        start_span: Span,
    ) -> DiagnosticResult<Expr> {
        let mut elements =
            parse_delimited_list!(self, CloseParen, Comma, self.parse_expr()?, ", or )");

        elements.insert(0, first_expr);

        Ok(Expr::new(
            ExprKind::TupleLiteral(ast::TupleLiteral { elements }),
            start_span.to(self.previous_span()),
        ))
    }

    pub fn parse_struct_literal(
        &mut self,
        type_expr: Option<Box<Expr>>,
        start_span: Span,
    ) -> DiagnosticResult<Expr> {
        let fields = parse_delimited_list!(
            self,
            CloseCurly,
            Comma,
            {
                let id_token = if eat!(self, Ident(_)) {
                    *self.previous()
                } else {
                    break;
                };

                let expr = if eat!(self, Colon) {
                    self.parse_expr()?
                } else {
                    Expr::new(
                        ExprKind::Ident(ast::Ident {
                            symbol: id_token.symbol(),
                            binding_info_id: Default::default(),
                        }),
                        id_token.span,
                    )
                };

                let span = expr.span;

                StructLiteralField {
                    symbol: id_token.symbol(),
                    expr,
                    span: id_token.span.to(span),
                }
            },
            ", or }"
        );

        Ok(Expr::new(
            ExprKind::StructLiteral(ast::StructLiteral { type_expr, fields }),
            start_span.to(self.previous_span()),
        ))
    }
}
