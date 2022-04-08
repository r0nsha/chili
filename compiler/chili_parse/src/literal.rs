use crate::*;
use chili_ast::ast::{self, ArrayLiteralKind, Expr, ExprKind, Literal, StructLiteralField};
use chili_error::*;
use chili_span::{Span, To};
use chili_token::TokenKind::*;
use codespan_reporting::diagnostic::{Diagnostic, Label};

impl<'p> Parser<'p> {
    pub(crate) fn parse_literal(&mut self) -> DiagnosticResult<Expr> {
        let token = self.previous();
        let span = token.span;

        let value = match &token.kind {
            Nil => Literal::Nil,
            True => Literal::Bool(true),
            False => Literal::Bool(false),
            Int(value) => Literal::Int(*value),
            Float(value) => Literal::Float(*value),
            Str(value) => Literal::Str(value.to_string()),
            Char(value) => Literal::Char(*value),
            _ => {
                return Err(Diagnostic::bug()
                    .with_message(format!("unexpected literal `{}`", token.lexeme))
                    .with_labels(vec![
                        Label::primary(span.file_id, span.range()).with_message("unknown literal")
                    ]));
            }
        };

        Ok(Expr::new(ExprKind::Literal(value), span))
    }

    pub(crate) fn parse_array_literal(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span;

        let mut elements = vec![];
        let mut is_first_el = true;

        while !eat!(self, CloseBracket) && !self.is_end() {
            let expr = self.parse_expr()?;

            if is_first_el {
                if eat!(self, Semicolon) {
                    let len = self.parse_expr()?;
                    expect!(self, CloseBracket, "]")?;

                    return Ok(Expr::new(
                        ExprKind::ArrayLiteral(ArrayLiteralKind::Fill {
                            expr: Box::new(expr),
                            len: Box::new(len),
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
                expect!(self, CloseBracket, "]")?;
                break;
            }
        }

        Ok(Expr::new(
            ExprKind::ArrayLiteral(ArrayLiteralKind::List(elements)),
            start_span.to(self.previous_span()),
        ))
    }

    pub(crate) fn parse_tuple_literal(
        &mut self,
        first_expr: Expr,
        start_span: Span,
    ) -> DiagnosticResult<Expr> {
        let mut exprs =
            parse_delimited_list!(self, CloseParen, Comma, self.parse_expr()?, ", or )");

        exprs.insert(0, first_expr);

        Ok(Expr::new(
            ExprKind::TupleLiteral(exprs),
            start_span.to(self.previous_span()),
        ))
    }

    pub(crate) fn parse_struct_literal(
        &mut self,
        type_expr: Option<Box<Expr>>,
        start_span: Span,
    ) -> DiagnosticResult<Expr> {
        let fields = parse_delimited_list!(
            self,
            CloseCurly,
            Comma,
            {
                let id_token = if eat!(self, Id(_)) {
                    self.previous().clone()
                } else {
                    break;
                };

                let value = if eat!(self, Colon) {
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

                let value_span = value.span;

                StructLiteralField {
                    symbol: id_token.symbol(),
                    value,
                    span: id_token.span.to(value_span),
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
