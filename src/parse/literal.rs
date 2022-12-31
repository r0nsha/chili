use super::*;
use crate::{
    ast::{self, LiteralKind},
    error::*,
    span::Span,
    token::TokenKind::*,
};

impl Parser {
    pub fn parse_literal(&mut self) -> DiagnosticResult<Ast> {
        let token = self.previous();
        let span = token.span;

        let kind = match &token.kind {
            Nil => LiteralKind::Nil,
            True => LiteralKind::Bool(true),
            False => LiteralKind::Bool(false),
            Int(value) => LiteralKind::Int(*value as i128),
            Float(value) => LiteralKind::Float(*value),
            Str(value) => LiteralKind::Str(*value),
            Char(value) => LiteralKind::Char(*value),
            _ => panic!("unexpected literal `{}`", token.lexeme),
        };

        Ok(Ast::Literal(ast::Literal { kind, span }))
    }

    pub fn parse_array_literal(&mut self, start_span: Span) -> DiagnosticResult<Ast> {
        let mut elements = vec![];
        let mut is_first_el = true;

        while !eat!(self, CloseBracket) && !self.eof() {
            self.skip_newlines();
            let expr = self.parse_expression(false, true)?;

            if is_first_el {
                if eat!(self, Semicolon) {
                    let len = self.parse_expression(false, true)?;
                    require!(self, CloseBracket, "]")?;

                    return Ok(Ast::ArrayLiteral(ast::ArrayLiteral {
                        kind: ast::ArrayLiteralKind::Fill {
                            expr: Box::new(expr),
                            len: Box::new(len),
                        },
                        span: start_span.to(self.previous_span()),
                    }));
                }
                is_first_el = false;
            }

            elements.push(expr);

            if eat!(self, Comma) {
                self.skip_newlines();
                continue;
            } else {
                require!(self, CloseBracket, "]")?;
                break;
            }
        }

        Ok(Ast::ArrayLiteral(ast::ArrayLiteral {
            kind: ast::ArrayLiteralKind::List(elements),
            span: start_span.to(self.previous_span()),
        }))
    }

    pub fn parse_tuple_literal(&mut self, first_expr: Ast, start_span: Span) -> DiagnosticResult<Ast> {
        let mut elements =
            parse_delimited_list!(self, CloseParen, Comma, self.parse_expression(false, true)?, ", or )");

        elements.insert(0, first_expr);

        Ok(Ast::TupleLiteral(ast::TupleLiteral {
            elements,
            span: start_span.to(self.previous_span()),
        }))
    }
}
