use super::*;
use crate::{
    ast::{self, LiteralKind, StructLiteralField},
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
            Int(value) => LiteralKind::Int(*value),
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

    pub fn parse_struct_literal(&mut self, type_expr: Option<Box<Ast>>) -> DiagnosticResult<Ast> {
        let open_curly_span = require!(self, OpenCurly, "{")?.span;
        let start_span = type_expr.as_ref().map(|e| e.span()).unwrap_or(open_curly_span);

        self.skip_newlines();

        let fields = parse_delimited_list!(
            self,
            CloseCurly,
            Comma,
            {
                let id_token = require!(self, Ident(_), "an identifier")?;

                self.skip_newlines();

                let expr = if eat!(self, Colon) {
                    self.parse_expression(false, true)?
                } else {
                    Ast::Ident(ast::Ident {
                        name: id_token.name(),
                        span: id_token.span,
                    })
                };

                StructLiteralField {
                    name: id_token.name(),
                    expr,
                    span: id_token.span,
                }
            },
            ", or }"
        );

        Ok(Ast::StructLiteral(ast::StructLiteral {
            type_expr,
            fields,
            span: start_span.to(self.previous_span()),
        }))
    }

    pub fn parse_struct_literal_or_parse_block_expr(&mut self) -> DiagnosticResult<Ast> {
        let last_index = self.current;
        let start_span = require!(self, OpenCurly, "{")?.span;

        if eat!(self, CloseCurly) {
            Ok(Ast::Block(ast::Block {
                statements: vec![],
                span: start_span.to(self.previous_span()),
            }))
        } else if eat!(self, Ident(_)) {
            if is!(self, Colon | Comma) {
                self.current = last_index;
                self.parse_struct_literal(None)
            } else {
                self.current = last_index;
                Ok(Ast::Block(self.parse_block()?))
            }
        } else {
            self.revert(1);
            Ok(Ast::Block(self.parse_block()?))
        }
    }
}
