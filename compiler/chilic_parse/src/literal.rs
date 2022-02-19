use crate::*;
use chilic_error::*;
use chilic_ir::expr::{
    ArrayLiteralKind, Expr, ExprKind, LiteralKind, StructLiteralField,
};
use chilic_span::Span;
use chilic_token::TokenType::*;
use codespan_reporting::diagnostic::{Diagnostic, Label};

impl Parser {
    pub(crate) fn parse_literal(&mut self) -> DiagnosticResult<Expr> {
        let token = self.previous();
        let span = token.span.clone();

        let value = match &token.token_type {
            Nil => LiteralKind::Nil,
            True => LiteralKind::Bool(true),
            False => LiteralKind::Bool(false),
            Int(value) => LiteralKind::Int(*value),
            Float(value) => LiteralKind::Float(*value),
            Str(value) => LiteralKind::Str(value.to_string()),
            Char(value) => LiteralKind::Char(*value),
            _ => {
                return Err(Diagnostic::bug()
                    .with_message(format!(
                        "unexpected literal `{}`",
                        token.lexeme
                    ))
                    .with_labels(vec![Label::primary(
                        span.file_id,
                        span.range,
                    )
                    .with_message("unknown literal")]));
            }
        };

        Ok(Expr::new(ExprKind::Literal(value), span))
    }

    pub(crate) fn parse_array_literal(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span.clone();

        let mut elements = vec![];
        let mut is_first_el = true;

        while !mat!(self, CloseBracket) {
            let expr = self.parse_expr()?;

            if is_first_el {
                if mat!(self, Semicolon) {
                    let len = self.parse_expr()?;
                    req!(self, CloseBracket, "]")?;

                    return Ok(Expr::new(
                        ExprKind::ArrayLiteral(ArrayLiteralKind::Fill {
                            expr: Box::new(expr),
                            len: Box::new(len),
                        }),
                        Span::merge(&start_span, self.previous_span_ref()),
                    ));
                }
                is_first_el = false;
            }

            elements.push(expr);

            if mat!(self, Comma) {
                continue;
            } else {
                req!(self, CloseBracket, "]")?;
                break;
            }
        }

        Ok(Expr::new(
            ExprKind::ArrayLiteral(ArrayLiteralKind::List(elements)),
            Span::merge(&start_span, self.previous_span_ref()),
        ))
    }

    pub(crate) fn parse_tuple_literal(
        &mut self,
        first_expr: Expr,
        start_span: Span,
    ) -> DiagnosticResult<Expr> {
        let mut exprs = vec![first_expr];

        while !mat!(self, CloseParen) && !self.is_end() {
            let expr = self.parse_expr()?;
            exprs.push(expr);

            if mat!(self, Comma) {
                continue;
            } else if mat!(self, CloseParen) {
                break;
            } else {
                return Err(SyntaxError::expected(self.span_ref(), ", or )"));
            }
        }

        Ok(Expr::new(
            ExprKind::TupleLiteral(exprs),
            Span::merge(&start_span, self.previous_span_ref()),
        ))
    }

    pub(crate) fn parse_struct_literal(
        &mut self,
        type_expr: Option<Box<Expr>>,
        start_span: Span,
    ) -> DiagnosticResult<Expr> {
        let mut fields: Vec<StructLiteralField> = vec![];

        while !mat!(self, CloseCurly) && !self.is_end() {
            let id_token = if mat!(self, Id(_)) {
                self.previous().clone()
            } else {
                break;
            };

            let value = if mat!(self, Colon) {
                self.parse_expr()?
            } else {
                Expr::new(
                    ExprKind::Id {
                        symbol: id_token.symbol(),
                        is_mutable: false,
                        entity_span: Span::empty(),
                    },
                    id_token.span.clone(),
                )
            };

            let value_span = value.span.clone();

            fields.push(StructLiteralField {
                symbol: id_token.symbol(),
                value,
                span: Span::merge(&id_token.span, &value_span),
            });

            if mat!(self, Comma) {
                continue;
            } else if mat!(self, CloseCurly) {
                break;
            } else {
                return Err(SyntaxError::expected(self.span_ref(), ", or )"));
            }
        }

        Ok(Expr::new(
            ExprKind::StructLiteral { type_expr, fields },
            Span::merge(&start_span, self.previous_span_ref()),
        ))
    }
}
