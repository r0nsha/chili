use crate::*;
use chilic_ast::expr::{
    ArrayLiteralKind, Expr, ExprKind, LiteralKind, StructLiteralField,
};
use chilic_error::*;
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

        while !match_token!(self, CloseBracket) && !self.is_end() {
            let expr = self.parse_expr()?;

            if is_first_el {
                if match_token!(self, Semicolon) {
                    let len = self.parse_expr()?;
                    require!(self, CloseBracket, "]")?;

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

            if match_token!(self, Comma) {
                continue;
            } else {
                require!(self, CloseBracket, "]")?;
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
        let mut exprs = parse_delimited_list!(
            self,
            CloseParen,
            Comma,
            self.parse_expr()?,
            ", or )"
        );

        exprs.insert(0, first_expr);

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
        let fields = parse_delimited_list!(
            self,
            CloseCurly,
            Comma,
            {
                let id_token = if match_token!(self, Id(_)) {
                    self.previous().clone()
                } else {
                    break;
                };

                let value = if match_token!(self, Colon) {
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

                StructLiteralField {
                    symbol: id_token.symbol(),
                    value,
                    span: Span::merge(&id_token.span, &value_span),
                }
            },
            ", or }"
        );

        Ok(Expr::new(
            ExprKind::StructLiteral { type_expr, fields },
            Span::merge(&start_span, self.previous_span_ref()),
        ))
    }
}
