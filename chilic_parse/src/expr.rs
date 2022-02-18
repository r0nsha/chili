use chilic_error::*;
use chilic_ir::{
    expr::{
        ArrayLiteralKind, Builtin, Expr, ExprKind, ForIter, LiteralKind,
        StructLiteralField,
    },
    op::{BinaryOp, UnaryOp},
};
use chilic_span::Span;
use chilic_token::TokenType::*;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use common::builtin::{default_index_name, default_iter_name};
use ustr::ustr;

use crate::*;

impl Parser {
    pub(crate) fn parse_expr(&mut self) -> DiagnosticResult<Expr> {
        self.parse_expr_internal(ustr(""))
    }

    pub(crate) fn parse_decl_expr(
        &mut self,
        decl_name: Ustr,
    ) -> DiagnosticResult<Expr> {
        self.parse_expr_internal(decl_name)
    }

    fn parse_expr_internal(
        &mut self,
        decl_name: Ustr,
    ) -> DiagnosticResult<Expr> {
        self.decl_name_frames.push(decl_name);

        let expr = if self.match_one(If) {
            self.parse_if()
        } else if self.match_one(While) {
            self.parse_while()
        } else if self.match_one(For) {
            self.parse_for()
        } else if self.match_one(OpenCurly) {
            self.parse_block()
        } else {
            self.parse_logic_or()
        }?;

        let expr = self.parse_postfix_expr(expr)?;

        self.decl_name_frames.pop();

        Ok(expr)
    }

    pub(crate) fn parse_if(&mut self) -> DiagnosticResult<Expr> {
        let token = self.previous();
        let span = token.span.clone();

        let cond = self.parse_expr()?;

        self.consume(OpenCurly)?;
        let then_expr = self.parse_block()?;

        let else_expr = if self.match_one(Else) {
            let expr = if self.match_one(If) {
                self.parse_if()?
            } else {
                self.consume(OpenCurly)?;
                self.parse_block()?
            };

            Some(Box::new(expr))
        } else {
            None
        };

        Ok(Expr::new(
            ExprKind::If {
                cond: Box::new(cond),
                then_expr: Box::new(then_expr),
                else_expr,
            },
            Span::merge(&span, self.previous_span_ref()),
        ))
    }

    pub(crate) fn parse_block(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span.clone();
        let stmts = self.parse_stmt_list()?;

        Ok(Expr::new(
            ExprKind::Block {
                stmts,
                deferred: vec![],
            },
            Span::merge(&start_span, self.previous_span_ref()),
        ))
    }

    pub(crate) fn parse_logic_or(&mut self) -> DiagnosticResult<Expr> {
        let mut expr = self.parse_logic_and()?;

        let start_span = expr.span.clone();

        while self.match_one(BarBar) {
            expr = Expr::new(
                ExprKind::Binary {
                    lhs: Box::new(expr),
                    op: BinaryOp::Or,
                    rhs: Box::new(self.parse_logic_and()?),
                },
                Span::new(
                    start_span.range.start..self.previous().span.range.end,
                    start_span.file_id,
                ),
            );
        }

        Ok(expr)
    }

    pub(crate) fn parse_logic_and(&mut self) -> DiagnosticResult<Expr> {
        let mut expr = self.parse_comparison()?;

        let start_span = expr.span.clone();

        while self.match_one(AmpAmp) {
            expr = Expr::new(
                ExprKind::Binary {
                    lhs: Box::new(expr),
                    op: BinaryOp::And,
                    rhs: Box::new(self.parse_comparison()?),
                },
                Span::new(
                    start_span.range.start..self.previous().span.range.end,
                    start_span.file_id,
                ),
            );
        }

        Ok(expr)
    }

    pub(crate) fn parse_comparison(&mut self) -> DiagnosticResult<Expr> {
        let mut expr = self.parse_bitwise_or()?;

        let start_span = expr.span.clone();

        while self.match_any(&[BangEq, EqEq, Gt, GtEq, Lt, LtEq]) {
            expr = Expr::new(
                ExprKind::Binary {
                    lhs: Box::new(expr),
                    op: self.previous().token_type.clone().into(),
                    rhs: Box::new(self.parse_bitwise_or()?),
                },
                Span::new(
                    start_span.range.start..self.previous().span.range.end,
                    start_span.file_id,
                ),
            );
        }

        Ok(expr)
    }

    pub(crate) fn parse_bitwise_or(&mut self) -> DiagnosticResult<Expr> {
        let mut expr = self.parse_bitwise_xor()?;

        let start_span = expr.span.clone();

        while self.match_one(Bar) {
            if self.peek().is(Eq) {
                self.revert(1);
                return Ok(expr);
            }

            expr = Expr::new(
                ExprKind::Binary {
                    lhs: Box::new(expr),
                    op: BinaryOp::BitwiseOr,
                    rhs: Box::new(self.parse_bitwise_xor()?),
                },
                Span::new(
                    start_span.range.start..self.previous().span.range.end,
                    start_span.file_id,
                ),
            );
        }

        Ok(expr)
    }

    pub(crate) fn parse_bitwise_xor(&mut self) -> DiagnosticResult<Expr> {
        let mut expr = self.parse_bitwise_and()?;

        let start_span = expr.span.clone();

        while self.match_one(Caret) {
            if self.peek().is(Eq) {
                self.revert(1);
                return Ok(expr);
            }

            expr = Expr::new(
                ExprKind::Binary {
                    lhs: Box::new(expr),
                    op: BinaryOp::BitwiseXor,
                    rhs: Box::new(self.parse_bitwise_and()?),
                },
                Span::new(
                    start_span.range.start..self.previous().span.range.end,
                    start_span.file_id,
                ),
            );
        }

        Ok(expr)
    }

    pub(crate) fn parse_bitwise_and(&mut self) -> DiagnosticResult<Expr> {
        let mut expr = self.parse_bitshift()?;

        let start_span = expr.span.clone();

        while self.match_one(Amp) {
            if self.peek().is(Eq) {
                self.revert(1);
                return Ok(expr);
            }

            expr = Expr::new(
                ExprKind::Binary {
                    lhs: Box::new(expr),
                    op: BinaryOp::BitwiseAnd,
                    rhs: Box::new(self.parse_bitshift()?),
                },
                Span::new(
                    start_span.range.start..self.previous().span.range.end,
                    start_span.file_id,
                ),
            );
        }

        Ok(expr)
    }

    pub(crate) fn parse_bitshift(&mut self) -> DiagnosticResult<Expr> {
        let mut expr = self.parse_term()?;

        let start_span = expr.span.clone();

        while self.match_any(&[LtLt, GtGt]) {
            if self.peek().is(Eq) {
                self.revert(1);
                return Ok(expr);
            }

            expr = Expr::new(
                ExprKind::Binary {
                    lhs: Box::new(expr),
                    op: self.previous().token_type.clone().into(),
                    rhs: Box::new(self.parse_term()?),
                },
                Span::new(
                    start_span.range.start..self.previous().span.range.end,
                    start_span.file_id,
                ),
            );
        }

        Ok(expr)
    }

    pub(crate) fn parse_term(&mut self) -> DiagnosticResult<Expr> {
        let mut expr = self.parse_factor()?;

        let start_span = expr.span.clone();

        while self.match_any(&[Minus, Plus]) {
            if self.peek().is(Eq) {
                self.revert(1);
                return Ok(expr);
            }

            expr = Expr::new(
                ExprKind::Binary {
                    lhs: Box::new(expr),
                    op: self.previous().token_type.clone().into(),
                    rhs: Box::new(self.parse_factor()?),
                },
                Span::new(
                    start_span.range.start..self.previous().span.range.end,
                    start_span.file_id,
                ),
            );
        }

        Ok(expr)
    }

    pub(crate) fn parse_factor(&mut self) -> DiagnosticResult<Expr> {
        let mut expr = self.parse_unary()?;

        let start_span = expr.span.clone();

        while self.match_any(&[Star, FwSlash, Percent]) {
            if self.peek().is(Eq) {
                self.revert(1);
                return Ok(expr);
            }

            expr = Expr::new(
                ExprKind::Binary {
                    lhs: Box::new(expr),
                    op: self.previous().token_type.clone().into(),
                    rhs: Box::new(self.parse_unary()?),
                },
                Span::new(
                    start_span.range.start..self.previous().span.range.end,
                    start_span.file_id,
                ),
            );
        }

        Ok(expr)
    }

    pub(crate) fn parse_unary(&mut self) -> DiagnosticResult<Expr> {
        if self.match_any(&[Amp, AmpAmp, Bang, Minus, Plus, Tilde]) {
            let span = self.previous().span.clone();
            let token = &self.previous().token_type;

            let expr = Expr::new(
                ExprKind::Unary {
                    op: match token {
                        Amp => UnaryOp::Ref(self.match_one(Mut)),
                        Star => UnaryOp::Deref,
                        Minus => UnaryOp::Neg,
                        Plus => UnaryOp::Plus,
                        Bang => UnaryOp::Not,
                        Tilde => UnaryOp::BitwiseNot,
                        t => panic!("{} is not a unary op", t),
                    },
                    lhs: Box::new(self.parse_unary()?),
                },
                Span::merge(&span, self.previous_span_ref()),
            );

            Ok(expr)
        } else {
            self.parse_atom()
        }
    }

    pub(crate) fn parse_atom(&mut self) -> DiagnosticResult<Expr> {
        let expr = if self.match_id() {
            let token = self.previous();
            let symbol = token.symbol();
            Expr::new(
                ExprKind::Id {
                    symbol,
                    is_mutable: false,
                    entity_span: Span::empty(),
                },
                token.span.clone(),
            )
        } else if self.match_one(OpenBracket) {
            self.parse_array_literal()?
        } else if self.match_one(Dot) {
            let start_span = self.previous().span.clone();
            if self.match_one(OpenParen) {
                self.parse_struct_literal(start_span)?
            } else {
                return Err(SyntaxError::expected(
                    self.span_ref(),
                    &format!("an expression, got `{}`", self.peek().lexeme),
                ));
            }
        } else if self.match_one(At) {
            return self.parse_builtin();
        } else if self.match_any(&[Break, Continue, Return]) {
            return self.parse_terminator();
        } else if self.match_any(&[
            Nil,
            True,
            False,
            Int(0),
            Float(0.0),
            Str(ustr("")),
            Char(' '),
        ]) {
            self.parse_literal()?
        } else if self.match_one(OpenParen) {
            let start_span = self.previous().span.clone();

            if self.match_one(CloseParen) {
                Expr::new(
                    ExprKind::Literal(LiteralKind::Unit),
                    Span::merge(&start_span, self.previous_span_ref()),
                )
            } else {
                let mut expr = self.parse_expr()?;

                let expr = if self.match_one(Comma) {
                    self.parse_tuple_expr(expr, start_span)?
                } else {
                    self.consume(CloseParen)?;

                    expr.span.range.start -= 1;
                    expr.span =
                        Span::merge(&expr.span, self.previous_span_ref());

                    expr
                };

                expr
            }
        } else if self.match_one(Fn) {
            self.parse_fn()?
        } else {
            return Err(SyntaxError::expected(
                self.span_ref(),
                &format!("an expression, got `{}`", self.peek().lexeme),
            ));
        };

        self.parse_postfix_expr(expr)
    }

    pub(crate) fn parse_builtin(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span.clone();
        let id_token = self.consume_id()?.clone();
        let symbol = id_token.symbol();

        self.consume(OpenParen)?;

        let builtin = match symbol.as_str() {
            "size_of" => Builtin::SizeOf(Box::new(self.parse_ty()?)),
            "align_of" => Builtin::AlignOf(Box::new(self.parse_ty()?)),
            "panic" => Builtin::Panic(if self.check_token(CloseParen) {
                None
            } else {
                Some(Box::new(self.parse_expr()?))
            }),
            name => {
                return Err(SyntaxError::unknown_builtin_function(
                    &Span::merge(&start_span, &id_token.span),
                    name.to_string(),
                ))
            }
        };

        self.consume(CloseParen)?;

        Ok(Expr::new(
            ExprKind::Builtin(builtin),
            Span::merge(&start_span, self.previous_span_ref()),
        ))
    }

    pub(crate) fn parse_while(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span.clone();
        let cond = self.parse_expr()?;

        self.consume(OpenCurly)?;
        let expr = self.parse_block()?;

        Ok(Expr::new(
            ExprKind::While {
                cond: Box::new(cond),
                expr: Box::new(expr),
            },
            Span::merge(&start_span, self.previous_span_ref()),
        ))
    }

    pub(crate) fn parse_for(&mut self) -> DiagnosticResult<Expr> {
        let default_iter = default_iter_name();
        let default_index = default_index_name();

        let mut declared_names = 0;

        let start_span = self.previous().span.clone();

        self.mark(0);

        // iterator and index declarations
        let (mut iter_name, iter_index_name) = if self.match_id() {
            declared_names = 1;

            let iter_name = self.previous().symbol();

            let iter_index_name = if self.match_one(Comma) {
                if self.match_id() {
                    declared_names = 2;
                    self.previous().symbol()
                } else {
                    default_index
                }
            } else {
                default_index
            };

            (iter_name, iter_index_name)
        } else {
            (default_iter, default_index)
        };

        // in declaration
        let mut has_reset_mark = false;
        if declared_names == 1 {
            if !self.match_one(In) {
                iter_name = default_iter;
                self.reset_to_mark();
                has_reset_mark = true;
            }
        } else if declared_names == 2 {
            self.consume(In)?;
        }

        if !has_reset_mark {
            self.pop_mark();
        }

        // actual expression
        let iter_start = self.parse_expr()?;

        let iterator = if self.match_one(DotDot) {
            let iter_end = self.parse_expr()?;
            ForIter::Range(Box::new(iter_start), Box::new(iter_end))
        } else {
            ForIter::Value(Box::new(iter_start))
        };

        self.consume(OpenCurly)?;
        let expr = self.parse_block()?;

        Ok(Expr::new(
            ExprKind::For {
                iter_name,
                iter_index_name,
                iterator,
                expr: Box::new(expr),
            },
            Span::merge(&start_span, self.previous_span_ref()),
        ))
    }

    pub(crate) fn parse_terminator(&mut self) -> DiagnosticResult<Expr> {
        let token = self.previous();
        let span = token.span.clone();

        let expr = Expr::new(
            match token.token_type {
                Break => ExprKind::Break { deferred: vec![] },
                Continue => ExprKind::Continue { deferred: vec![] },
                Return => {
                    let expr = if self.match_line_terminator() {
                        None
                    } else {
                        let expr = self.parse_expr()?;
                        self.consume_line_terminator()?;
                        Some(Box::new(expr))
                    };

                    ExprKind::Return {
                        expr,
                        deferred: vec![],
                    }
                }
                _ => {
                    return Err(Diagnostic::bug()
                        .with_message("got an invalid terminator"));
                }
            },
            Span::merge(&span, self.previous_span_ref()),
        );

        return Ok(expr);
    }

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

        while !self.match_one(CloseBracket) {
            let expr = self.parse_expr()?;

            if is_first_el {
                if self.match_one(Semicolon) {
                    let len = self.parse_expr()?;
                    self.consume(CloseBracket)?;

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

            if self.match_one(Comma) {
                continue;
            } else {
                self.consume(CloseBracket)?;
                break;
            }
        }

        Ok(Expr::new(
            ExprKind::ArrayLiteral(ArrayLiteralKind::List(elements)),
            Span::merge(&start_span, self.previous_span_ref()),
        ))
    }

    fn parse_tuple_expr(
        &mut self,
        first_expr: Expr,
        start_span: Span,
    ) -> DiagnosticResult<Expr> {
        let mut exprs = vec![first_expr];

        while !self.match_one(CloseParen) && !self.is_end() {
            let expr = self.parse_expr()?;
            exprs.push(expr);

            if self.match_one(Comma) {
                continue;
            } else if self.match_one(CloseParen) {
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

    fn parse_struct_literal(
        &mut self,
        start_span: Span,
    ) -> DiagnosticResult<Expr> {
        let mut fields: Vec<StructLiteralField> = vec![];

        while !self.match_one(CloseParen) && !self.is_end() {
            let id_token = if self.match_id() {
                self.previous().clone()
            } else {
                break;
            };

            let value = if self.match_one(Colon) {
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

            if self.match_one(Comma) {
                continue;
            } else if self.match_one(CloseParen) {
                break;
            } else {
                return Err(SyntaxError::expected(self.span_ref(), ", or )"));
            }
        }

        Ok(Expr::new(
            ExprKind::StructLiteral(fields),
            Span::merge(&start_span, self.previous_span_ref()),
        ))
    }
}
