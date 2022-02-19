use crate::*;
use chilic_ast::{
    entity::{EntityKind, Visibility},
    expr::{Block, Builtin, Expr, ExprKind, ForIter, LiteralKind},
    op::{BinaryOp, UnaryOp},
};
use chilic_error::*;
use chilic_span::Span;
use chilic_token::TokenType::*;
use codespan_reporting::diagnostic::Diagnostic;
use common::builtin::{default_index_name, default_iter_name};
use ustr::ustr;

impl Parser {
    pub(crate) fn parse_expr(&mut self) -> DiagnosticResult<Expr> {
        self.parse_expr_internal(ustr(""))
    }

    pub(crate) fn parse_expr_with_res(
        &mut self,
        restrictions: Restrictions,
    ) -> DiagnosticResult<Expr> {
        self.with_res(restrictions, |p| p.parse_expr())
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

        let expr = if match_token!(self, If) {
            self.parse_if()
        } else if match_token!(self, While) {
            self.parse_while()
        } else if match_token!(self, For) {
            self.parse_for()
        } else if match_token!(self, OpenCurly) {
            self.parse_block()
        } else if match_token!(self, Use) {
            todo!()
            // let uses = self.parse_use(Visibility::Private)?;

            // let stmts: Vec<Stmt> = uses
            //     .into_iter()
            //     .map(|use_| {
            //         let span = use_.span.clone();
            //         Stmt::new(StmtKind::Use(use_), span)
            //     })
            //     .collect();

            // require!(self, Semicolon, ";")?;

            // return Ok(stmts);
        } else if match_token!(self, Defer) {
            let span = self.span();
            let expr = self.parse_expr()?;
            Ok(Expr::new(ExprKind::Defer(Box::new(expr)), span))
        } else if match_token!(self, Type) {
            let start_span = self.previous().span.clone();

            let entity = self.parse_entity(
                EntityKind::Type,
                Visibility::Private,
                false,
            )?;

            Ok(Expr::new(
                ExprKind::Entity(Box::new(entity)),
                Span::merge(&start_span, self.previous_span_ref()),
            ))
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

            Ok(Expr::new(
                ExprKind::Entity(Box::new(entity)),
                Span::merge(&start_span, self.previous_span_ref()),
            ))
        } else if match_token!(self, Foreign) {
            todo!()
            // let start_span = self.previous().span.clone();
            // let entities = self.parse_foreign_block()?;

            // let stmts = entities
            //     .iter()
            //     .map(|entity| {
            //         Stmt::new(
            //             StmtKind::Entity(entity.clone()),
            //             Span::merge(&start_span, self.previous_span_ref()),
            //         )
            //     })
            //     .collect();

            // return Ok(stmts);
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

        let cond = self.parse_expr_with_res(Restrictions::NO_STRUCT_LITERAL)?;

        require!(self, OpenCurly, "{")?;
        let then_expr = self.parse_block()?;

        let else_expr = if match_token!(self, Else) {
            let expr = if match_token!(self, If) {
                self.parse_if()?
            } else {
                require!(self, OpenCurly, "{")?;
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

        let mut yields = false;
        let mut exprs: Vec<Expr> = vec![];

        while !match_token!(self, CloseCurly) && !self.is_end() {
            let expr = self.parse_expr()?;
            exprs.push(expr);

            if match_token!(self, Semicolon) {
                continue;
            } else if match_token!(self, CloseCurly) {
                if !last_is!(self, Semicolon) {
                    yields = true;
                }

                break;
            } else {
                if last_is!(self, CloseCurly) {
                    continue;
                }

                return Err(SyntaxError::expected(
                    self.previous_span_ref(),
                    "; or }",
                ));
            }
        }

        Ok(Expr::new(
            ExprKind::Block(Block {
                exprs,
                deferred: vec![],
                yields,
            }),
            Span::merge(&start_span, self.previous_span_ref()),
        ))
    }

    pub(crate) fn parse_logic_or(&mut self) -> DiagnosticResult<Expr> {
        let mut expr = self.parse_logic_and()?;

        let start_span = expr.span.clone();

        while match_token!(self, BarBar) {
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

        while match_token!(self, AmpAmp) {
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

        while match_token!(self, BangEq | EqEq | Gt | GtEq | Lt | LtEq) {
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

        while match_token!(self, Bar) {
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

        while match_token!(self, Caret) {
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

        while match_token!(self, Amp) {
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

        while match_token!(self, LtLt | GtGt) {
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

        while match_token!(self, Minus | Plus) {
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

        while match_token!(self, Star | FwSlash | Percent) {
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
        if match_token!(self, Amp | AmpAmp | Bang | Minus | Plus | Tilde) {
            let span = self.previous().span.clone();
            let token = &self.previous().token_type;

            let expr = Expr::new(
                ExprKind::Unary {
                    op: match token {
                        Amp => UnaryOp::Ref(match_token!(self, Mut)),
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
        let expr = if match_token!(self, Id(_)) {
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
        } else if match_token!(self, OpenBracket) {
            self.parse_array_literal()?
        } else if match_token!(self, Dot) {
            let start_span = self.previous().span.clone();

            // anonymous struct literal
            if !self.is_res(Restrictions::NO_STRUCT_LITERAL)
                && match_token!(self, OpenCurly)
            {
                self.parse_struct_literal(None, start_span)?
            } else {
                return Err(SyntaxError::expected(
                    self.span_ref(),
                    &format!("an expression, got `{}`", self.peek().lexeme),
                ));
            }
        } else if match_token!(self, At) {
            return self.parse_builtin();
        } else if match_token!(self, Break | Continue | Return) {
            return self.parse_terminator();
        } else if match_token!(
            self,
            Nil | True | False | Int(_) | Float(_) | Str(_) | Char(_)
        ) {
            self.parse_literal()?
        } else if match_token!(self, OpenParen) {
            let start_span = self.previous().span.clone();

            if match_token!(self, CloseParen) {
                Expr::new(
                    ExprKind::Literal(LiteralKind::Unit),
                    Span::merge(&start_span, self.previous_span_ref()),
                )
            } else {
                let mut expr = self.parse_expr()?;

                let expr = if match_token!(self, Comma) {
                    self.parse_tuple_literal(expr, start_span)?
                } else {
                    require!(self, CloseParen, ")")?;

                    expr.span.range.start -= 1;
                    expr.span =
                        Span::merge(&expr.span, self.previous_span_ref());

                    expr
                };

                expr
            }
        } else if match_token!(self, Fn) {
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
        let id_token = require!(self, Id(_), "identifier")?.clone();
        let symbol = id_token.symbol();

        require!(self, OpenParen, "(")?;

        let builtin = match symbol.as_str() {
            "size_of" => Builtin::SizeOf(Box::new(self.parse_ty()?)),
            "align_of" => Builtin::AlignOf(Box::new(self.parse_ty()?)),
            "panic" => Builtin::Panic(if is!(self, CloseParen) {
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

        require!(self, CloseParen, ")")?;

        Ok(Expr::new(
            ExprKind::Builtin(builtin),
            Span::merge(&start_span, self.previous_span_ref()),
        ))
    }

    pub(crate) fn parse_while(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span.clone();

        let cond = self.parse_expr_with_res(Restrictions::NO_STRUCT_LITERAL)?;

        require!(self, OpenCurly, "{")?;
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
        let (mut iter_name, iter_index_name) = if match_token!(self, Id(_)) {
            declared_names = 1;

            let iter_name = self.previous().symbol();

            let iter_index_name = if match_token!(self, Comma) {
                if match_token!(self, Id(_)) {
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
            if !match_token!(self, In) {
                iter_name = default_iter;
                self.reset_to_mark();
                has_reset_mark = true;
            }
        } else if declared_names == 2 {
            require!(self, In, "in")?;
        }

        if !has_reset_mark {
            self.pop_mark();
        }

        // actual expression
        let iter_start =
            self.parse_expr_with_res(Restrictions::NO_STRUCT_LITERAL)?;

        let iterator = if match_token!(self, DotDot) {
            let iter_end =
                self.parse_expr_with_res(Restrictions::NO_STRUCT_LITERAL)?;
            ForIter::Range(Box::new(iter_start), Box::new(iter_end))
        } else {
            ForIter::Value(Box::new(iter_start))
        };

        require!(self, OpenCurly, "{")?;
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
                    let expr = if match_token!(self, Semicolon) {
                        None
                    } else {
                        let expr = self.parse_expr()?;
                        require!(self, Semicolon, ";")?;
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
}
