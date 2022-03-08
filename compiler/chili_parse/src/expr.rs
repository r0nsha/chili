use crate::*;
use chili_ast::ast::{
    BinaryOp, BindingKind, Block, Builtin, Expr, ExprKind, ForIter,
    LiteralKind, UnaryOp, Visibility,
};
use chili_error::*;
use chili_span::{Span, To};
use chili_token::TokenKind::*;
use codespan_reporting::diagnostic::Diagnostic;
use common::builtin::{default_index_name, default_iter_name};
use ustr::ustr;

impl<'w> Parser<'w> {
    pub(crate) fn parse_expr(&mut self) -> DiagnosticResult<Expr> {
        self.with_res(Restrictions::empty(), |p| {
            p.parse_expr_internal(ustr(""))
        })
    }

    pub(crate) fn parse_expr_with_res(
        &mut self,
        restrictions: Restrictions,
    ) -> DiagnosticResult<Expr> {
        self.with_res(restrictions, |p| p.parse_expr_internal(ustr("")))
    }

    pub(crate) fn parse_decl_expr(
        &mut self,
        decl_name: Ustr,
    ) -> DiagnosticResult<Expr> {
        self.with_res(Restrictions::empty(), |p| {
            p.parse_expr_internal(decl_name)
        })
    }

    fn parse_expr_internal(
        &mut self,
        decl_name: Ustr,
    ) -> DiagnosticResult<Expr> {
        let is_stmt = self.restrictions.contains(Restrictions::STMT_EXPR);

        self.decl_name_frames.push(decl_name);

        let expr = if is_stmt {
            if eat!(self, Use) {
                let start_span = self.previous().span;
                let imports = self.parse_import(Visibility::Private)?;

                Ok(Expr::new(
                    ExprKind::Import(imports),
                    start_span.to(self.previous_span()),
                ))
            } else if eat!(self, Defer) {
                let span = self.span();
                let expr = self.parse_expr()?;
                Ok(Expr::new(ExprKind::Defer(Box::new(expr)), span))
            } else if eat!(self, Type) {
                let start_span = self.previous().span;

                let binding = self.parse_binding(
                    BindingKind::Type,
                    Visibility::Private,
                    false,
                )?;

                Ok(Expr::new(
                    ExprKind::Binding(Box::new(binding)),
                    start_span.to(self.previous_span()),
                ))
            } else if eat!(self, Let) {
                let start_span = self.previous().span;

                if eat!(self, Foreign) {
                    let binding =
                        self.parse_foreign_single(Visibility::Private)?;

                    Ok(Expr::new(
                        ExprKind::Foreign(vec![binding]),
                        start_span.to(self.previous_span()),
                    ))
                } else {
                    let binding = self.parse_binding(
                        BindingKind::Let,
                        Visibility::Private,
                        false,
                    )?;

                    Ok(Expr::new(
                        ExprKind::Binding(Box::new(binding)),
                        start_span.to(self.previous_span()),
                    ))
                }
            } else if eat!(self, Foreign) {
                let start_span = self.previous().span;
                let bindings = self.parse_foreign_block()?;

                Ok(Expr::new(
                    ExprKind::Foreign(bindings),
                    start_span.to(self.previous_span()),
                ))
            } else {
                self.parse_logic_or()
            }
        } else {
            self.parse_logic_or()
        }?;

        let expr = self.parse_postfix_expr(expr)?;

        self.decl_name_frames.pop();

        Ok(expr)
    }

    pub(crate) fn parse_if(&mut self) -> DiagnosticResult<Expr> {
        let token = self.previous();
        let span = token.span;

        let cond = self.parse_expr_with_res(Restrictions::NO_STRUCT_LITERAL)?;

        expect!(self, OpenCurly, "{")?;
        let then_expr = self.parse_block()?;

        let else_expr = if eat!(self, Else) {
            let expr = if eat!(self, If) {
                self.parse_if()?
            } else {
                expect!(self, OpenCurly, "{")?;
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
            span.to(self.previous_span()),
        ))
    }

    pub(crate) fn parse_block(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span;

        let exprs = parse_delimited_list!(
            self,
            CloseCurly,
            Semicolon,
            self.parse_expr_with_res(Restrictions::STMT_EXPR)?,
            "; or }"
        );

        Ok(Expr::new(
            ExprKind::Block(Block {
                exprs,
                deferred: vec![],
                yields: true,
            }),
            start_span.to(self.previous_span()),
        ))
    }

    pub(crate) fn parse_logic_or(&mut self) -> DiagnosticResult<Expr> {
        let mut expr = self.parse_logic_and()?;

        let start_span = expr.span;

        while eat!(self, BarBar) {
            expr = Expr::new(
                ExprKind::Binary {
                    lhs: Box::new(expr),
                    op: BinaryOp::Or,
                    rhs: Box::new(self.parse_logic_and()?),
                },
                start_span.to(self.previous_span()),
            );
        }

        Ok(expr)
    }

    pub(crate) fn parse_logic_and(&mut self) -> DiagnosticResult<Expr> {
        let mut expr = self.parse_comparison()?;

        let start_span = expr.span;

        while eat!(self, AmpAmp) {
            expr = Expr::new(
                ExprKind::Binary {
                    lhs: Box::new(expr),
                    op: BinaryOp::And,
                    rhs: Box::new(self.parse_comparison()?),
                },
                start_span.to(self.previous_span()),
            );
        }

        Ok(expr)
    }

    pub(crate) fn parse_comparison(&mut self) -> DiagnosticResult<Expr> {
        let mut expr = self.parse_bitwise_or()?;

        let start_span = expr.span;

        while eat!(self, BangEq | EqEq | Gt | GtEq | Lt | LtEq) {
            expr = Expr::new(
                ExprKind::Binary {
                    lhs: Box::new(expr),
                    op: self.previous().kind.into(),
                    rhs: Box::new(self.parse_bitwise_or()?),
                },
                start_span.to(self.previous_span()),
            );
        }

        Ok(expr)
    }

    pub(crate) fn parse_bitwise_or(&mut self) -> DiagnosticResult<Expr> {
        let mut expr = self.parse_bitwise_xor()?;

        let start_span = expr.span;

        while eat!(self, Bar) {
            expr = Expr::new(
                ExprKind::Binary {
                    lhs: Box::new(expr),
                    op: BinaryOp::BitwiseOr,
                    rhs: Box::new(self.parse_bitwise_xor()?),
                },
                start_span.to(self.previous_span()),
            );
        }

        Ok(expr)
    }

    pub(crate) fn parse_bitwise_xor(&mut self) -> DiagnosticResult<Expr> {
        let mut expr = self.parse_bitwise_and()?;

        let start_span = expr.span;

        while eat!(self, Caret) {
            expr = Expr::new(
                ExprKind::Binary {
                    lhs: Box::new(expr),
                    op: BinaryOp::BitwiseXor,
                    rhs: Box::new(self.parse_bitwise_and()?),
                },
                start_span.to(self.previous_span()),
            );
        }

        Ok(expr)
    }

    pub(crate) fn parse_bitwise_and(&mut self) -> DiagnosticResult<Expr> {
        let mut expr = self.parse_bitshift()?;

        let start_span = expr.span;

        while eat!(self, Amp) {
            expr = Expr::new(
                ExprKind::Binary {
                    lhs: Box::new(expr),
                    op: BinaryOp::BitwiseAnd,
                    rhs: Box::new(self.parse_bitshift()?),
                },
                start_span.to(self.previous_span()),
            );
        }

        Ok(expr)
    }

    pub(crate) fn parse_bitshift(&mut self) -> DiagnosticResult<Expr> {
        let mut expr = self.parse_term()?;

        let start_span = expr.span;

        while eat!(self, LtLt | GtGt) {
            expr = Expr::new(
                ExprKind::Binary {
                    lhs: Box::new(expr),
                    op: self.previous().kind.into(),
                    rhs: Box::new(self.parse_term()?),
                },
                start_span.to(self.previous_span()),
            );
        }

        Ok(expr)
    }

    pub(crate) fn parse_term(&mut self) -> DiagnosticResult<Expr> {
        let mut expr = self.parse_factor()?;

        let start_span = expr.span;

        while eat!(self, Minus | Plus) {
            expr = Expr::new(
                ExprKind::Binary {
                    lhs: Box::new(expr),
                    op: self.previous().kind.into(),
                    rhs: Box::new(self.parse_factor()?),
                },
                start_span.to(self.previous_span()),
            );
        }

        Ok(expr)
    }

    pub(crate) fn parse_factor(&mut self) -> DiagnosticResult<Expr> {
        let mut expr = self.parse_unary()?;

        let start_span = expr.span;

        while eat!(self, Star | FwSlash | Percent) {
            expr = Expr::new(
                ExprKind::Binary {
                    lhs: Box::new(expr),
                    op: self.previous().kind.into(),
                    rhs: Box::new(self.parse_unary()?),
                },
                start_span.to(self.previous_span()),
            );
        }

        Ok(expr)
    }

    pub(crate) fn parse_unary(&mut self) -> DiagnosticResult<Expr> {
        if eat!(self, Amp | AmpAmp | Bang | Minus | Plus | Tilde) {
            let span = self.previous().span;
            let token = &self.previous().kind;

            let expr = Expr::new(
                ExprKind::Unary {
                    op: match token {
                        Amp => UnaryOp::Ref(eat!(self, Mut)),
                        Star => UnaryOp::Deref,
                        Minus => UnaryOp::Neg,
                        Plus => UnaryOp::Plus,
                        Bang => UnaryOp::Not,
                        Tilde => UnaryOp::BitwiseNot,
                        t => panic!("{} is not a unary op", t),
                    },
                    lhs: Box::new(self.parse_unary()?),
                },
                span.to(self.previous_span()),
            );

            Ok(expr)
        } else {
            self.parse_primary()
        }
    }

    pub(crate) fn parse_primary(&mut self) -> DiagnosticResult<Expr> {
        let expr = if eat!(self, Id(_)) {
            let token = self.previous();
            let symbol = token.symbol();
            Expr::new(
                ExprKind::Id {
                    symbol,
                    is_mutable: false,
                    binding_span: Span::unknown(),
                    binding_info_id: Default::default(),
                },
                token.span,
            )
        } else if eat!(self, If) {
            self.parse_if()?
        } else if eat!(self, While) {
            self.parse_while()?
        } else if eat!(self, For) {
            self.parse_for()?
        } else if eat!(self, OpenCurly) {
            self.parse_block()?
        } else if eat!(self, OpenBracket) {
            self.parse_array_literal()?
        } else if eat!(self, Dot) {
            let start_span = self.previous().span;

            // anonymous struct literal
            if eat!(self, OpenCurly) {
                self.parse_struct_literal(None, start_span)?
            } else {
                return Err(SyntaxError::expected(
                    self.span(),
                    &format!("an expression, got `{}`", self.peek().lexeme),
                ));
            }
        } else if eat!(self, At) {
            return self.parse_builtin();
        } else if eat!(self, Break | Continue | Return) {
            return self.parse_terminator();
        } else if eat!(
            self,
            Nil | True | False | Int(_) | Float(_) | Str(_) | Char(_)
        ) {
            self.parse_literal()?
        } else if eat!(self, OpenParen) {
            let start_span = self.previous().span;

            if eat!(self, CloseParen) {
                Expr::new(
                    ExprKind::Literal(LiteralKind::Unit),
                    start_span.to(self.previous_span()),
                )
            } else {
                let mut expr = self.parse_expr()?;

                let expr = if eat!(self, Comma) {
                    self.parse_tuple_literal(expr, start_span)?
                } else {
                    expect!(self, CloseParen, ")")?;

                    expr.span.range().start -= 1;
                    expr.span = Span::to(&expr.span, self.previous_span());

                    expr
                };

                expr
            }
        } else if eat!(self, Fn) {
            self.parse_fn()?
        } else {
            return Err(SyntaxError::expected(
                self.span(),
                &format!("an expression, got `{}`", self.peek().lexeme),
            ));
        };

        self.parse_postfix_expr(expr)
    }

    pub(crate) fn parse_builtin(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span;
        let id_token = expect!(self, Id(_), "identifier")?.clone();
        let symbol = id_token.symbol();

        expect!(self, OpenParen, "(")?;

        let builtin = match symbol.as_str() {
            "size_of" => Builtin::SizeOf(Box::new(self.parse_ty()?)),
            "align_of" => Builtin::AlignOf(Box::new(self.parse_ty()?)),
            "panic" => Builtin::Panic(if token_is!(self, CloseParen) {
                None
            } else {
                Some(Box::new(self.parse_expr()?))
            }),
            name => {
                return Err(SyntaxError::unknown_builtin_function(
                    start_span.to(id_token.span),
                    name.to_string(),
                ))
            }
        };

        expect!(self, CloseParen, ")")?;

        Ok(Expr::new(
            ExprKind::Builtin(builtin),
            start_span.to(self.previous_span()),
        ))
    }

    pub(crate) fn parse_while(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span;

        let cond = self.parse_expr_with_res(Restrictions::NO_STRUCT_LITERAL)?;

        expect!(self, OpenCurly, "{")?;
        let expr = self.parse_block()?;

        Ok(Expr::new(
            ExprKind::While {
                cond: Box::new(cond),
                expr: Box::new(expr),
            },
            start_span.to(self.previous_span()),
        ))
    }

    pub(crate) fn parse_for(&mut self) -> DiagnosticResult<Expr> {
        let default_iter = default_iter_name();
        let default_index = default_index_name();

        let mut declared_names = 0;

        let start_span = self.previous().span;

        self.mark(0);

        // iterator and index declarations
        let (mut iter_name, iter_index_name) = if eat!(self, Id(_)) {
            declared_names = 1;

            let iter_name = self.previous().symbol();

            let iter_index_name = if eat!(self, Comma) {
                if eat!(self, Id(_)) {
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
            if !eat!(self, In) {
                iter_name = default_iter;
                self.reset_to_mark();
                has_reset_mark = true;
            }
        } else if declared_names == 2 {
            expect!(self, In, "in")?;
        }

        if !has_reset_mark {
            self.pop_mark();
        }

        // actual expression
        let iter_start =
            self.parse_expr_with_res(Restrictions::NO_STRUCT_LITERAL)?;

        let iterator = if eat!(self, DotDot) {
            let iter_end =
                self.parse_expr_with_res(Restrictions::NO_STRUCT_LITERAL)?;
            ForIter::Range(Box::new(iter_start), Box::new(iter_end))
        } else {
            ForIter::Value(Box::new(iter_start))
        };

        expect!(self, OpenCurly, "{")?;
        let expr = self.parse_block()?;

        Ok(Expr::new(
            ExprKind::For {
                iter_name,
                iter_index_name,
                iterator,
                expr: Box::new(expr),
            },
            start_span.to(self.previous_span()),
        ))
    }

    pub(crate) fn parse_terminator(&mut self) -> DiagnosticResult<Expr> {
        let token = self.previous();
        let span = token.span;

        let kind = match token.kind {
            Break => ExprKind::Break { deferred: vec![] },
            Continue => ExprKind::Continue { deferred: vec![] },
            Return => {
                let expr = if !self.peek().kind.is_expr_start()
                    && token_is!(self, Semicolon)
                {
                    None
                } else {
                    let expr = self.parse_expr()?;
                    Some(Box::new(expr))
                };

                ExprKind::Return {
                    expr,
                    deferred: vec![],
                }
            }
            _ => {
                return Err(
                    Diagnostic::bug().with_message("got an invalid terminator")
                );
            }
        };

        return Ok(Expr::new(kind, span.to(self.previous_span())));
    }
}
