use crate::*;
use chili_ast::ast::{
    self, BinaryOp, BindingKind, Block, Builtin, Expr, ExprKind, ForIter, Literal, UnaryOp,
    Visibility,
};
use chili_error::*;
use chili_span::{Span, To};
use chili_token::TokenKind::*;
use common::builtin::{default_index_name, default_iter_name};
use ustr::ustr;

macro_rules! parse_binary {
    ($parser:expr, pattern = $(|) ? $($pattern : pat_param) | +, next_fn = $next:expr) => {{
        let mut expr = $next($parser)?;
        let start_span = expr.span;

        while eat!($parser, $( $pattern )|+) {
            let op: BinaryOp = $parser.previous().kind.into();
            let rhs = $next($parser)?;
            let span = start_span.to($parser.previous_span());

            expr = Expr::new(
                ExprKind::Binary(ast::Binary {
                    lhs: Box::new(expr),
                    op,
                    rhs: Box::new(rhs),
                    span,
                }),
                span,
            );
        }

        Ok(expr)
    }};
}

impl<'p> Parser<'p> {
    pub(crate) fn parse_expr(&mut self) -> DiagnosticResult<Expr> {
        self.with_res(Restrictions::empty(), |p| p.parse_expr_inner(ustr("")))
    }

    pub(crate) fn parse_expr_with_res(
        &mut self,
        restrictions: Restrictions,
    ) -> DiagnosticResult<Expr> {
        self.with_res(restrictions, |parser| {
            let start_span = parser.span();
            let is_stmt = parser.restrictions.contains(Restrictions::STMT_EXPR);
            let result = parser.parse_expr_inner(ustr(""));
            if is_stmt {
                let end_span = parser.previous_span();
                let expr = result
                    .or_recover(parser)
                    .unwrap_or(Expr::new(ExprKind::Error, start_span.to(end_span)));
                Ok(expr)
            } else {
                result
            }
        })
    }

    pub(crate) fn parse_decl_expr(&mut self, decl_name: Ustr) -> DiagnosticResult<Expr> {
        self.with_res(Restrictions::empty(), |p| p.parse_expr_inner(decl_name))
    }

    fn parse_expr_inner(&mut self, decl_name: Ustr) -> DiagnosticResult<Expr> {
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

                let binding = self.parse_binding(BindingKind::Type, Visibility::Private, false)?;

                Ok(Expr::new(
                    ExprKind::Binding(Box::new(binding)),
                    start_span.to(self.previous_span()),
                ))
            } else if eat!(self, Let) {
                let start_span = self.previous().span;

                if eat!(self, Foreign) {
                    let binding = self.parse_foreign_single(Visibility::Private)?;

                    Ok(Expr::new(
                        ExprKind::Foreign(vec![binding]),
                        start_span.to(self.previous_span()),
                    ))
                } else {
                    let binding =
                        self.parse_binding(BindingKind::Value, Visibility::Private, false)?;

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
        let then = self.parse_block_expr()?;

        let otherwise = if eat!(self, Else) {
            let expr = if eat!(self, If) {
                self.parse_if()?
            } else {
                expect!(self, OpenCurly, "{")?;
                self.parse_block_expr()?
            };

            Some(Box::new(expr))
        } else {
            None
        };

        Ok(Expr::new(
            ExprKind::If(ast::If {
                cond: Box::new(cond),
                then: Box::new(then),
                otherwise,
            }),
            span.to(self.previous_span()),
        ))
    }

    pub(crate) fn parse_block(&mut self) -> DiagnosticResult<Block> {
        let start_span = self.previous().span;

        let exprs = parse_delimited_list!(
            self,
            CloseCurly,
            Semicolon,
            self.parse_expr_with_res(Restrictions::STMT_EXPR)?,
            "; or }"
        );

        let span = start_span.to(self.previous_span());
        Ok(Block {
            exprs,
            deferred: vec![],
            yields: true,
            span,
        })
    }

    pub(crate) fn parse_block_expr(&mut self) -> DiagnosticResult<Expr> {
        let block = self.parse_block()?;
        let span = block.span;
        Ok(Expr::new(ExprKind::Block(block), span))
    }

    pub(crate) fn parse_logic_or(&mut self) -> DiagnosticResult<Expr> {
        parse_binary!(self, pattern = BarBar, next_fn = Parser::parse_logic_and)
    }

    pub(crate) fn parse_logic_and(&mut self) -> DiagnosticResult<Expr> {
        parse_binary!(self, pattern = AmpAmp, next_fn = Parser::parse_comparison)
    }

    pub(crate) fn parse_comparison(&mut self) -> DiagnosticResult<Expr> {
        parse_binary!(
            self,
            pattern = BangEq | EqEq | Gt | GtEq | Lt | LtEq,
            next_fn = Parser::parse_bitwise_or
        )
    }

    pub(crate) fn parse_bitwise_or(&mut self) -> DiagnosticResult<Expr> {
        parse_binary!(self, pattern = Bar, next_fn = Parser::parse_bitwise_xor)
    }

    pub(crate) fn parse_bitwise_xor(&mut self) -> DiagnosticResult<Expr> {
        parse_binary!(self, pattern = Caret, next_fn = Parser::parse_bitwise_and)
    }

    pub(crate) fn parse_bitwise_and(&mut self) -> DiagnosticResult<Expr> {
        parse_binary!(self, pattern = Amp, next_fn = Parser::parse_bitshift)
    }

    pub(crate) fn parse_bitshift(&mut self) -> DiagnosticResult<Expr> {
        parse_binary!(self, pattern = LtLt | GtGt, next_fn = Parser::parse_term)
    }

    pub(crate) fn parse_term(&mut self) -> DiagnosticResult<Expr> {
        parse_binary!(self, pattern = Minus | Plus, next_fn = Parser::parse_factor)
    }

    pub(crate) fn parse_factor(&mut self) -> DiagnosticResult<Expr> {
        parse_binary!(
            self,
            pattern = Star | FwSlash | Percent,
            next_fn = Parser::parse_unary
        )
    }

    pub(crate) fn parse_unary(&mut self) -> DiagnosticResult<Expr> {
        if eat!(self, Amp | AmpAmp | Bang | Minus | Plus | Tilde) {
            let start_span = self.previous().span;
            let token = self.previous().kind;

            let op = match token {
                Amp => UnaryOp::Ref(eat!(self, Mut)),
                Star => UnaryOp::Deref,
                Minus => UnaryOp::Neg,
                Plus => UnaryOp::Plus,
                Bang => UnaryOp::Not,
                Tilde => UnaryOp::BitwiseNot,
                t => panic!("{} is not a unary op", t),
            };

            let lhs = self.parse_unary()?;

            let span = start_span.to(self.previous_span());

            let expr = Expr::new(
                ExprKind::Unary(ast::Unary {
                    op,
                    lhs: Box::new(lhs),
                    span,
                }),
                span,
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
                ExprKind::Ident(ast::Ident {
                    symbol,
                    binding_info_id: Default::default(),
                }),
                token.span,
            )
        } else if eat!(self, If) {
            self.parse_if()?
        } else if eat!(self, While) {
            self.parse_while()?
        } else if eat!(self, For) {
            self.parse_for()?
        } else if eat!(self, OpenCurly) {
            self.parse_block_expr()?
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
                    ExprKind::Literal(Literal::Unit),
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
        let id_token = expect!(self, Id(_), "identifier")?;
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
        let block = self.parse_block()?;

        Ok(Expr::new(
            ExprKind::While(ast::While {
                cond: Box::new(cond),
                block,
            }),
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
        let iter_start = self.parse_expr_with_res(Restrictions::NO_STRUCT_LITERAL)?;

        let iterator = if eat!(self, DotDot) {
            let iter_end = self.parse_expr_with_res(Restrictions::NO_STRUCT_LITERAL)?;
            ForIter::Range(Box::new(iter_start), Box::new(iter_end))
        } else {
            ForIter::Value(Box::new(iter_start))
        };

        expect!(self, OpenCurly, "{")?;
        let block = self.parse_block_expr()?;

        Ok(Expr::new(
            ExprKind::For(ast::For {
                iter_name,
                iter_id: Default::default(),
                iter_index_name,
                iter_index_id: Default::default(),
                iterator,
                block: Box::new(block),
            }),
            start_span.to(self.previous_span()),
        ))
    }

    pub(crate) fn parse_terminator(&mut self) -> DiagnosticResult<Expr> {
        let token = self.previous();
        let span = token.span;

        let kind = match token.kind {
            Break => ExprKind::Break(ast::Deferred { deferred: vec![] }),
            Continue => ExprKind::Continue(ast::Deferred { deferred: vec![] }),
            Return => {
                let expr = if !self.peek().kind.is_expr_start() && token_is!(self, Semicolon) {
                    None
                } else {
                    let expr = self.parse_expr()?;
                    Some(Box::new(expr))
                };

                ExprKind::Return(ast::Return {
                    expr,
                    deferred: vec![],
                })
            }
            _ => panic!("got an invalid terminator"),
        };

        return Ok(Expr::new(kind, span.to(self.previous_span())));
    }
}
