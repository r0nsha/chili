use crate::*;
use chili_ast::ast::{self, BinaryOp, Cast, Expr, ExprKind, FnCall, UnaryOp};
use chili_error::*;
use chili_span::{EndPosition, To};
use chili_token::TokenKind::*;
use ustr::ustr;

impl<'p> Parser<'p> {
    pub(crate) fn parse_postfix_expr(&mut self, mut expr: Expr) -> DiagnosticResult<Expr> {
        // named struct literal
        if !self.restrictions.contains(Restrictions::NO_STRUCT_LITERAL) && eat!(self, OpenCurly) {
            let start_span = expr.span;
            return self.parse_struct_literal(Some(Box::new(expr)), start_span);
        }

        // compound operations (non-recursive)
        if eat!(
            self,
            PlusEq
                | MinusEq
                | StarEq
                | FwSlashEq
                | PercentEq
                | AmpEq
                | BarEq
                | CaretEq
                | LtLtEq
                | GtGtEq
                | AmpAmpEq
                | BarBarEq
        ) {
            return self.parse_compound_assign(expr);
        }

        // postfix expressions (recursive)
        loop {
            expr = if eat!(self, Eq) {
                self.parse_assign(expr)?
            } else if eat!(self, Dot) {
                self.parse_member_access(expr)?
            } else if eat!(self, OpenParen) {
                self.parse_call(expr)?
            } else if eat!(self, OpenBracket) {
                self.parse_subscript_or_slice(expr)?
            } else if eat!(self, As) {
                self.parse_as(expr)?
            } else if eat!(self, Fn) {
                let start_span = expr.span;

                let fn_arg = self.parse_fn()?;
                let span = start_span.to(self.previous_span());

                match &mut expr.kind {
                    ExprKind::FnCall(call) => {
                        // map(x) fn ...
                        call.args.push(fn_arg);
                        expr
                    }
                    _ => {
                        // map fn ...
                        Expr::new(
                            ExprKind::FnCall(FnCall {
                                callee: Box::new(expr),
                                args: vec![fn_arg],
                                span,
                            }),
                            span,
                        )
                    }
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_assign(&mut self, expr: Expr) -> DiagnosticResult<Expr> {
        let start_span = expr.span;

        let rvalue = self.parse_expr()?;
        let end_span = self.previous().span;

        Ok(Expr::new(
            ExprKind::Assign(ast::Assign {
                lvalue: Box::new(expr),
                rvalue: Box::new(rvalue),
            }),
            start_span.to(end_span),
        ))
    }

    fn parse_compound_assign(&mut self, lvalue: Expr) -> DiagnosticResult<Expr> {
        let op: BinaryOp = self.previous().kind.into();
        let rvalue = self.parse_expr()?;

        let lvalue_span = lvalue.span;
        let rvalue_span = rvalue.span;

        Ok(Expr::new(
            ExprKind::Assign(ast::Assign {
                lvalue: Box::new(lvalue.clone()),
                rvalue: Box::new(Expr::new(
                    ExprKind::Binary(ast::Binary {
                        lhs: Box::new(lvalue),
                        op,
                        rhs: Box::new(rvalue),
                        span: rvalue_span,
                    }),
                    rvalue_span,
                )),
            }),
            lvalue_span.to(rvalue_span),
        ))
    }

    fn parse_as(&mut self, expr: Expr) -> DiagnosticResult<Expr> {
        let start_span = expr.span;

        let ty_expr = if eat!(self, Placeholder) {
            None
        } else {
            let expr = self.parse_ty()?;
            Some(Box::new(expr))
        };

        Ok(Expr::new(
            ExprKind::Cast(Cast {
                expr: Box::new(expr),
                ty_expr,
                target_ty: Default::default(),
            }),
            start_span.to(self.previous_span()),
        ))
    }

    fn parse_member_access(&mut self, expr: Expr) -> DiagnosticResult<Expr> {
        let start_span = expr.span;

        let token = self.bump();

        let expr = match token.kind {
            Id(id) => Expr::new(
                ExprKind::MemberAccess(ast::MemberAccess {
                    expr: Box::new(expr),
                    member: id,
                }),
                start_span.to(token.span),
            ),

            Int(i) => Expr::new(
                ExprKind::MemberAccess(ast::MemberAccess {
                    expr: Box::new(expr),
                    member: ustr(&i.to_string()),
                }),
                start_span.to(token.span),
            ),

            Float(_) => {
                // this is for chained tuple access like `tuple.0.1`
                let components = token.lexeme.split('.').collect::<Vec<&str>>();

                let first_access = Expr::new(
                    ExprKind::MemberAccess(ast::MemberAccess {
                        expr: Box::new(expr),
                        member: ustr(components[0]),
                    }),
                    start_span.to(token.span.with_end(EndPosition {
                        index: token.span.end.index - components[0].len() + 1,
                    })),
                );

                let second_access = Expr::new(
                    ExprKind::MemberAccess(ast::MemberAccess {
                        expr: Box::new(first_access),
                        member: ustr(components[0]),
                    }),
                    start_span.to(token.span),
                );

                second_access
            }

            Star => {
                let span = start_span.to(token.span);
                Expr::new(
                    ExprKind::Unary(ast::Unary {
                        op: UnaryOp::Deref,
                        lhs: Box::new(expr),
                        span,
                    }),
                    span,
                )
            }

            OpenParen => self.parse_call(expr)?,

            _ => {
                return Err(SyntaxError::expected(
                    self.span(),
                    "an identifier, number or *",
                ))
            }
        };

        Ok(expr)
    }

    fn parse_call(&mut self, callee: Expr) -> DiagnosticResult<Expr> {
        let start_span = callee.span;
        let args = parse_delimited_list!(self, CloseParen, Comma, self.parse_expr()?, ", or )");
        let span = start_span.to(self.previous_span());
        Ok(Expr::new(
            ExprKind::FnCall(FnCall {
                callee: Box::new(callee),
                args,
                span,
            }),
            span,
        ))
    }

    fn parse_subscript_or_slice(&mut self, expr: Expr) -> DiagnosticResult<Expr> {
        let start_span = expr.span;

        match self.parse_expr() {
            Ok(index) => {
                if eat!(self, DotDot) {
                    let high = match self.parse_expr() {
                        Ok(high) => Some(Box::new(high)),
                        Err(_) => None,
                    };

                    expect!(self, CloseBracket, "]")?;

                    return Ok(Expr::new(
                        ExprKind::Slice(ast::Slice {
                            expr: Box::new(expr),
                            low: Some(Box::new(index)),
                            high,
                        }),
                        start_span.to(self.previous_span()),
                    ));
                }

                expect!(self, CloseBracket, "]")?;

                Ok(Expr::new(
                    ExprKind::Subscript(ast::Subscript {
                        expr: Box::new(expr),
                        index: Box::new(index),
                    }),
                    start_span.to(self.previous_span()),
                ))
            }
            Err(err) => {
                if eat!(self, DotDot) {
                    let high = match self.parse_expr() {
                        Ok(high) => Some(Box::new(high)),
                        Err(_) => None,
                    };

                    expect!(self, CloseBracket, "]")?;

                    Ok(Expr::new(
                        ExprKind::Slice(ast::Slice {
                            expr: Box::new(expr),
                            low: None,
                            high,
                        }),
                        start_span.to(self.previous_span()),
                    ))
                } else {
                    Err(err)
                }
            }
        }
    }
}
