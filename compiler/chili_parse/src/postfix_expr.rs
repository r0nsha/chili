use crate::*;
use chili_ast::ast::{BinaryOp, Call, CallArg, Cast, Expr, ExprKind, UnaryOp};
use chili_error::*;
use chili_span::{EndPosition, Spanned, To};
use chili_token::TokenKind::*;
use chili_ty::Ty;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::ustr;

impl Parser {
    pub(crate) fn parse_postfix_expr(
        &mut self,
        mut expr: Expr,
    ) -> DiagnosticResult<Expr> {
        // named struct literal
        if !self.restrictions.contains(Restrictions::NO_STRUCT_LITERAL)
            && eat!(self, OpenCurly)
        {
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
                self.parse_field_access(expr)?
            } else if eat!(self, OpenParen) {
                self.parse_call(expr)?
            } else if eat!(self, OpenBracket) {
                self.parse_subscript_or_slice(expr)?
            } else if eat!(self, As) {
                self.parse_as(expr)?
            } else if eat!(self, Fn) {
                let start_span = expr.span;

                let fn_expr = self.parse_fn()?;
                let fn_arg = CallArg {
                    symbol: None,
                    value: fn_expr,
                };

                let span = start_span.to(self.previous_span());

                match &expr.kind {
                    ExprKind::Call(call) => {
                        // map(x) fn ...
                        let mut call = call.clone();
                        call.args.push(fn_arg);
                        Expr::new(ExprKind::Call(call), span)
                    }
                    _ => {
                        // map fn ...
                        Expr::new(
                            ExprKind::Call(Call {
                                callee: Box::new(expr),
                                args: vec![fn_arg],
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
            ExprKind::Assign {
                lvalue: Box::new(expr.clone()),
                rvalue: Box::new(rvalue),
            },
            start_span.to(end_span),
        ))
    }

    fn parse_compound_assign(
        &mut self,
        lvalue: Expr,
    ) -> DiagnosticResult<Expr> {
        let op: BinaryOp = self.previous().kind.into();
        let rvalue = self.parse_expr()?;

        let lvalue_span = lvalue.span;
        let rvalue_span = rvalue.span;

        Ok(Expr::new(
            ExprKind::Assign {
                lvalue: Box::new(lvalue.clone()),
                rvalue: Box::new(Expr::new(
                    ExprKind::Binary {
                        lhs: Box::new(lvalue),
                        op,
                        rhs: Box::new(rvalue),
                    },
                    rvalue_span,
                )),
            },
            lvalue_span.to(rvalue_span),
        ))
    }

    fn parse_as(&mut self, expr: Expr) -> DiagnosticResult<Expr> {
        let start_span = expr.span;

        let type_expr = if eat!(self, Placeholder) {
            None
        } else {
            let expr = self.parse_ty()?;
            Some(Box::new(expr))
        };

        Ok(Expr::new(
            ExprKind::Cast(Cast {
                expr: Box::new(expr.clone()),
                type_expr,
                target_ty: Ty::Unknown,
            }),
            start_span.to(self.previous_span()),
        ))
    }

    fn parse_field_access(&mut self, expr: Expr) -> DiagnosticResult<Expr> {
        let start_span = expr.span;

        let token = self.bump();

        let expr = match token.kind {
            Id(id) => Expr::new(
                ExprKind::MemberAccess {
                    expr: Box::new(expr.clone()),
                    member: id,
                },
                start_span.to(token.span),
            ),

            Int(i) => Expr::new(
                ExprKind::MemberAccess {
                    expr: Box::new(expr.clone()),
                    member: ustr(&i.to_string()),
                },
                start_span.to(token.span),
            ),

            Float(_) => {
                // this is for chained tuple access like `tuple.0.1`
                let components = token.lexeme.split('.').collect::<Vec<&str>>();

                let first_access = Expr::new(
                    ExprKind::MemberAccess {
                        expr: Box::new(expr.clone()),
                        member: ustr(components[0]),
                    },
                    start_span.to(token.span.with_end(EndPosition {
                        index: token.span.end.index - components[0].len() + 1,
                    })),
                );

                let second_access = Expr::new(
                    ExprKind::MemberAccess {
                        expr: Box::new(first_access),
                        member: ustr(components[0]),
                    },
                    start_span.to(token.span),
                );

                second_access
            }

            Star => Expr::new(
                ExprKind::Unary {
                    op: UnaryOp::Deref,
                    lhs: Box::new(expr.clone()),
                },
                start_span.to(token.span),
            ),

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
        let mut used_named_argument = false;

        let args = parse_delimited_list!(
            self,
            CloseParen,
            Comma,
            {
                let symbol = if eat!(self, Id(_)) {
                    let id_token = self.previous().clone();
                    if eat!(self, Colon) {
                        Some(Spanned::new(id_token.symbol(), id_token.span))
                    } else {
                        self.revert(1);
                        None
                    }
                } else {
                    None
                };

                if symbol.is_some() {
                    used_named_argument = true;
                } else if used_named_argument {
                    let span = self.span();
                    return Err(Diagnostic::error()
                        .with_message(
                            "can't use positional arguments after named arguments",
                        )
                        .with_labels(vec![Label::primary(
                            span.file_id,
                            span.range(),
                        )]));
                }

                let value = self.parse_expr()?;

                CallArg { symbol, value }
            },
            ", or )"
        );

        Ok(Expr::new(
            ExprKind::Call(Call {
                callee: Box::new(callee),
                args,
            }),
            start_span.to(self.previous_span()),
        ))
    }

    fn parse_subscript_or_slice(
        &mut self,
        expr: Expr,
    ) -> DiagnosticResult<Expr> {
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
                        ExprKind::Slice {
                            expr: Box::new(expr),
                            low: Some(Box::new(index)),
                            high,
                        },
                        start_span.to(self.previous_span()),
                    ));
                }

                expect!(self, CloseBracket, "]")?;

                Ok(Expr::new(
                    ExprKind::Subscript {
                        expr: Box::new(expr),
                        index: Box::new(index),
                    },
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
                        ExprKind::Slice {
                            expr: Box::new(expr),
                            low: None,
                            high,
                        },
                        start_span.to(self.previous_span()),
                    ))
                } else {
                    Err(err)
                }
            }
        }
    }
}
