use super::*;
use crate::ast::{self, Ast, BinaryOp, Call, Cast, UnaryOp};
use crate::error::*;
use crate::span::{EndPosition, To};
use crate::token::TokenKind::*;
use ustr::ustr;

impl Parser {
    pub fn parse_postfix_expr(&mut self, mut expr: Ast) -> DiagnosticResult<Ast> {
        // named struct literal
        if !self.restrictions.contains(Restrictions::NO_STRUCT_LITERAL) && eat!(self, OpenCurly) {
            let start_span = expr.span();
            return self.parse_struct_literal(Some(Box::new(expr)), start_span);
        }

        // compound operations (non-recursive)
        if self.restrictions.contains(Restrictions::STMT_EXPR) {
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
            } else if eat!(self, Eq) {
                return self.parse_assign(expr);
            }
        }

        // postfix expressions (recursive)
        loop {
            expr = if eat!(self, Dot) {
                self.parse_member_access(expr)?
            } else if eat!(self, OpenParen) {
                self.parse_call(expr)?
            } else if eat!(self, OpenBracket) {
                self.parse_subscript_or_slice(expr)?
            } else if eat!(self, As) {
                self.parse_as(expr)?
            } else if eat!(self, Fn) {
                let start_span = expr.span();

                let fn_arg = self.parse_fn()?;
                let span = start_span.to(self.previous_span());

                match &mut expr {
                    Ast::Call(call) => {
                        // map(x) fn ...
                        call.args.push(fn_arg);
                        expr
                    }
                    _ => {
                        // map fn ...
                        Ast::Call(Call {
                            callee: Box::new(expr),
                            args: vec![fn_arg],
                            ty: Default::default(),
                            span,
                        })
                    }
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_assign(&mut self, expr: Ast) -> DiagnosticResult<Ast> {
        let start_span = expr.span();

        let rvalue = self.parse_expr()?;
        let end_span = self.previous_span();

        Ok(Ast::Assignment(ast::Assignment {
            lvalue: Box::new(expr),
            rvalue: Box::new(rvalue),
            ty: Default::default(),
            span: start_span.to(end_span),
        }))
    }

    fn parse_compound_assign(&mut self, lvalue: Ast) -> DiagnosticResult<Ast> {
        let op: BinaryOp = self.previous().kind.into();
        let rvalue = self.parse_expr()?;

        let lvalue_span = lvalue.span();
        let rvalue_span = rvalue.span();

        Ok(Ast::Assignment(ast::Assignment {
            lvalue: Box::new(lvalue.clone()),
            rvalue: Box::new(Ast::Binary(ast::Binary {
                lhs: Box::new(lvalue),
                op,
                rhs: Box::new(rvalue),
                ty: Default::default(),
                span: rvalue_span,
            })),
            ty: Default::default(),
            span: lvalue_span.to(rvalue_span),
        }))
    }

    fn parse_as(&mut self, expr: Ast) -> DiagnosticResult<Ast> {
        let start_span = expr.span();

        let ty_expr = if eat!(self, Placeholder) {
            None
        } else {
            Some(Box::new(self.parse_expr()?))
        };

        Ok(Ast::Cast(Cast {
            expr: Box::new(expr),
            target: ty_expr,
            ty: Default::default(),
            span: start_span.to(self.previous_span()),
        }))
    }

    fn parse_member_access(&mut self, expr: Ast) -> DiagnosticResult<Ast> {
        let start_span = expr.span();

        let token = self.bump();

        let expr = match token.kind {
            Ident(id) => Ast::MemberAccess(ast::MemberAccess {
                expr: Box::new(expr),
                member: id,
                ty: Default::default(),
                span: start_span.to(token.span),
            }),

            Int(i) => Ast::MemberAccess(ast::MemberAccess {
                expr: Box::new(expr),
                member: ustr(&i.to_string()),
                ty: Default::default(),
                span: start_span.to(token.span),
            }),

            Float(_) => {
                // this is for chained tuple access like `tuple.0.1`
                let components = token.lexeme.split('.').collect::<Vec<&str>>();

                let first_access = Ast::MemberAccess(ast::MemberAccess {
                    expr: Box::new(expr),
                    member: ustr(components[0]),
                    ty: Default::default(),
                    span: start_span.to(token.span.with_end(EndPosition {
                        index: token.span.end.index - components[0].len() + 1,
                    })),
                });

                Ast::MemberAccess(ast::MemberAccess {
                    expr: Box::new(first_access),
                    member: ustr(components[0]),
                    ty: Default::default(),
                    span: start_span.to(token.span),
                })
            }

            Star => Ast::Unary(ast::Unary {
                op: UnaryOp::Deref,
                value: Box::new(expr),
                ty: Default::default(),
                span: start_span.to(token.span),
            }),

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

    fn parse_call(&mut self, callee: Ast) -> DiagnosticResult<Ast> {
        let start_span = callee.span();
        let args = parse_delimited_list!(self, CloseParen, Comma, self.parse_expr()?, ", or )");

        Ok(Ast::Call(Call {
            callee: Box::new(callee),
            args,
            ty: Default::default(),
            span: start_span.to(self.previous_span()),
        }))
    }

    fn parse_subscript_or_slice(&mut self, expr: Ast) -> DiagnosticResult<Ast> {
        let start_span = expr.span();

        match self.parse_expr() {
            Ok(index) => {
                if eat!(self, DotDot) {
                    let high = match self.parse_expr() {
                        Ok(high) => Some(Box::new(high)),
                        Err(_) => None,
                    };

                    require!(self, CloseBracket, "]")?;

                    return Ok(Ast::Slice(ast::Slice {
                        expr: Box::new(expr),
                        low: Some(Box::new(index)),
                        high,
                        ty: Default::default(),
                        span: start_span.to(self.previous_span()),
                    }));
                }

                require!(self, CloseBracket, "]")?;

                Ok(Ast::Subscript(ast::Subscript {
                    expr: Box::new(expr),
                    index: Box::new(index),
                    ty: Default::default(),
                    span: start_span.to(self.previous_span()),
                }))
            }
            Err(err) => {
                if eat!(self, DotDot) {
                    let high = match self.parse_expr() {
                        Ok(high) => Some(Box::new(high)),
                        Err(_) => None,
                    };

                    require!(self, CloseBracket, "]")?;

                    Ok(Ast::Slice(ast::Slice {
                        expr: Box::new(expr),
                        low: None,
                        high,
                        ty: Default::default(),
                        span: start_span.to(self.previous_span()),
                    }))
                } else {
                    Err(err)
                }
            }
        }
    }
}
