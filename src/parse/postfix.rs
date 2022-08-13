use super::*;
use crate::{
    ast::{self, Ast, Call, Cast, UnaryOp},
    error::*,
    span::EndPosition,
    token::TokenKind::*,
};
use ustr::ustr;

impl Parser {
    pub fn parse_operand_postfix_operator(&mut self, mut expr: Ast) -> DiagnosticResult<Ast> {
        // named struct literal
        if !self.restrictions.contains(Restrictions::NO_STRUCT_LITERAL) && is!(self, OpenCurly) {
            return self.parse_struct_literal(Some(Box::new(expr)));
        }

        // postfix expressions (recursive)
        loop {
            expr = if eat!(self, Dot) {
                self.parse_member_access(expr)?
            } else if eat!(self, OpenParen) {
                self.parse_call(expr)?
            } else if eat!(self, OpenBracket) {
                self.parse_subscript_or_slice(expr)?
            } else if !self.restrictions.contains(Restrictions::NO_CAST) && eat!(self, As) {
                self.parse_cast(expr)?
            } else if eat!(self, Fn) {
                let start_span = expr.span();

                let fn_arg = self.parse_function(None, false)?;
                let span = start_span.to(self.previous_span());

                match &mut expr {
                    Ast::Call(call) => {
                        // map(x) fn ...
                        call.args.push(ast::CallArg {
                            value: fn_arg,
                            spread: false,
                        });

                        expr
                    }
                    _ => {
                        // map fn ...
                        Ast::Call(Call {
                            callee: Box::new(expr),
                            args: vec![ast::CallArg {
                                value: fn_arg,
                                spread: false,
                            }],
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

    fn parse_cast(&mut self, expr: Ast) -> DiagnosticResult<Ast> {
        let start_span = expr.span();

        let target_type = self.parse_expression_res(Restrictions::NO_CAST, false, true)?;

        Ok(Ast::Cast(Cast {
            expr: Box::new(expr),
            target_type: Box::new(target_type),
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
                span: start_span.to(token.span),
            }),

            Int(i) => Ast::MemberAccess(ast::MemberAccess {
                expr: Box::new(expr),
                member: ustr(&i.to_string()),
                span: start_span.to(token.span),
            }),

            Float(_) => {
                // this is for chained tuple access like `tuple.0.1`
                let components = token.lexeme.split('.').collect::<Vec<&str>>();

                let first_access = Ast::MemberAccess(ast::MemberAccess {
                    expr: Box::new(expr),
                    member: ustr(components[0]),
                    span: start_span.to(token.span.with_end(EndPosition {
                        index: token.span.end.index - components[0].len() + 1,
                    })),
                });

                Ast::MemberAccess(ast::MemberAccess {
                    expr: Box::new(first_access),
                    member: ustr(components[0]),
                    span: start_span.to(token.span),
                })
            }

            Star => Ast::Unary(ast::Unary {
                op: UnaryOp::Deref,
                value: Box::new(expr),
                span: start_span.to(token.span),
            }),

            OpenParen => self.parse_call(expr)?,

            _ => return Err(SyntaxError::expected(self.span(), "an identifier, number or *")),
        };

        Ok(expr)
    }

    fn parse_call(&mut self, callee: Ast) -> DiagnosticResult<Ast> {
        let start_span = callee.span();
        let args = parse_delimited_list!(
            self,
            CloseParen,
            Comma,
            {
                let value = self.parse_expression(false, true)?;
                let spread = eat!(self, DotDotDot);

                ast::CallArg { value, spread }
            },
            ", or )"
        );

        Ok(Ast::Call(Call {
            callee: Box::new(callee),
            args,
            span: start_span.to(self.previous_span()),
        }))
    }

    fn parse_subscript_or_slice(&mut self, expr: Ast) -> DiagnosticResult<Ast> {
        let start_span = expr.span();

        if eat!(self, DotDot) {
            let high = if eat!(self, CloseBracket) {
                None
            } else {
                let high = self.parse_expression(false, true)?;
                require!(self, CloseBracket, "]")?;
                Some(Box::new(high))
            };

            Ok(Ast::Slice(ast::Slice {
                expr: Box::new(expr),
                low: None,
                high,
                span: start_span.to(self.previous_span()),
            }))
        } else {
            let index = self.parse_expression(false, true)?;

            if eat!(self, DotDot) {
                let high = if eat!(self, CloseBracket) {
                    None
                } else {
                    let high = self.parse_expression(false, true)?;
                    require!(self, CloseBracket, "]")?;
                    Some(Box::new(high))
                };

                Ok(Ast::Slice(ast::Slice {
                    expr: Box::new(expr),
                    low: Some(Box::new(index)),
                    high,
                    span: start_span.to(self.previous_span()),
                }))
            } else {
                require!(self, CloseBracket, "]")?;

                Ok(Ast::Subscript(ast::Subscript {
                    expr: Box::new(expr),
                    index: Box::new(index),
                    span: start_span.to(self.previous_span()),
                }))
            }
        }
    }
}
