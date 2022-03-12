use crate::*;
use chili_ast::ty::TyKind;
use chili_ast::{
    ast::{Block, Expr, ExprKind, Fn, FnParam, Proto},
    pattern::{Pattern, SymbolPattern},
};
use chili_error::{DiagnosticResult, SyntaxError};
use chili_span::{Span, To};
use ustr::{ustr, Ustr};

impl<'p> Parser<'p> {
    pub(crate) fn parse_fn(&mut self) -> DiagnosticResult<Expr> {
        let name = self.get_decl_name();
        let start_span = self.previous().span;

        let proto = self.parse_fn_proto(name, ParseProtoKind::Value)?;

        let body = self.parse_fn_body()?;

        Ok(Expr::new(
            ExprKind::Fn(Fn {
                proto,
                body,
                is_entry_point: false,
            }),
            start_span.to(self.previous_span()),
        ))
    }

    pub(crate) fn parse_fn_proto(
        &mut self,
        name: Ustr,
        kind: ParseProtoKind,
    ) -> DiagnosticResult<Proto> {
        let (params, variadic) = self.parse_fn_params(kind)?;

        let ret_ty = if eat!(self, RightArrow) {
            Some(Box::new(self.parse_ty()?))
        } else {
            None
        };

        Ok(Proto {
            lib_name: None,
            name,
            params,
            variadic,
            ret: ret_ty,
            ty: TyKind::Unknown,
            binding_info_idx: None,
        })
    }

    // TODO: this function is a hot mess, i need to refactor this
    pub(crate) fn parse_fn_params(
        &mut self,
        kind: ParseProtoKind,
    ) -> DiagnosticResult<(Vec<FnParam>, bool)> {
        if !eat!(self, OpenParen) {
            return Ok((vec![], false));
        }

        let mut variadic = false;

        let params = parse_delimited_list!(
            self,
            CloseParen,
            Comma,
            {
                if eat!(self, DotDot) {
                    expect!(self, CloseParen, ")")?;
                    variadic = true;
                    break;
                }

                match kind {
                    ParseProtoKind::Value => {
                        let pattern = self.parse_pattern()?;

                        let ty = if eat!(self, Colon) {
                            let ty = self.parse_ty()?;
                            Some(Box::new(ty))
                        } else {
                            None
                        };

                        FnParam { pattern, ty }
                    }
                    ParseProtoKind::Type => {
                        // the parameter's name is optional, so we are checking
                        // for ambiguity here
                        if eat!(self, Id(_)) || eat!(self, Placeholder) {
                            if eat!(self, Colon) {
                                // (a: type, ..)
                                self.revert(2);
                                let pattern = Pattern::Single(self.parse_symbol_pattern()?);
                                expect!(self, Colon, ":")?;
                                let ty = Some(Box::new(self.parse_ty()?));
                                FnParam { pattern, ty }
                            } else {
                                // (type, ..)
                                self.revert(1);
                                let pattern = Pattern::Single(SymbolPattern {
                                    binding_info_idx: Default::default(),
                                    symbol: ustr(""),
                                    alias: None,
                                    span: Span::unknown(),
                                    is_mutable: false,
                                    ignore: true,
                                });

                                let ty = Some(Box::new(self.parse_ty()?));
                                FnParam { pattern, ty }
                            }
                        } else {
                            // (a: type, ..)
                            let pattern = Pattern::Single(self.parse_symbol_pattern()?);
                            expect!(self, Colon, ":")?;
                            let ty = Some(Box::new(self.parse_ty()?));
                            FnParam { pattern, ty }
                        }
                    }
                }
            },
            ", or )"
        );

        Ok((params, variadic))
    }

    pub(crate) fn parse_fn_body(&mut self) -> DiagnosticResult<Block> {
        expect!(self, OpenCurly, "{")?;
        let block = self.parse_block()?;

        Ok(match block.kind {
            ExprKind::Block(block) => block,
            _ => unreachable!(),
        })
    }
}

pub(crate) enum ParseProtoKind {
    Value,
    Type,
}
