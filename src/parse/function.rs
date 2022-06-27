use super::*;
use crate::{
    ast::{FunctionExpr, FunctionId, FunctionParam, FunctionSig, FunctionVarargs},
    error::{DiagnosticResult, SyntaxError},
    span::To,
    types::{FunctionTypeKind, TypeId},
};
use ustr::Ustr;

impl Parser {
    pub fn parse_fn(&mut self) -> DiagnosticResult<Ast> {
        let name = self.get_decl_name();
        let start_span = self.previous_span();

        let sig = self.parse_fn_sig(name)?;

        if eat!(self, OpenCurly) {
            let body = self.parse_block()?;

            Ok(Ast::Function(FunctionExpr {
                sig,
                body,
                id: FunctionId::unknown(),
                ty: Default::default(),
                span: start_span.to(self.previous_span()),
            }))
        } else {
            Ok(Ast::FunctionType(sig))
        }
    }

    pub fn parse_fn_sig(&mut self, name: Ustr) -> DiagnosticResult<FunctionSig> {
        let is_extern = self.extern_lib.is_some();

        let start_span = self.previous_span();

        let (params, varargs) = self.parse_fn_params(is_extern)?;

        let return_type = if eat!(self, RightArrow) {
            Some(Box::new(
                self.parse_expr_with_res(Restrictions::NO_STRUCT_LITERAL)?,
            ))
        } else {
            None
        };

        Ok(FunctionSig {
            name,
            params,
            varargs,
            return_type,
            kind: self
                .extern_lib
                .as_ref()
                .map_or(FunctionTypeKind::Orphan, |lib| FunctionTypeKind::Extern {
                    lib: lib.clone(),
                }),
            ty: Default::default(),
            span: start_span.to(self.previous_span()),
        })
    }

    pub fn parse_fn_params(
        &mut self,
        is_extern: bool,
    ) -> DiagnosticResult<(Vec<FunctionParam>, Option<FunctionVarargs>)> {
        if !eat!(self, OpenParen) {
            return Ok((vec![], None));
        }

        let mut varargs: Option<FunctionVarargs> = None;

        let params = parse_delimited_list!(
            self,
            CloseParen,
            Comma,
            {
                if eat!(self, DotDot) {
                    let start_span = self.previous_span();

                    let name = require!(self, Ident(_), "an identifier")?.symbol();

                    let type_expr = if eat!(self, Colon) {
                        Some(Box::new(self.parse_expr()?))
                    } else if !is_extern {
                        return Err(SyntaxError::expected(self.previous_span(), ":"));
                    } else {
                        None
                    };

                    let end_span = self.previous_span();

                    require!(self, CloseParen, ")")?;

                    varargs = Some(FunctionVarargs {
                        name,
                        type_expr,
                        span: start_span.to(end_span),
                    });

                    break;
                }

                let pattern = self.parse_pattern()?;

                let ty = if eat!(self, Colon) {
                    Some(Box::new(self.parse_expr()?))
                } else if is_extern {
                    return Err(SyntaxError::expected(self.previous_span(), ":"));
                } else {
                    None
                };

                FunctionParam {
                    pattern,
                    type_expr: ty,
                    ty: TypeId::unknown(),
                }
            },
            ", or )"
        );

        Ok((params, varargs))
    }
}
