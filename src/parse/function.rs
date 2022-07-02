use super::*;
use crate::{
    ast::{Function, FunctionParam, FunctionSig, FunctionVarargs},
    error::{DiagnosticResult, SyntaxError},
    span::To,
    types::FunctionTypeKind,
};
use ustr::Ustr;

impl Parser {
    pub fn parse_function(
        &mut self,
        name: Ustr,
        extern_lib: Option<Option<ExternLibrary>>,
    ) -> DiagnosticResult<Ast> {
        let start_span = self.previous_span();

        let sig = self.parse_function_sig(name, extern_lib)?;

        if eat!(self, OpenCurly) {
            let body = self.parse_block()?;

            Ok(Ast::Function(Function {
                sig,
                body,
                span: start_span.to(self.previous_span()),
            }))
        } else {
            Ok(Ast::FunctionType(sig))
        }
    }

    pub fn parse_function_sig(
        &mut self,
        name: Ustr,
        extern_lib: Option<Option<ExternLibrary>>,
    ) -> DiagnosticResult<FunctionSig> {
        let is_extern = extern_lib.is_some();

        let start_span = self.previous_span();

        let (params, varargs) = self.parse_function_params(is_extern)?;

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
            kind: extern_lib.as_ref().map_or(FunctionTypeKind::Orphan, |lib| {
                FunctionTypeKind::Extern { lib: lib.clone() }
            }),
            span: start_span.to(self.previous_span()),
        })
    }

    pub fn parse_function_params(
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

                    let name = require!(self, Ident(_), "an identifier")?.name();

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
                }
            },
            ", or )"
        );

        Ok((params, varargs))
    }
}
