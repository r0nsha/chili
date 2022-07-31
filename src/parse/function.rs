use super::*;
use crate::{
    ast::{Function, FunctionParam, FunctionSig, FunctionVarargs},
    error::{DiagnosticResult, SyntaxError},
    span::To,
    types::FunctionTypeKind,
};
use ustr::Ustr;

impl Parser {
    pub fn parse_function(&mut self, name: Option<Ustr>, is_extern: bool) -> DiagnosticResult<Ast> {
        let start_span = self.previous_span();

        let sig = self.parse_function_sig(name, is_extern)?;

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

    pub fn parse_function_sig(&mut self, name: Option<Ustr>, is_extern: bool) -> DiagnosticResult<FunctionSig> {
        let start_span = self.previous_span();

        let (params, varargs) = self.parse_function_params()?;

        let return_type = if eat!(self, RightArrow) {
            Some(Box::new(self.parse_expr_res(Restrictions::NO_STRUCT_LITERAL)?))
        } else {
            None
        };

        Ok(FunctionSig {
            name,
            params,
            varargs,
            return_type,
            kind: if is_extern {
                FunctionTypeKind::Extern
            } else {
                FunctionTypeKind::Orphan
            },
            span: start_span.to(self.previous_span()),
        })
    }

    pub fn parse_function_params(&mut self) -> DiagnosticResult<(Vec<FunctionParam>, Option<FunctionVarargs>)> {
        if !eat!(self, OpenParen) {
            return Ok((vec![], None));
        }

        let mut varargs: Option<FunctionVarargs> = None;

        let params = parse_delimited_list!(
            self,
            CloseParen,
            Comma,
            {
                let pattern = self.parse_pattern()?;

                let type_expr = if eat!(self, Colon) {
                    Some(Box::new(self.parse_expr()?))
                } else {
                    None
                };

                if eat!(self, DotDotDot) {
                    let name = if let Some(name) = pattern.as_name() {
                        name.clone()
                    } else {
                        return Err(SyntaxError::expected(pattern.span(), "an identifier or _"));
                    };

                    let end_span = self.previous_span();

                    require!(self, CloseParen, ")")?;

                    varargs = Some(FunctionVarargs {
                        name,
                        type_expr,
                        span: pattern.span().to(end_span),
                    });

                    break;
                } else {
                    let default_value = if eat!(self, Eq) {
                        Some(Box::new(self.parse_expr()?))
                    } else {
                        None
                    };

                    FunctionParam {
                        pattern,
                        type_expr,
                        default_value,
                    }
                }
            },
            ", or )"
        );

        Ok((params, varargs))
    }
}
