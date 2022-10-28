use super::*;
use crate::{
    ast::{Function, FunctionParam, FunctionSig, FunctionVarargs},
    error::{DiagnosticResult, SyntaxError},
    types::FunctionTypeKind,
};
use ustr::Ustr;

impl Parser {
    pub fn parse_function_expr(&mut self, name: Option<Ustr>, kind: FunctionTypeKind) -> DiagnosticResult<Ast> {
        let start_span = self.previous_span();

        let (sig, used_parens) = self.parse_function_sig(name, kind, false)?;

        if is!(self, OpenCurly) {
            let body = self.parse_block()?;

            Ok(Ast::Function(Function {
                sig,
                body,
                span: start_span.to(self.previous_span()),
            }))
        } else if used_parens {
            Ok(Ast::FunctionType(sig))
        } else {
            Err(SyntaxError::expected(start_span.after(), "("))
        }
    }

    pub fn parse_function_sig(
        &mut self,
        name: Option<Ustr>,
        kind: FunctionTypeKind,
        require_parens: bool,
    ) -> DiagnosticResult<(FunctionSig, bool)> {
        let start_span = self.previous_span();

        let (params, varargs, used_parens) = self.parse_function_params(require_parens)?;

        let return_type = self.parse_function_return_type()?;

        Ok((
            FunctionSig {
                name,
                params,
                varargs,
                return_type,
                kind,
                span: start_span.to(self.previous_span()),
            },
            used_parens,
        ))
    }

    fn parse_function_params(
        &mut self,
        require_parens: bool,
    ) -> DiagnosticResult<(Vec<FunctionParam>, Option<FunctionVarargs>, bool)> {
        if !eat!(self, OpenParen) {
            if require_parens {
                return Err(SyntaxError::expected(self.span(), "("));
            }

            return Ok((vec![], None, false));
        }

        let mut varargs: Option<FunctionVarargs> = None;

        let params = parse_delimited_list!(
            self,
            CloseParen,
            Comma,
            {
                let pat = self.parse_pat()?;

                let type_expr = if eat!(self, Colon) {
                    Some(Box::new(self.parse_expression(false, true)?))
                } else {
                    None
                };

                if eat!(self, DotDot) {
                    let name = if let Some(name) = pat.as_name() {
                        name.clone()
                    } else {
                        return Err(SyntaxError::expected(pat.span(), "an identifier or _"));
                    };

                    let end_span = self.previous_span();

                    require!(self, CloseParen, ")")?;

                    varargs = Some(FunctionVarargs {
                        name,
                        type_expr,
                        span: pat.span().to(end_span),
                    });

                    break;
                } else {
                    let default_value = if eat!(self, Eq) {
                        Some(Box::new(self.parse_expression(false, true)?))
                    } else {
                        None
                    };

                    FunctionParam {
                        pat,
                        type_expr,
                        default_value,
                    }
                }
            },
            ", or )"
        );

        Ok((params, varargs, true))
    }

    fn parse_function_return_type(&mut self) -> DiagnosticResult<Option<Box<Ast>>> {
        if eat!(self, RightArrow) {
            Ok(Some(Box::new(self.parse_expression_res(
                Restrictions::NO_STRUCT_LITERAL,
                false,
                false,
            )?)))
        } else {
            Ok(None)
        }
    }
}
