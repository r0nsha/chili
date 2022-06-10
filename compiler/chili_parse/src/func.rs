use crate::*;
use chili_ast::{
    ast::{Expr, ExprKind, Function, FunctionParam, FunctionSig, FunctionVarargs},
    ty::Ty,
};
use chili_error::{DiagnosticResult, SyntaxError};
use chili_span::To;
use ustr::Ustr;

impl Parser {
    pub(crate) fn parse_fn(&mut self) -> DiagnosticResult<Expr> {
        let name = self.get_decl_name();
        let start_span = self.previous_span();

        let sig = self.parse_fn_sig(name)?;

        if eat!(self, OpenCurly) {
            let body = self.parse_block()?;

            Ok(Expr::new(
                ExprKind::Function(Function {
                    sig,
                    body,
                    binding_info_id: None,
                    is_entry_point: false,
                }),
                start_span.to(self.previous_span()),
            ))
        } else {
            Ok(Expr::new(
                ExprKind::FunctionType(sig),
                start_span.to(self.previous_span()),
            ))
        }
    }

    pub(crate) fn parse_fn_sig(&mut self, name: Ustr) -> DiagnosticResult<FunctionSig> {
        let start_span = self.previous_span();

        let (params, varargs) = self.parse_fn_params()?;

        let ret_ty = if eat!(self, RightArrow) {
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
            ret: ret_ty,
            kind: self
                .extern_lib
                .as_ref()
                .map_or(ast::FunctionKind::Orphan, |lib| ast::FunctionKind::Extern {
                    lib: lib.clone(),
                }),
            ty: Default::default(),
            span: start_span.to(self.previous_span()),
        })
    }

    pub(crate) fn parse_fn_params(
        &mut self,
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

                    let ty = if eat!(self, Colon) {
                        Some(Box::new(self.parse_expr()?))
                    } else {
                        None
                    };

                    let end_span = self.previous_span();

                    require!(self, CloseParen, ")")?;

                    varargs = Some(FunctionVarargs {
                        name,
                        ty,
                        span: start_span.to(end_span),
                    });

                    break;
                }

                let pattern = self.parse_pattern()?;

                let ty = if eat!(self, Colon) {
                    Some(Box::new(self.parse_expr()?))
                } else {
                    None
                };

                FunctionParam {
                    pattern,
                    ty_expr: ty,
                    ty: Ty::unknown(),
                }
            },
            ", or )"
        );

        Ok((params, varargs))
    }
}
