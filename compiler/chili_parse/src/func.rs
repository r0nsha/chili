use crate::*;
use chili_ast::{
    ast::{Expr, ExprKind, Function, FunctionKind, FunctionParam, FunctionSig},
    ty::Ty,
};
use chili_error::{DiagnosticResult, SyntaxError};
use chili_span::To;
use ustr::Ustr;

impl Parser {
    pub(crate) fn parse_fn(&mut self, kind: FunctionKind) -> DiagnosticResult<Expr> {
        let name = self.get_decl_name();
        let start_span = self.previous_span();

        let sig = self.parse_fn_sig(kind, name)?;

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

    pub(crate) fn parse_fn_sig(
        &mut self,
        kind: FunctionKind,
        name: Ustr,
    ) -> DiagnosticResult<FunctionSig> {
        let start_span = self.previous_span();

        let (params, variadic) = self.parse_fn_params()?;

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
            variadic,
            ret: ret_ty,
            kind,
            ty: Default::default(),
            span: start_span.to(self.previous_span()),
        })
    }

    pub(crate) fn parse_fn_params(&mut self) -> DiagnosticResult<(Vec<FunctionParam>, bool)> {
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
                    require!(self, CloseParen, ")")?;
                    variadic = true;
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

        Ok((params, variadic))
    }
}
