use crate::*;
use chili_ast::{
    ast::{self, Binding, BindingKind, Expr, ExprKind, Visibility},
    pattern::{Pattern, SymbolPattern},
    ty::Ty,
};
use chili_error::{DiagnosticResult, SyntaxError};
use chili_span::To;
use ustr::Ustr;

impl<'p> Parser<'p> {
    pub(crate) fn parse_extern_block(&mut self) -> DiagnosticResult<Vec<Binding>> {
        let lib_name = self.parse_lib_name()?;

        require!(self, OpenCurly, "{")?;

        let bindings = parse_delimited_list!(
            self,
            CloseCurly,
            Semicolon,
            {
                let visibility = if eat!(self, Pub) {
                    Visibility::Public
                } else {
                    Visibility::Private
                };

                require!(self, Let, "let")?;

                self.parse_foreign_binding(lib_name, visibility)?
            },
            "; or }"
        );

        Ok(bindings)
    }

    pub(crate) fn parse_extern_single(
        &mut self,
        visibility: Visibility,
    ) -> DiagnosticResult<Binding> {
        let lib_name = self.parse_lib_name()?;
        let binding = self.parse_foreign_binding(lib_name, visibility)?;
        Ok(binding)
    }

    fn parse_foreign_binding(
        &mut self,
        lib: Ustr,
        visibility: Visibility,
    ) -> DiagnosticResult<Binding> {
        let start_span = self.previous_span();

        let id = require!(self, Ident(_), "identifier")?;

        let pattern = Pattern::Single(SymbolPattern {
            binding_info_id: Default::default(),
            symbol: id.symbol(),
            alias: None,
            is_mutable: false,
            span: id.span,
            ignore: false,
        });

        let binding = if eat!(self, Eq) {
            require!(self, Fn, "fn")?;

            let fn_sig_start_span = self.previous_span();
            let sig = self.parse_fn_sig(ast::FunctionKind::Extern { lib }, id.symbol())?;

            Binding {
                module_id: Default::default(),
                visibility,
                kind: BindingKind::Value,
                pattern,
                ty: Ty::unknown(),
                ty_expr: None,
                expr: Some(Expr::new(
                    ExprKind::FunctionType(sig),
                    fn_sig_start_span.to(self.previous_span()),
                )),
                lib_name: Some(lib),
                span: start_span.to(self.previous_span()),
            }
        } else {
            let ty_expr = self.parse_expr()?;

            Binding {
                module_id: Default::default(),
                visibility,
                kind: BindingKind::Value,
                pattern,
                ty: Ty::unknown(),
                ty_expr: Some(ty_expr),
                expr: None,
                lib_name: Some(lib),
                span: start_span.to(self.previous_span()),
            }
        };

        Ok(binding)
    }

    fn parse_lib_name(&mut self) -> DiagnosticResult<Ustr> {
        require!(self, OpenParen, "(")?;

        let lib_token = require!(self, Str(_), "str")?;
        let lib = lib_token.symbol();

        require!(self, CloseParen, ")")?;

        Ok(lib)
    }
}
