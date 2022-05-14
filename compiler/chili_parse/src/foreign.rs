use crate::*;
use chili_ast::{
    ast::{Binding, BindingKind, Expr, ExprKind, Visibility},
    pattern::{Pattern, SymbolPattern},
    ty::Ty,
};
use chili_error::{DiagnosticResult, SyntaxError};
use chili_span::To;
use ustr::{ustr, Ustr};

impl<'p> Parser<'p> {
    pub(crate) fn parse_foreign_block(&mut self) -> DiagnosticResult<Vec<Binding>> {
        let lib_name = self.parse_lib_name()?;

        expect!(self, OpenCurly, "{")?;

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

                expect!(self, Let, "let")?;

                self.parse_foreign_binding(lib_name, visibility)?
            },
            "; or }"
        );

        Ok(bindings)
    }

    pub(crate) fn parse_foreign_single(
        &mut self,
        visibility: Visibility,
    ) -> DiagnosticResult<Binding> {
        let lib_name = self.parse_lib_name()?;
        let binding = self.parse_foreign_binding(lib_name, visibility)?;
        Ok(binding)
    }

    fn parse_foreign_binding(
        &mut self,
        lib_name: Ustr,
        visibility: Visibility,
    ) -> DiagnosticResult<Binding> {
        let start_span = self.previous_span();

        let id = expect!(self, Ident(_), "identifier")?;

        let pattern = Pattern::Single(SymbolPattern {
            binding_info_id: Default::default(),
            symbol: id.symbol(),
            alias: None,
            is_mutable: false,
            span: id.span,
            ignore: false,
        });

        let binding = if eat!(self, Eq) {
            expect!(self, Fn, "fn")?;

            let fn_sig_start_span = self.previous().span;
            let mut sig = self.parse_fn_sig(id.symbol())?;
            sig.lib_name = Some(lib_name);

            Binding {
                module_id: Default::default(),
                visibility,
                kind: BindingKind::Value,
                pattern,
                ty: Ty::unknown(),
                ty_expr: None,
                expr: Some(Expr::new(
                    ExprKind::FnType(sig),
                    fn_sig_start_span.to(self.previous_span()),
                )),
                lib_name: Some(lib_name),
                span: start_span.to(self.previous_span()),
            }
        } else {
            let ty_expr = self.parse_ty()?;

            Binding {
                module_id: Default::default(),
                visibility,
                kind: BindingKind::Value,
                pattern,
                ty: Ty::unknown(),
                ty_expr: Some(ty_expr),
                expr: None,
                lib_name: Some(lib_name),
                span: start_span.to(self.previous_span()),
            }
        };

        Ok(binding)
    }

    fn parse_lib_name(&mut self) -> DiagnosticResult<Ustr> {
        expect!(self, OpenParen, "(")?;

        let lib_token = expect!(self, Str(_), "str")?;
        let lib = lib_token.symbol();

        expect!(self, CloseParen, ")")?;

        Ok(lib)
    }
}
