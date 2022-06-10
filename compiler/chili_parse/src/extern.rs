use crate::*;
use chili_ast::{
    ast,
    path::RelativeTo,
    pattern::{Pattern, SymbolPattern},
    ty::Ty,
    workspace::{BindingInfoId, ModuleId},
};
use chili_error::{DiagnosticResult, SyntaxError};
use chili_span::To;

impl Parser {
    pub(crate) fn parse_extern(
        &mut self,
        visibility: ast::Visibility,
        start_span: Span,
    ) -> DiagnosticResult<ast::Binding> {
        let lib = eat!(self, Str(_)).then(|| self.previous().symbol());

        let lib = if let Some(lib) = lib {
            let lib = ast::ExternLibrary::try_from_str(
                &lib,
                RelativeTo::Path(self.module_info.dir()),
                self.previous_span(),
            )?;

            Some(lib)
        } else {
            None
        };

        let is_mutable = eat!(self, Mut);

        let id = require!(self, Ident(_), "an identifier")?;

        let pattern = Pattern::Symbol(SymbolPattern {
            id: BindingInfoId::unknown(),
            symbol: id.symbol(),
            alias: None,
            is_mutable,
            span: id.span,
            ignore: false,
        });

        require!(self, Colon, ":")?;

        self.extern_lib = Some(lib.clone());
        let ty_expr = self.parse_expr()?;
        self.extern_lib = None;

        Ok(ast::Binding {
            module_id: ModuleId::default(),
            visibility,
            kind: ast::BindingKind::Extern(lib),
            pattern,
            ty: Ty::unknown(),
            ty_expr: Some(ty_expr),
            expr: None,
            span: start_span.to(self.previous_span()),
        })
    }
}
