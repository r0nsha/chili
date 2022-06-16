use crate::*;
use chili_ast::{
    ast::{Binding, BindingKind, Intrinsic, Visibility},
    path::RelativeTo,
    pattern::{Pattern, SymbolPattern},
    ty::Ty,
    workspace::{BindingInfoId, ModuleId},
};
use chili_error::diagnostic::Label;
use chili_span::To;

impl Parser {
    pub(crate) fn parse_binding(
        &mut self,
        visibility: Visibility,
        require_value: bool,
    ) -> DiagnosticResult<Binding> {
        let start_span = self.previous_span();

        let pattern = self.parse_pattern()?;

        let ty_expr = if eat!(self, Colon) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        if require_value {
            require!(self, Eq, "=")?;
        } else if !eat!(self, Eq) {
            return Ok(Binding {
                module_id: Default::default(),
                visibility,
                kind: BindingKind::Normal,
                pattern,
                ty: Ty::unknown(),
                ty_expr,
                expr: None,
                span: start_span.to(self.previous_span()),
            });
        }

        let expr = match &pattern {
            Pattern::Symbol(pattern) => self.parse_decl_expr(pattern.symbol)?,
            _ => self.parse_expr()?,
        };

        Ok(Binding {
            module_id: Default::default(),
            visibility,
            kind: BindingKind::Normal,
            pattern,
            ty: Ty::unknown(),
            ty_expr,
            expr: Some(expr),
            span: start_span.to(self.previous_span()),
        })
    }

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
            module_id: ModuleId::unknown(),
            visibility,
            kind: ast::BindingKind::Extern(lib),
            pattern,
            ty: Ty::unknown(),
            ty_expr: Some(ty_expr),
            expr: None,
            span: start_span.to(self.previous_span()),
        })
    }

    pub(crate) fn parse_builtin_binding(
        &mut self,
        visibility: ast::Visibility,
        start_span: Span,
    ) -> DiagnosticResult<ast::Binding> {
        let id = require!(self, Ident(_), "an identifier")?;
        let symbol = id.symbol();

        let intrinsic = Intrinsic::from_str(&symbol).ok_or_else(|| {
            Diagnostic::error()
                .with_message(format!("unknown intrinsic `{}`", symbol))
                .with_label(Label::primary(id.span, "unknown intrinsic"))
        })?;

        let pattern = Pattern::Symbol(SymbolPattern {
            id: BindingInfoId::unknown(),
            symbol: id.symbol(),
            alias: None,
            is_mutable: false,
            span: id.span,
            ignore: false,
        });

        require!(self, Colon, ":")?;

        let ty_expr = self.parse_expr()?;

        Ok(ast::Binding {
            module_id: ModuleId::unknown(),
            visibility,
            kind: ast::BindingKind::Intrinsic(intrinsic),
            pattern,
            ty: Ty::unknown(),
            ty_expr: Some(ty_expr),
            expr: None,
            span: start_span.to(self.previous_span()),
        })
    }
}
