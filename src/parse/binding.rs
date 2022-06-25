use super::*;
use crate::ast::{
    path::RelativeTo,
    pattern::{Pattern, SymbolPattern},
    ty::TypeId,
    workspace::{BindingId, ModuleId},
    Binding, BindingKind, Intrinsic, Visibility,
};
use crate::error::diagnostic::Label;
use crate::span::To;

impl Parser {
    pub fn parse_binding(
        &mut self,
        visibility: Visibility,
        require_value: bool,
    ) -> DiagnosticResult<Binding> {
        let start_span = self.previous_span();

        let pattern = self.parse_pattern()?;

        let ty_expr = if eat!(self, Colon) {
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };

        require!(self, Eq, "=")?;

        let value = match &pattern {
            Pattern::Symbol(pattern) => self.parse_decl_expr(pattern.symbol)?,
            _ => self.parse_expr()?,
        };

        Ok(Binding {
            module_id: Default::default(),
            visibility,
            kind: BindingKind::Normal,
            pattern,
            ty: TypeId::unknown(),
            type_expr: ty_expr,
            value: Box::new(value),
            span: start_span.to(self.previous_span()),
        })
    }

    pub fn parse_extern(
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
            id: BindingId::unknown(),
            symbol: id.symbol(),
            alias: None,
            is_mutable,
            span: id.span,
            ignore: false,
        });

        if eat!(self, Colon) {
            todo!("parse extern variables")
        } else if eat!(self, Eq) {
            self.extern_lib = Some(lib.clone());
            let value = self.parse_decl_expr(pattern.as_symbol_ref().symbol)?;
            self.extern_lib = None;

            Ok(ast::Binding {
                module_id: ModuleId::unknown(),
                visibility,
                kind: ast::BindingKind::Extern(lib),
                pattern,
                ty: TypeId::unknown(),
                type_expr: None,
                value: Box::new(value),
                span: start_span.to(self.previous_span()),
            })
        } else {
            Err(SyntaxError::expected(self.previous_span(), ": or ="))
        }
    }

    pub fn parse_builtin_binding(
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
            id: BindingId::unknown(),
            symbol: id.symbol(),
            alias: None,
            is_mutable: false,
            span: id.span,
            ignore: false,
        });

        require!(self, Eq, "=")?;

        let value = self.parse_decl_expr(pattern.as_symbol_ref().symbol)?;

        Ok(ast::Binding {
            module_id: ModuleId::unknown(),
            visibility,
            kind: ast::BindingKind::Intrinsic(intrinsic),
            pattern,
            ty: TypeId::unknown(),
            type_expr: Some(Box::new(value)),
            value: Box::new(value),
            span: start_span.to(self.previous_span()),
        })
    }
}
