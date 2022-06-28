use super::*;
use crate::{
    ast::{
        pattern::{NamePattern, Pattern},
        Binding, BindingKind, Intrinsic, Visibility,
    },
    common::path::RelativeTo,
    error::diagnostic::Label,
    span::To,
    types::TypeId,
    workspace::{BindingId, ModuleId},
};

impl Parser {
    pub fn parse_binding(&mut self, visibility: Visibility) -> DiagnosticResult<Binding> {
        let start_span = self.previous_span();

        let pattern = self.parse_pattern()?;

        let ty_expr = if eat!(self, Colon) {
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };

        require!(self, Eq, "=")?;

        let value = match &pattern {
            Pattern::Name(pattern) => self.parse_decl_expr(pattern.name)?,
            _ => self.parse_expr()?,
        };

        Ok(Binding {
            module_id: Default::default(),
            visibility,
            kind: BindingKind::Orphan,
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

        let pattern = Pattern::Name(NamePattern {
            id: BindingId::unknown(),
            name: id.symbol(),
            alias: None,
            is_mutable,
            span: id.span,
            ignore: false,
        });

        if eat!(self, Colon) {
            todo!("parse extern variables")
        } else if eat!(self, Eq) {
            self.extern_lib = Some(lib.clone());
            let value = self.parse_decl_expr(pattern.as_name().name)?;
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

        let pattern = Pattern::Name(NamePattern {
            id: BindingId::unknown(),
            name: id.symbol(),
            alias: None,
            is_mutable: false,
            span: id.span,
            ignore: false,
        });

        require!(self, Eq, "=")?;

        let value = self.parse_decl_expr(pattern.as_name().name)?;

        Ok(ast::Binding {
            module_id: ModuleId::unknown(),
            visibility,
            kind: ast::BindingKind::Intrinsic(intrinsic),
            pattern,
            ty: TypeId::unknown(),
            type_expr: None,
            value: Box::new(value),
            span: start_span.to(self.previous_span()),
        })
    }
}
