use super::*;
use crate::{
    ast::{pattern::Pattern, Binding},
    common::path::RelativeTo,
    span::To,
    workspace::ModuleId,
};

impl Parser {
    pub fn try_parse_any_binding(
        &mut self,
        visibility: ast::Visibility,
    ) -> DiagnosticResult<Option<DiagnosticResult<Binding>>> {
        if eat!(self, Let) {
            Ok(Some(self.parse_binding(visibility, false)))
        } else if eat!(self, Static) {
            require!(self, Let, "let")?;
            Ok(Some(self.parse_binding(visibility, true)))
        } else if eat!(self, Extern) {
            Ok(Some(self.parse_extern_binding(visibility)))
        } else if eat!(self, Intrinsic) {
            Ok(Some(self.parse_intrinsic_binding(visibility)))
        } else if eat!(self, Type) {
            Ok(Some(self.parse_type_binding(visibility)))
        } else {
            Ok(None)
        }
    }

    pub fn parse_binding(
        &mut self,
        visibility: ast::Visibility,
        is_static: bool,
    ) -> DiagnosticResult<Binding> {
        let start_span = self.previous_span();

        let pattern = self.parse_pattern()?;

        let type_expr = if eat!(self, Colon) {
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
            kind: ast::BindingKind::Orphan {
                pattern,
                type_expr,
                value: Box::new(value),
                is_static,
            },
            span: start_span.to(self.previous_span()),
        })
    }

    pub fn parse_extern_binding(
        &mut self,
        visibility: ast::Visibility,
    ) -> DiagnosticResult<ast::Binding> {
        let start_span = self.previous_span();

        let lib = eat!(self, Str(_)).then(|| self.previous().name());

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

        require!(self, Let, "let")?;

        let is_mutable = eat!(self, Mut);

        let id = require!(self, Ident(_), "an identifier")?;
        let name = id.name();

        let name_and_span = ast::NameAndSpan {
            name,
            span: id.span,
        };

        let kind = if is_mutable {
            require!(self, Colon, ":")?;
            let type_expr = self.parse_expr()?;

            ast::BindingKind::ExternVariable {
                name: name_and_span,
                lib,
                is_mutable,
                type_expr: Box::new(type_expr),
            }
        } else if eat!(self, Colon) {
            let type_expr = self.parse_expr()?;

            ast::BindingKind::ExternVariable {
                name: name_and_span,
                lib,
                is_mutable,
                type_expr: Box::new(type_expr),
            }
        } else if eat!(self, Eq) {
            require!(self, Fn, "fn")?;
            let sig = self.parse_function_sig(name, Some(lib.clone()))?;

            ast::BindingKind::ExternFunction {
                name: name_and_span,
                lib,
                sig,
            }
        } else {
            return Err(SyntaxError::expected(self.previous_span(), ": or ="));
        };

        Ok(ast::Binding {
            module_id: ModuleId::unknown(),
            visibility,
            kind,
            span: start_span.to(self.previous_span()),
        })
    }

    pub fn parse_intrinsic_binding(
        &mut self,
        visibility: ast::Visibility,
    ) -> DiagnosticResult<ast::Binding> {
        let start_span = self.previous_span();

        require!(self, Let, "let")?;

        let id = require!(self, Ident(_), "an identifier")?;
        let name = id.name();

        require!(self, Eq, "=")?;
        require!(self, Fn, "fn")?;
        let function_type = self.parse_function_sig(name, None)?;

        Ok(ast::Binding {
            module_id: ModuleId::unknown(),
            visibility,
            kind: ast::BindingKind::Intrinsic {
                name: ast::NameAndSpan {
                    name,
                    span: id.span,
                },
                function_type,
            },
            span: start_span.to(self.previous_span()),
        })
    }

    pub fn parse_type_binding(&mut self, visibility: ast::Visibility) -> DiagnosticResult<Binding> {
        let start_span = self.previous_span();

        let id = require!(self, Ident(_), "an identifier")?;
        let name = id.name();

        require!(self, Eq, "=")?;

        let type_expr = self.parse_decl_expr(name)?;

        Ok(ast::Binding {
            module_id: ModuleId::unknown(),
            visibility,
            kind: ast::BindingKind::Type {
                name: ast::NameAndSpan {
                    name,
                    span: id.span,
                },
                type_expr: Box::new(type_expr),
            },
            span: start_span.to(self.previous_span()),
        })
    }
}
