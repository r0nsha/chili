use super::*;
use crate::{ast::pat::Pat, types::FunctionTypeKind};

impl Parser {
    pub fn try_parse_any_binding(
        &mut self,
        attrs: Vec<ast::Attr>,
        vis: ast::Vis,
        is_top_level: bool,
    ) -> DiagnosticResult<Option<DiagnosticResult<ast::Binding>>> {
        if eat!(self, Let) {
            Ok(Some(self.parse_binding(attrs, vis)))
        } else if eat!(self, Fn) {
            if is!(self, Ident(_)) {
                Ok(Some(self.parse_function_binding(attrs, vis)))
            } else if is_top_level {
                Err(SyntaxError::expected(self.span(), "an identifier"))
            } else {
                // This is considered Ok, since this could be a function expression or a function type
                self.revert(1);
                Ok(None)
            }
        } else if eat!(self, Extern) {
            Ok(Some(self.parse_extern_binding(attrs, vis)))
        } else if eat!(self, Type) {
            Ok(Some(self.parse_type_binding(attrs, vis)))
        } else {
            Ok(None)
        }
    }

    pub fn parse_binding(&mut self, attrs: Vec<ast::Attr>, vis: ast::Vis) -> DiagnosticResult<ast::Binding> {
        let start_span = self.previous_span();

        let pat = self.parse_pat()?;

        let type_expr = if eat!(self, Colon) {
            Some(Box::new(self.parse_expression(false, true)?))
        } else {
            None
        };

        require!(self, Eq, "=")?;

        let mut value = self.parse_expression(false, true)?;

        match &pat {
            Pat::Name(pat) => Self::assign_expr_name_if_needed(&mut value, pat.name),
            _ => (),
        }

        Ok(ast::Binding {
            attrs,
            vis,
            kind: ast::BindingKind::Let {
                pat,
                type_expr,
                value: Box::new(value),
            },
            span: start_span.to(self.previous_span()),
        })
    }

    pub fn parse_function_binding(&mut self, attrs: Vec<ast::Attr>, vis: ast::Vis) -> DiagnosticResult<ast::Binding> {
        let start_span = self.previous_span();

        let id = require!(self, Ident(_), "an identifier")?;
        let name = id.name();

        let name_and_span = ast::NameAndSpan { name, span: id.span };

        let (sig, _) = self.parse_function_sig(Some(name), FunctionTypeKind::Orphan, true)?;

        require!(self, Eq, "=")?;

        let body = Box::new(self.parse_expression(false, true)?);

        Ok(ast::Binding {
            attrs,
            vis,
            kind: ast::BindingKind::Function {
                name: name_and_span,
                sig,
                body,
            },
            span: start_span.to(self.previous_span()),
        })
    }

    pub fn parse_extern_binding(&mut self, attrs: Vec<ast::Attr>, vis: ast::Vis) -> DiagnosticResult<ast::Binding> {
        let start_span = self.previous_span();

        if eat!(self, Fn) {
            let id = require!(self, Ident(_), "an identifier")?;
            let name = id.name();

            let name_and_span = ast::NameAndSpan { name, span: id.span };

            let (sig, _) = self.parse_function_sig(Some(name), FunctionTypeKind::Extern, true)?;

            Ok(ast::Binding {
                attrs,
                vis,
                kind: ast::BindingKind::ExternFunction {
                    name: name_and_span,
                    sig,
                },
                span: start_span.to(self.previous_span()),
            })
        } else if eat!(self, Let) {
            let is_mutable = eat!(self, Mut);

            let id = require!(self, Ident(_), "an identifier")?;
            let name = id.name();

            let name_and_span = ast::NameAndSpan { name, span: id.span };

            require!(self, Colon, ":")?;

            let type_expr = self.parse_expression(false, true)?;

            Ok(ast::Binding {
                attrs,
                vis,
                kind: ast::BindingKind::ExternVariable {
                    name: name_and_span,
                    is_mutable,
                    type_expr: Box::new(type_expr),
                },
                span: start_span.to(self.previous_span()),
            })
        } else {
            Err(SyntaxError::expected(self.span(), "fn or let"))
        }
    }

    pub fn parse_type_binding(&mut self, attrs: Vec<ast::Attr>, vis: ast::Vis) -> DiagnosticResult<ast::Binding> {
        let start_span = self.previous_span();

        let id = require!(self, Ident(_), "an identifier")?;
        let name = id.name();

        require!(self, Eq, "=")?;

        let mut type_expr = self.parse_expression(false, true)?;
        Self::assign_expr_name_if_needed(&mut type_expr, name);

        Ok(ast::Binding {
            attrs,
            vis,
            kind: ast::BindingKind::Type {
                name: ast::NameAndSpan { name, span: id.span },
                type_expr: Box::new(type_expr),
            },
            span: start_span.to(self.previous_span()),
        })
    }
}
