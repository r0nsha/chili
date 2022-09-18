use super::*;
use crate::{
    ast::pat::{NamePat, Pat, StructPat, StructSubPat},
    types::FunctionTypeKind,
    workspace::BindingId,
};

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
        } else if eat!(self, Import) {
            if is!(self, OpenParen) {
                // This is considered Ok, since this could be a use expression
                self.revert(1);
                Ok(None)
            } else {
                Ok(Some(self.parse_import_binding(attrs, vis)))
            }
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

        let id = self.require_ident()?;
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
            let id = self.require_ident()?;
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

            let id = self.require_ident()?;
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

        let id = self.require_ident()?;
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

    pub fn parse_import_binding(&mut self, attrs: Vec<ast::Attr>, vis: ast::Vis) -> DiagnosticResult<ast::Binding> {
        let start_span = self.previous_span();

        let ident = self.require_ident()?;
        let name = ident.name();

        let import_expr = self.search_import_name(name, ident.span)?;

        let pat = self.parse_import_binding_pat(Some(ident))?;

        Ok(ast::Binding {
            attrs,
            vis,
            kind: ast::BindingKind::Let {
                pat,
                type_expr: None,
                value: Box::new(import_expr),
            },
            span: start_span.to(self.previous_span()),
        })
    }

    fn parse_import_binding_pat(&mut self, ident: Option<Token>) -> DiagnosticResult<Pat> {
        let ident = if let Some(ident) = ident {
            ident
        } else {
            self.require_ident()?
        };

        if eat!(self, Dot) {
            if eat!(self, Ident(_)) {
                let sym = self.previous().clone();
                let name = sym.name();

                let subpat = self.parse_import_binding_subpat(name, sym)?;

                let span = ident.span.to(subpat.span());

                Ok(Pat::Struct(StructPat {
                    subpats: vec![subpat],
                    span,
                    glob: None,
                }))
            } else if eat!(self, OpenCurly) {
                self.skip_newlines();

                let subpats = if eat!(self, CloseCurly) {
                    vec![]
                } else {
                    parse_delimited_list!(
                        self,
                        CloseCurly,
                        Comma,
                        {
                            self.skip_newlines();

                            let sym = self.require_ident()?;
                            let name = sym.name();

                            self.skip_newlines();

                            self.parse_import_binding_subpat(name, sym)?
                        },
                        "a , or }"
                    )
                };

                let span = if let Some(last) = subpats.last() {
                    last.span()
                } else {
                    ident.span
                };

                self.skip_newlines();

                Ok(Pat::Struct(StructPat {
                    subpats,
                    span,
                    glob: None,
                }))
            } else {
                Err(SyntaxError::expected(self.span(), "an identifier or {"))
            }
        } else if eat!(self, As) {
            let alias = self.require_ident()?;
            let name = alias.name();

            Ok(Pat::Name(NamePat {
                id: BindingId::unknown(),
                name,
                span: alias.span,
                is_mutable: false,
                ignore: false,
            }))
        } else {
            Ok(Pat::Name(NamePat {
                id: BindingId::unknown(),
                name: ident.name(),
                span: ident.span,
                is_mutable: false,
                ignore: false,
            }))
        }
    }

    fn parse_import_binding_subpat(&mut self, name: Ustr, sym: Token) -> DiagnosticResult<StructSubPat> {
        if is!(self, Dot) {
            // Nested subpath: foo.bar.baz.qux
            let name_and_span = ast::NameAndSpan { name, span: sym.span };
            let pat = self.parse_import_binding_pat(Some(sym))?;
            Ok(StructSubPat::NameAndPat(name_and_span, pat))
        } else if eat!(self, As) {
            // Aliased subpath: foo as qux
            let name_and_span = ast::NameAndSpan { name, span: sym.span };

            let alias_tok = self.require_ident()?;
            let alias = alias_tok.name();

            Ok(StructSubPat::NameAndPat(
                name_and_span,
                Pat::Name(NamePat {
                    id: BindingId::unknown(),
                    name: alias,
                    span: alias_tok.span,
                    is_mutable: false,
                    ignore: false,
                }),
            ))
        } else {
            // Subpath: foo
            Ok(StructSubPat::Name(NamePat {
                id: BindingId::unknown(),
                name,
                span: sym.span,
                is_mutable: false,
                ignore: false,
            }))
        }
    }
}
