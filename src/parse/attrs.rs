use super::*;
use crate::{
    ast::{self},
    error::{diagnostic::Label, DiagnosticResult},
};

impl Parser {
    pub(super) fn parse_attrs(&mut self) -> DiagnosticResult<Vec<ast::Attr>> {
        let mut attrs = vec![];

        while is!(self, Hash) {
            let start_span = self.bump().span;

            require!(self, OpenBracket, "[")?;

            let id = require!(self, Ident(_), "an identifier")?;
            let name = id.name();

            let name_and_span = ast::NameAndSpan {
                name,
                span: id.span,
            };

            let kind = match name.as_str() {
                "intrinsic" => ast::AttrKind::Intrinsic,
                _ => {
                    return Err(Diagnostic::error()
                        .with_message(format!("unknown attribute `{}`", name))
                        .with_label(Label::primary(id.span, "unknown attribute")))
                }
            };

            let value = if eat!(self, Eq) {
                Some(Box::new(self.parse_expr()?))
            } else {
                None
            };

            require!(self, CloseBracket, "]")?;

            let attr = ast::Attr {
                name: name_and_span,
                kind,
                value,
                span: start_span.to(self.previous_span()),
            };

            attrs.push(attr);
        }

        Ok(attrs)
    }
}
