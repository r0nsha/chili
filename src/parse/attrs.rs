use super::*;
use crate::{
    ast::{
        self,
        attrs::{Attr, AttrKind, Attrs},
    },
    error::{diagnostic::Label, DiagnosticResult},
};

impl Parser {
    pub(super) fn parse_attrs(&mut self) -> DiagnosticResult<Attrs> {
        let mut attrs = Attrs::new();

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
                "intrinsic" => AttrKind::Intrinsic,
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

            let attr = Attr {
                name: name_and_span,
                kind,
                value,
                span: start_span.to(self.previous_span()),
            };

            if let Some(used_attr) = attrs.insert(attr) {
                return Err(Diagnostic::error()
                    .with_message(format!("attribute `{}` has already been used", name))
                    .with_label(Label::primary(id.span, "duplicate attribute"))
                    .with_label(Label::secondary(used_attr.name.span, "already used here")));
            }
        }

        Ok(attrs)
    }
}
