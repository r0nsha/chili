use super::*;
use crate::{ast, error::DiagnosticResult};

impl Parser {
    pub(super) fn parse_attrs(&mut self) -> DiagnosticResult<Vec<ast::Attr>> {
        let mut attrs = vec![];

        while is!(self, At) {
            self.skip_newlines();

            let start_span = self.bump().span;

            let id = require!(self, Ident(_), "an identifier")?;
            let name = id.name();

            let name_and_span = ast::NameAndSpan { name, span: id.span };

            let value = if eat!(self, OpenParen) {
                let value = Some(Box::new(self.parse_expression(false, true)?));
                require!(self, CloseParen, ")")?;
                value
            } else {
                None
            };

            let attr = ast::Attr {
                name: name_and_span,
                value,
                span: start_span.to(self.previous_span()),
            };

            attrs.push(attr);
        }

        self.skip_newlines();

        Ok(attrs)
    }
}
