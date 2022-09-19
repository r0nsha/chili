use super::*;
use crate::{ast, error::DiagnosticResult};

impl Parser {
    pub(super) fn parse_attrs(&mut self) -> DiagnosticResult<Vec<ast::Attr>> {
        let mut attrs = vec![];

        if !is!(self, At) {
            return Ok(attrs);
        }

        while eat!(self, At) {
            let start_span = self.previous_span();

            self.skip_newlines();

            let id = self.require_ident()?;
            let name = id.name();

            let name_and_span = ast::NameAndSpan { name, span: id.span };

            let value = if eat!(self, OpenParen) {
                let value = self.parse_expression(false, true)?;
                require!(self, CloseParen, ")")?;
                Some(Box::new(value))
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
