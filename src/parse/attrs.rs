use super::*;
use crate::{ast, error::DiagnosticResult};

impl Parser {
    pub(super) fn parse_attrs(&mut self) -> DiagnosticResult<Vec<ast::Attr>> {
        let mut attrs = vec![];

        if !self.is_attr_start() {
            return Ok(attrs);
        }

        while eat!(self, Bang) {
            let start_span = self.previous_span();

            self.skip_newlines();

            require!(self, OpenBracket, "[")?;

            self.skip_newlines();

            let id = require!(self, Ident(_), "an identifier")?;
            let name = id.name();

            let name_and_span = ast::NameAndSpan { name, span: id.span };

            let value = if eat!(self, Eq) {
                let value = self.parse_expression(false, true)?;
                Some(Box::new(value))
            } else {
                None
            };

            require!(self, CloseBracket, "]")?;

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

    fn is_attr_start(&mut self) -> bool {
        if eat!(self, Bang) {
            let result = is!(self, OpenBracket);
            self.revert(1);
            result
        } else {
            false
        }
    }
}
