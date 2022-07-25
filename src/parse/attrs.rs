use super::*;
use crate::{ast, error::DiagnosticResult};

impl Parser {
    pub(super) fn parse_attrs(&mut self) -> DiagnosticResult<Vec<ast::Attr>> {
        let mut attrs = vec![];

        while is!(self, Hash) {
            let start_span = self.bump().span;

            require!(self, OpenBracket, "[")?;

            let id = require!(self, Ident(_), "an identifier")?;
            let name = id.name();

            let name_and_span = ast::NameAndSpan { name, span: id.span };

            let value = if eat!(self, Eq) {
                Some(Box::new(self.parse_expr()?))
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

        Ok(attrs)
    }
}
