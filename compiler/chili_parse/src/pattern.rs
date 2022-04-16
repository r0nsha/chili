use crate::*;
use chili_ast::pattern::{Pattern, SymbolPattern, UnpackPattern};
use chili_error::SyntaxError;
use chili_span::To;

impl<'p> Parser<'p> {
    pub(crate) fn parse_pattern(&mut self) -> DiagnosticResult<Pattern> {
        match self.parse_symbol_pattern() {
            Ok(symbol) => Ok(Pattern::Single(symbol)),
            Err(_) => {
                if eat!(self, OpenCurly) {
                    self.parse_struct_unpack()
                } else if eat!(self, OpenParen) {
                    self.parse_tuple_unpack()
                } else {
                    Err(SyntaxError::expected(self.span(), "a pattern"))
                }
            }
        }
    }

    fn parse_struct_unpack(&mut self) -> DiagnosticResult<Pattern> {
        let start_span = self.previous().span;

        let symbols = parse_delimited_list!(
            self,
            CloseCurly,
            Comma,
            {
                let mut symbol_pattern = self.parse_symbol_pattern()?;

                if eat!(self, Colon) {
                    let id_token = expect!(self, Id(_), "identifier")?;
                    let symbol = id_token.symbol();
                    symbol_pattern.alias = Some(symbol);
                }

                symbol_pattern
            },
            ", or }"
        );

        let unpack = UnpackPattern {
            symbols,
            span: start_span.to(self.previous_span()),
        };

        Ok(Pattern::StructUnpack(unpack))
    }

    fn parse_tuple_unpack(&mut self) -> DiagnosticResult<Pattern> {
        let start_span = self.previous().span;

        let symbols = parse_delimited_list!(
            self,
            CloseParen,
            Comma,
            self.parse_symbol_pattern()?,
            ", or )"
        );

        let unpack = UnpackPattern {
            symbols,
            span: start_span.to(self.previous_span()),
        };

        Ok(Pattern::TupleUnpack(unpack))
    }

    pub(super) fn parse_symbol_pattern(&mut self) -> DiagnosticResult<SymbolPattern> {
        let is_mutable = eat!(self, Mut);

        let (symbol, ignore) = if eat!(self, Id(_)) {
            (self.previous().symbol(), false)
        } else if eat!(self, Placeholder) {
            (ustr(""), true)
        } else {
            return Err(SyntaxError::expected(self.span(), "identifier or _"));
        };

        Ok(SymbolPattern {
            binding_info_id: Default::default(),
            symbol,
            alias: None,
            is_mutable,
            span: self.previous_span(),
            ignore,
        })
    }
}