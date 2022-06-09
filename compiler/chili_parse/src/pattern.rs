use crate::*;
use chili_ast::pattern::{HybridPattern, Pattern, SymbolPattern, UnpackPattern, UnpackPatternKind};
use chili_error::SyntaxError;
use chili_span::To;

impl Parser {
    pub(crate) fn parse_pattern(&mut self) -> DiagnosticResult<Pattern> {
        if is!(self, Mut | Ident(_) | Placeholder) {
            let pattern = self.parse_symbol_pattern().map(Pattern::Symbol)?;

            if eat!(self, At) {
                let pattern = pattern.into_symbol();
                let start_span = pattern.span;

                let unpack_pattern = match self.parse_unpack_pattern("an unpack pattern")? {
                    Pattern::StructUnpack(pat) => UnpackPatternKind::Struct(pat),
                    Pattern::TupleUnpack(pat) => UnpackPatternKind::Tuple(pat),
                    _ => panic!(),
                };

                Ok(Pattern::Hybrid(HybridPattern {
                    symbol: pattern,
                    unpack: unpack_pattern,
                    span: start_span.to(self.previous_span()),
                }))
            } else {
                Ok(pattern)
            }
        } else {
            self.parse_unpack_pattern("a pattern")
        }
    }

    fn parse_unpack_pattern(&mut self, expectation: &str) -> DiagnosticResult<Pattern> {
        if eat!(self, OpenCurly) {
            self.parse_struct_unpack()
        } else if eat!(self, OpenParen) {
            self.parse_tuple_unpack()
        } else if eat!(self, QuestionMark) {
            let span = self.previous_span();
            Ok(Pattern::StructUnpack(UnpackPattern {
                symbols: vec![],
                span,
                wildcard_symbol: Some(span),
            }))
        } else {
            Err(SyntaxError::expected(self.span(), expectation))
        }
    }

    fn parse_struct_unpack(&mut self) -> DiagnosticResult<Pattern> {
        let start_span = self.previous_span();

        let mut symbols = vec![];
        let mut wildcard_symbol: Option<Span> = None;

        while !eat!(self, CloseCurly) && !self.is_end() {
            if eat!(self, QuestionMark) {
                wildcard_symbol = Some(self.previous().span);
                require!(self, CloseCurly, "}")?;
                break;
            } else {
                let mut symbol_pattern = self.parse_symbol_pattern()?;

                if eat!(self, Colon) {
                    let id_token = require!(self, Ident(_), "an identifier")?;
                    let symbol = id_token.symbol();
                    symbol_pattern.alias = Some(symbol);
                }

                symbols.push(symbol_pattern);

                if eat!(self, Comma) {
                    continue;
                } else if eat!(self, CloseCurly) {
                    break;
                } else {
                    let span = Parser::get_missing_delimiter_span(self.previous_span());
                    return Err(SyntaxError::expected(span, ", or }"));
                }
            }
        }

        let unpack = UnpackPattern {
            symbols,
            span: start_span.to(self.previous_span()),
            wildcard_symbol,
        };

        Ok(Pattern::StructUnpack(unpack))
    }

    fn parse_tuple_unpack(&mut self) -> DiagnosticResult<Pattern> {
        let start_span = self.previous_span();

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
            wildcard_symbol: None,
        };

        Ok(Pattern::TupleUnpack(unpack))
    }

    pub(super) fn parse_symbol_pattern(&mut self) -> DiagnosticResult<SymbolPattern> {
        let is_mutable = eat!(self, Mut);

        let (symbol, ignore) = if eat!(self, Ident(_)) {
            (self.previous().symbol(), false)
        } else if eat!(self, Placeholder) {
            (ustr(""), true)
        } else {
            return Err(SyntaxError::expected(self.span(), "an identifier or _"));
        };

        Ok(SymbolPattern {
            id: Default::default(),
            symbol,
            alias: None,
            is_mutable,
            span: self.previous_span(),
            ignore,
        })
    }
}
