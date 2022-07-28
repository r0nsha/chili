use super::*;
use crate::ast::pattern::{HybridPattern, NamePattern, Pattern, UnpackPattern, UnpackPatternKind, Wildcard};
use crate::error::SyntaxError;
use crate::span::To;

impl Parser {
    pub fn parse_pattern(&mut self) -> DiagnosticResult<Pattern> {
        if is!(self, Mut | Ident(_) | Placeholder) {
            let pattern = self.parse_name_pattern().map(Pattern::Name)?;

            if eat!(self, At) {
                let pattern = pattern.into_name().unwrap();
                let start_span = pattern.span;

                let unpack_pattern = match self.parse_unpack_pattern("an unpack pattern")? {
                    Pattern::StructUnpack(pattern) => UnpackPatternKind::Struct(pattern),
                    Pattern::TupleUnpack(pattern) => UnpackPatternKind::Tuple(pattern),
                    _ => panic!(),
                };

                Ok(Pattern::Hybrid(HybridPattern {
                    name_pattern: pattern,
                    unpack_pattern,
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
        } else if eat!(self, Star) {
            let span = self.previous_span();
            Ok(Pattern::StructUnpack(UnpackPattern {
                symbols: vec![],
                span,
                wildcard: Some(Wildcard { span }),
            }))
        } else {
            Err(SyntaxError::expected(self.span(), expectation))
        }
    }

    fn parse_struct_unpack(&mut self) -> DiagnosticResult<Pattern> {
        let start_span = self.previous_span();

        let mut symbols = vec![];
        let mut wildcard_span: Option<Span> = None;

        while !eat!(self, CloseCurly) && !self.is_end() {
            if eat!(self, Star) {
                wildcard_span = Some(self.previous().span);
                require!(self, CloseCurly, "}")?;
                break;
            } else {
                let mut symbol = self.parse_name_pattern()?;

                // This means the user used `_`, instead of `x: _` - which is illegal
                if symbol.ignore {
                    return Err(SyntaxError::expected(symbol.span, "an identifier, ? or }"));
                }

                if eat!(self, Colon) {
                    if eat!(self, Placeholder) {
                        symbol.ignore = true;
                    } else {
                        let id_token = require!(self, Ident(_), "an identifier")?;
                        symbol.alias = Some(id_token.name());
                    }
                }

                symbols.push(symbol);

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
            wildcard: wildcard_span.map(|span| Wildcard { span }),
        };

        Ok(Pattern::StructUnpack(unpack))
    }

    fn parse_tuple_unpack(&mut self) -> DiagnosticResult<Pattern> {
        let start_span = self.previous_span();

        let symbols = parse_delimited_list!(self, CloseParen, Comma, self.parse_name_pattern()?, ", or )");

        let unpack = UnpackPattern {
            symbols,
            span: start_span.to(self.previous_span()),
            wildcard: None,
        };

        Ok(Pattern::TupleUnpack(unpack))
    }

    pub fn parse_name_pattern(&mut self) -> DiagnosticResult<NamePattern> {
        let is_mutable = eat!(self, Mut);

        let (symbol, ignore) = if eat!(self, Ident(_)) {
            (self.previous().name(), false)
        } else if eat!(self, Placeholder) {
            (ustr(Placeholder.lexeme()), true)
        } else {
            return Err(SyntaxError::expected(self.span(), "an identifier or _"));
        };

        Ok(NamePattern {
            id: Default::default(),
            name: symbol,
            alias: None,
            is_mutable,
            span: self.previous_span(),
            ignore,
        })
    }
}
