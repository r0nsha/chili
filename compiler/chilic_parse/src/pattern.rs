use crate::*;
use chilic_ast::pattern::{DestructorPattern, Pattern, SymbolPattern};
use chilic_error::SyntaxError;
use chilic_span::To;

impl Parser {
    pub(crate) fn parse_pattern(&mut self) -> DiagnosticResult<Pattern> {
        match self.parse_symbol_pattern() {
            Ok(symbol) => Ok(Pattern::Single(symbol)),
            Err(_) => {
                if eat!(self, OpenCurly) {
                    self.parse_struct_destructor()
                } else if eat!(self, OpenParen) {
                    self.parse_tuple_destructor()
                } else {
                    Err(SyntaxError::expected(self.span(), "pattern"))
                }
            }
        }
    }

    fn parse_struct_destructor(&mut self) -> DiagnosticResult<Pattern> {
        let start_span = self.previous().span;
        let mut exhaustive = true;

        let symbols = parse_delimited_list!(
            self,
            CloseCurly,
            Comma,
            {
                if eat!(self, DotDot) {
                    expect!(self, CloseCurly, "}")?;
                    exhaustive = false;
                    break;
                }

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

        let destructor = DestructorPattern {
            symbols,
            exhaustive,
            span: start_span.to(self.previous_span()),
        };

        Ok(Pattern::StructDestructor(destructor))
    }

    fn parse_tuple_destructor(&mut self) -> DiagnosticResult<Pattern> {
        let start_span = self.previous().span;
        let mut exhaustive = true;

        let symbols = parse_delimited_list!(
            self,
            CloseParen,
            Comma,
            {
                if eat!(self, DotDot) {
                    expect!(self, CloseParen, ")")?;
                    exhaustive = false;
                    break;
                }

                self.parse_symbol_pattern()?
            },
            ", or )"
        );

        let destructor = DestructorPattern {
            symbols,
            exhaustive,
            span: start_span.to(self.previous_span()),
        };

        Ok(Pattern::TupleDestructor(destructor))
    }

    pub(super) fn parse_symbol_pattern(
        &mut self,
    ) -> DiagnosticResult<SymbolPattern> {
        let is_mutable = eat!(self, Mut);

        let (symbol, ignore) = if eat!(self, Id(_)) {
            (self.previous().symbol(), false)
        } else if eat!(self, Placeholder) {
            (ustr(""), true)
        } else {
            return Err(SyntaxError::expected(self.span(), "identifier or _"));
        };

        Ok(SymbolPattern {
            symbol,
            alias: None,
            is_mutable,
            span: self.previous_span(),
            ignore,
        })
    }
}
