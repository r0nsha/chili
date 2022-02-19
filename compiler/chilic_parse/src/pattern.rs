use crate::*;
use chilic_error::SyntaxError;
use chilic_ir::pattern::{DestructorPattern, Pattern, SymbolPattern};

impl Parser {
    pub(crate) fn parse_pattern(&mut self) -> DiagnosticResult<Pattern> {
        match self.parse_symbol_pattern() {
            Ok(symbol) => Ok(Pattern::Single(symbol)),
            Err(_) => {
                if match_token!(self, OpenCurly) {
                    self.parse_struct_destructor()
                } else if match_token!(self, OpenParen) {
                    self.parse_tuple_destructor()
                } else {
                    Err(SyntaxError::expected(&self.peek().span, "pattern"))
                }
            }
        }
    }

    fn parse_struct_destructor(&mut self) -> DiagnosticResult<Pattern> {
        let start_span = self.previous().span.clone();
        let mut exhaustive = true;

        let symbols = parse_delimited_list!(
            self,
            CloseCurly,
            Comma,
            {
                if match_token!(self, DotDot) {
                    require!(self, CloseCurly, "}")?;
                    exhaustive = false;
                    break;
                }

                let mut symbol_pattern = self.parse_symbol_pattern()?;

                if match_token!(self, Colon) {
                    let id_token = require!(self, Id(_), "identifier")?;
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
            span: Span::merge(&start_span, self.previous_span_ref()),
        };

        Ok(Pattern::StructDestructor(destructor))
    }

    fn parse_tuple_destructor(&mut self) -> DiagnosticResult<Pattern> {
        let start_span = self.previous().span.clone();
        let mut exhaustive = true;

        let symbols = parse_delimited_list!(
            self,
            CloseParen,
            Comma,
            {
                if match_token!(self, DotDot) {
                    require!(self, CloseParen, ")")?;
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
            span: Span::merge(&start_span, self.previous_span_ref()),
        };

        Ok(Pattern::TupleDestructor(destructor))
    }

    pub(super) fn parse_symbol_pattern(
        &mut self,
    ) -> DiagnosticResult<SymbolPattern> {
        let is_mutable = match_token!(self, Mut);

        let (symbol, ignore) = if match_token!(self, Id(_)) {
            (self.previous().symbol(), false)
        } else if match_token!(self, Placeholder) {
            (ustr(""), true)
        } else {
            return Err(SyntaxError::expected(
                self.span_ref(),
                "identifier or _",
            ));
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
