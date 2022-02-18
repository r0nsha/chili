use crate::*;
use chilic_error::SyntaxError;
use chilic_ir::pattern::{DestructorPattern, Pattern, SymbolPattern};
use chilic_token::TokenType::*;

impl Parser {
    pub(crate) fn parse_pattern(&mut self) -> DiagnosticResult<Pattern> {
        match self.parse_symbol_pattern() {
            Ok(symbol) => Ok(Pattern::Single(symbol)),
            Err(_) => {
                if self.match_one(OpenCurly) {
                    self.parse_struct_destructor()
                } else if self.match_one(OpenParen) {
                    self.parse_tuple_destructor()
                } else {
                    Err(SyntaxError::expected(&self.peek().span, "pattern"))
                }
            }
        }
    }

    fn parse_struct_destructor(&mut self) -> DiagnosticResult<Pattern> {
        Ok(Pattern::StructDestructor(
            self.parse_destructor_pattern_internal(CloseCurly, true)?,
        ))
    }

    fn parse_tuple_destructor(&mut self) -> DiagnosticResult<Pattern> {
        Ok(Pattern::TupleDestructor(
            self.parse_destructor_pattern_internal(CloseParen, false)?,
        ))
    }

    fn parse_destructor_pattern_internal(
        &mut self,
        end_token: TokenType,
        allow_aliasing: bool,
    ) -> DiagnosticResult<DestructorPattern> {
        let start_span = self.previous().span.clone();
        let mut symbols = vec![];
        let mut exhaustive = true;

        while !self.match_one(end_token.clone()) && !self.is_end() {
            if self.match_one(DotDot) {
                self.consume(end_token)?;
                exhaustive = false;
                break;
            }

            let mut symbol_pattern = self.parse_symbol_pattern()?;

            if allow_aliasing && self.match_one(Colon) {
                let id_token = self.consume_id()?;
                let symbol = id_token.symbol();
                symbol_pattern.alias = Some(symbol);
            }

            symbols.push(symbol_pattern);

            if self.match_one(Comma) {
                continue;
            } else if self.match_one(end_token.clone()) {
                break;
            } else {
                return Err(SyntaxError::expected(
                    self.span_ref(),
                    &format!(", or {}", end_token.lexeme()),
                ));
            }
        }

        Ok(DestructorPattern {
            symbols,
            exhaustive,
            span: Span::merge(&start_span, self.previous_span_ref()),
        })
    }

    pub(super) fn parse_symbol_pattern(&mut self) -> DiagnosticResult<SymbolPattern> {
        let is_mutable = self.match_one(Mut);

        let (symbol, ignore) = if self.match_id() {
            (self.previous().symbol(), false)
        } else if self.match_one(Placeholder) {
            (ustr(""), true)
        } else {
            return Err(SyntaxError::expected(self.span_ref(), "identifier or _"));
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
