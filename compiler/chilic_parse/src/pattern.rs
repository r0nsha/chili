use crate::*;
use chilic_error::SyntaxError;
use chilic_ir::pattern::{DestructorPattern, Pattern, SymbolPattern};

impl Parser {
    pub(crate) fn parse_pattern(&mut self) -> DiagnosticResult<Pattern> {
        match self.parse_symbol_pattern() {
            Ok(symbol) => Ok(Pattern::Single(symbol)),
            Err(_) => {
                if mat!(self, OpenCurly) {
                    self.parse_struct_destructor()
                } else if mat!(self, OpenParen) {
                    self.parse_tuple_destructor()
                } else {
                    Err(SyntaxError::expected(&self.peek().span, "pattern"))
                }
            }
        }
    }

    fn parse_struct_destructor(&mut self) -> DiagnosticResult<Pattern> {
        let start_span = self.previous().span.clone();
        let mut symbols = vec![];
        let mut exhaustive = true;

        while !mat!(self, CloseCurly) && !self.is_end() {
            if mat!(self, DotDot) {
                req!(self, CloseCurly, "}")?;
                exhaustive = false;
                break;
            }

            let mut symbol_pattern = self.parse_symbol_pattern()?;

            if mat!(self, Colon) {
                let id_token = req!(self, Id(_), "identifier")?;
                let symbol = id_token.symbol();
                symbol_pattern.alias = Some(symbol);
            }

            symbols.push(symbol_pattern);

            if mat!(self, Comma) {
                continue;
            } else if mat!(self, CloseCurly) {
                break;
            } else {
                return Err(SyntaxError::expected(self.span_ref(), ", or }"));
            }
        }

        let destructor = DestructorPattern {
            symbols,
            exhaustive,
            span: Span::merge(&start_span, self.previous_span_ref()),
        };

        Ok(Pattern::StructDestructor(destructor))
    }

    fn parse_tuple_destructor(&mut self) -> DiagnosticResult<Pattern> {
        let start_span = self.previous().span.clone();
        let mut symbols = vec![];
        let mut exhaustive = true;

        while !mat!(self, CloseParen) && !self.is_end() {
            if mat!(self, DotDot) {
                req!(self, CloseParen, ")")?;
                exhaustive = false;
                break;
            }

            let symbol_pattern = self.parse_symbol_pattern()?;
            symbols.push(symbol_pattern);

            if mat!(self, Comma) {
                continue;
            } else if mat!(self, CloseParen) {
                break;
            } else {
                return Err(SyntaxError::expected(self.span_ref(), ", or )"));
            }
        }

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
        let is_mutable = mat!(self, Mut);

        let (symbol, ignore) = if mat!(self, Id(_)) {
            (self.previous().symbol(), false)
        } else if mat!(self, Placeholder) {
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
