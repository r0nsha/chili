use super::*;
use crate::{
    ast::pattern::{
        GlobPat, HybridPattern, NamePattern, Pattern, StructUnpackPattern, StructUnpackSubPattern, TupleUnpackPattern,
        UnpackPatternKind,
    },
    error::SyntaxError,
    workspace::BindingId,
};

impl Parser {
    pub fn parse_pattern(&mut self) -> DiagnosticResult<Pattern> {
        self.skip_newlines();

        if is!(self, Mut | Ident(_) | Placeholder) {
            let pattern = self.parse_name_pattern().map(Pattern::Name)?;

            self.skip_newlines();

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
        self.skip_newlines();

        if eat!(self, OpenCurly) {
            self.parse_struct_unpack()
        } else if eat!(self, OpenParen) {
            self.parse_tuple_unpack()
        } else if eat!(self, Star) {
            let span = self.previous_span();
            Ok(Pattern::StructUnpack(StructUnpackPattern {
                sub_patterns: vec![],
                span,
                glob: Some(GlobPat { span }),
            }))
        } else {
            Err(SyntaxError::expected(self.span(), expectation))
        }
    }

    fn parse_struct_unpack(&mut self) -> DiagnosticResult<Pattern> {
        let start_span = self.previous_span();

        let mut sub_patterns = vec![];
        let mut glob_span: Option<Span> = None;

        while !eat!(self, CloseCurly) && !self.eof() {
            self.skip_newlines();

            if eat!(self, Star) {
                glob_span = Some(self.previous().span);
                require!(self, CloseCurly, "}")?;
                break;
            } else {
                fn parse_name(
                    parser: &mut Parser,
                    sub_patterns: &mut Vec<StructUnpackSubPattern>,
                ) -> DiagnosticResult<()> {
                    let pattern = parser.parse_name_pattern()?;

                    // This means the user used `_`, instead of `x: _` - which is illegal
                    if pattern.ignore {
                        return Err(SyntaxError::expected(pattern.span, "an identifier, ? or }"));
                    }

                    sub_patterns.push(StructUnpackSubPattern::Name(pattern));

                    Ok(())
                }

                self.skip_newlines();

                if eat!(self, Ident(_)) {
                    if is!(self, Colon) {
                        let ident = self.previous();
                        let name = ast::NameAndSpan::new(ident.name(), ident.span);

                        self.bump();

                        let pattern = self.parse_pattern()?;

                        sub_patterns.push(StructUnpackSubPattern::NameAndPattern(name, pattern));
                    } else {
                        self.revert(1);
                        parse_name(self, &mut sub_patterns)?;
                    }
                } else {
                    parse_name(self, &mut sub_patterns)?;
                }

                self.skip_newlines();

                if eat!(self, Comma) {
                    self.skip_newlines();
                    continue;
                } else if eat!(self, CloseCurly) {
                    break;
                } else {
                    let span = self.previous_span().after();
                    return Err(SyntaxError::expected(span, ", or }"));
                }
            }
        }

        Ok(Pattern::StructUnpack(StructUnpackPattern {
            sub_patterns,
            span: start_span.to(self.previous_span()),
            glob: glob_span.map(|span| GlobPat { span }),
        }))
    }

    fn parse_tuple_unpack(&mut self) -> DiagnosticResult<Pattern> {
        let start_span = self.previous_span();

        let sub_patterns = parse_delimited_list!(self, CloseParen, Comma, self.parse_pattern()?, ", or )");

        Ok(Pattern::TupleUnpack(TupleUnpackPattern {
            sub_patterns,
            span: start_span.to(self.previous_span()),
        }))
    }

    pub fn parse_name_pattern(&mut self) -> DiagnosticResult<NamePattern> {
        let is_mutable = eat!(self, Mut);

        let (name, ignore) = if eat!(self, Ident(_)) {
            (self.previous().name(), false)
        } else if eat!(self, Placeholder) {
            (ustr(Placeholder.lexeme()), true)
        } else {
            return Err(SyntaxError::expected(self.span(), "an identifier or _"));
        };

        Ok(NamePattern {
            id: BindingId::unknown(),
            name,
            is_mutable,
            span: self.previous_span(),
            ignore,
        })
    }
}
