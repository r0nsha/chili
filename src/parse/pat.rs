use super::*;
use crate::{
    ast::pat::{GlobPat, HybridPat, NamePat, Pat, StructPat, StructSubPat, TuplePat, UnpackPatKind},
    error::SyntaxError,
    workspace::BindingId,
};

impl Parser {
    pub fn parse_pat(&mut self) -> DiagnosticResult<Pat> {
        self.skip_newlines();

        if is!(self, Mut | Ident(_) | Placeholder) {
            let pat = self.parse_name_pat().map(Pat::Name)?;

            self.skip_newlines();

            if eat!(self, At) {
                let pat = pat.into_name().unwrap();
                let start_span = pat.span;

                let unpack_pat = match self.parse_unpack_pat("an unpack pattern")? {
                    Pat::Struct(pat) => UnpackPatKind::Struct(pat),
                    Pat::Tuple(pat) => UnpackPatKind::Tuple(pat),
                    _ => panic!(),
                };

                Ok(Pat::Hybrid(HybridPat {
                    name_pat: pat,
                    unpack_pat,
                    span: start_span.to(self.previous_span()),
                }))
            } else {
                Ok(pat)
            }
        } else {
            self.parse_unpack_pat("a pattern")
        }
    }

    fn parse_unpack_pat(&mut self, expectation: &str) -> DiagnosticResult<Pat> {
        self.skip_newlines();

        if eat!(self, OpenCurly) {
            self.parse_struct_unpack()
        } else if eat!(self, OpenParen) {
            self.parse_tuple_unpack()
        } else {
            Err(SyntaxError::expected(self.span(), expectation))
        }
    }

    fn parse_struct_unpack(&mut self) -> DiagnosticResult<Pat> {
        let start_span = self.previous_span();

        let mut subpats = vec![];
        let mut glob: Option<GlobPat> = None;

        while !eat!(self, CloseCurly) && !self.eof() {
            self.skip_newlines();

            if eat!(self, Star) {
                glob = Some(GlobPat {
                    span: self.previous_span(),
                });
                require!(self, CloseCurly, "}")?;
                break;
            } else {
                fn parse_name(parser: &mut Parser, subpats: &mut Vec<StructSubPat>) -> DiagnosticResult<()> {
                    let pat = parser.parse_name_pat()?;

                    // This means the user used `_`, instead of `x: _` - which is illegal
                    if pat.ignore {
                        return Err(SyntaxError::expected(pat.span, "an identifier, ? or }"));
                    }

                    subpats.push(StructSubPat::Name(pat));

                    Ok(())
                }

                self.skip_newlines();

                if eat!(self, Ident(_)) {
                    if is!(self, Colon) {
                        let ident = self.previous();
                        let name = ast::NameAndSpan::new(ident.name(), ident.span);

                        self.bump();

                        let pat = self.parse_pat()?;

                        subpats.push(StructSubPat::NameAndPat(name, pat));
                    } else {
                        self.revert(1);
                        parse_name(self, &mut subpats)?;
                    }
                } else {
                    parse_name(self, &mut subpats)?;
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

        Ok(Pat::Struct(StructPat {
            subpats,
            span: start_span.to(self.previous_span()),
            glob,
        }))
    }

    fn parse_tuple_unpack(&mut self) -> DiagnosticResult<Pat> {
        let start_span = self.previous_span();

        let subpats = parse_delimited_list!(self, CloseParen, Comma, self.parse_pat()?, ", or )");

        Ok(Pat::Tuple(TuplePat {
            subpats,
            span: start_span.to(self.previous_span()),
        }))
    }

    pub fn parse_name_pat(&mut self) -> DiagnosticResult<NamePat> {
        let is_mutable = eat!(self, Mut);

        let (name, ignore) = if eat!(self, Ident(_)) {
            (self.previous().name(), false)
        } else if eat!(self, Placeholder) {
            (ustr(Placeholder.lexeme()), true)
        } else {
            return Err(SyntaxError::expected(self.span(), "an identifier or _"));
        };

        Ok(NamePat {
            id: BindingId::unknown(),
            name,
            is_mutable,
            span: self.previous_span(),
            ignore,
        })
    }
}
