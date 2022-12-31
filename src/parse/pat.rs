use super::*;
use crate::{
    ast::pat::{GlobPat, HybridPat, NamePat, Pat, UnpackPat, UnpackSubPat},
    error::SyntaxError,
    workspace::BindingId,
};

impl Parser {
    pub fn parse_pat(&mut self) -> DiagnosticResult<Pat> {
        self.skip_newlines();

        if eat!(self, Placeholder) {
            Ok(Pat::Ignore(self.previous_span()))
        } else if is!(self, Mut | Ident(_)) {
            let pat = self.parse_name_pat().map(Pat::Name)?;

            self.skip_newlines();

            if eat!(self, At) {
                let pat = pat.into_name().unwrap();
                let start_span = pat.span;

                self.skip_newlines();

                let unpack_pat = self.parse_unpack_pat()?;

                Ok(Pat::Hybrid(HybridPat {
                    name_pat: pat,
                    unpack_pat,
                    span: start_span.to(self.previous_span()),
                }))
            } else {
                Ok(pat)
            }
        } else if eat!(self, OpenParen) {
            let unpack_pat = self.parse_unpack_pat()?;
            Ok(Pat::Unpack(unpack_pat))
        } else {
            Err(SyntaxError::expected(self.span(), "a pattern"))
        }
    }

    fn parse_unpack_pat(&mut self) -> DiagnosticResult<UnpackPat> {
        let start_span = self.previous_span();

        let mut subpats = vec![];
        let mut glob: Option<GlobPat> = None;

        while !eat!(self, CloseParen) && !self.eof() {
            self.skip_newlines();

            if eat!(self, Star) {
                glob = Some(GlobPat {
                    span: self.previous_span(),
                });
                require!(self, CloseParen, ")")?;
                break;
            } else {
                self.skip_newlines();

                let pat = self.parse_pat()?;

                let (alias_pat, span) = if eat!(self, Colon) {
                    self.skip_newlines();
                    let alias_pat = self.parse_pat()?;
                    let span = pat.span().to(alias_pat.span());
                    (Some(alias_pat), span)
                } else {
                    (None, pat.span())
                };

                subpats.push(UnpackSubPat { pat, alias_pat, span });

                self.skip_newlines();

                if eat!(self, Comma) {
                    self.skip_newlines();
                    continue;
                } else if eat!(self, CloseParen) {
                    break;
                } else {
                    let span = self.previous_span().after();
                    return Err(SyntaxError::expected(span, ", or )"));
                }
            }
        }

        Ok(UnpackPat {
            subpats,
            span: start_span.to(self.previous_span()),
            glob,
        })
    }

    pub fn parse_name_pat(&mut self) -> DiagnosticResult<NamePat> {
        let is_mutable = eat!(self, Mut);
        let id_token = require!(self, Ident(_), "an identifier")?;

        Ok(NamePat {
            id: BindingId::unknown(),
            name: id_token.name(),
            is_mutable,
            span: id_token.span,
        })
    }
}
