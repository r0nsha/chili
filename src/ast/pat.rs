use crate::{span::Span, workspace::BindingId};
use enum_as_inner::EnumAsInner;
use std::fmt::Display;
use ustr::Ustr;

#[derive(Debug, PartialEq, Clone, EnumAsInner)]
pub enum Pat {
    Ignore(Span),
    Name(NamePat),
    Unpack(UnpackPat),
    Hybrid(HybridPat),
}

impl Pat {
    pub fn span(&self) -> Span {
        match self {
            Pat::Ignore(span) => *span,
            Pat::Name(p) => p.span,
            Pat::Unpack(p) => p.span,
            Pat::Hybrid(p) => p.span,
        }
    }

    pub fn iter(&self) -> PatIter {
        PatIter {
            pats: vec![self],
            positions: vec![0],
        }
    }

    pub fn is_mutable(&self) -> bool {
        self.iter().any(|p| p.is_mutable)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnpackPat {
    pub subpats: Vec<UnpackSubPat>,
    pub span: Span,
    pub glob: Option<GlobPat>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnpackSubPat {
    pat: Pat,
    alias_pat: Option<Pat>,
    span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct GlobPat {
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct HybridPat {
    pub name_pat: NamePat,
    pub unpack_pat: UnpackPat,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct NamePat {
    pub id: BindingId,
    pub name: Ustr,
    pub span: Span,
    pub is_mutable: bool,
}

pub struct PatIter<'a> {
    pats: Vec<&'a Pat>,
    positions: Vec<usize>,
}

impl<'a> PatIter<'a> {
    fn push(&mut self, pat: &'a Pat) {
        self.pats.push(pat);
        self.positions.push(0);
    }

    fn pop(&mut self) {
        self.pats.pop();
        self.positions.pop();
    }

    fn handle_unpack(&mut self, pat: &'a UnpackPat, pos: usize) -> Option<<Self as Iterator>::Item> {
        match pat.subpats.get(pos) {
            Some(subpat) => match &subpat.alias_pat {
                Some(alias_pat) => {
                    self.push(alias_pat);
                    self.next()
                }
                None => Some(pat),
            },
            None => None,
        }
    }
}

impl<'a> Iterator for PatIter<'a> {
    type Item = &'a NamePat;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pats.is_empty() {
            return None;
        }

        let index = self.pats.len() - 1;

        let pat = &self.pats[index];
        let pos = self.positions[index];

        let item = match pat {
            Pat::Ignore(_) => None,
            Pat::Name(pat) => match pos {
                0 => Some(pat),
                _ => None,
            },
            Pat::Unpack(pat) => self.handle_struct_unpack(pat, pos),
            Pat::Tuple(pat) => self.handle_tuple_unpack(pat, pos),
            Pat::Hybrid(pat) => match pos {
                0 => Some(&pat.name_pat),
                _ => self.handle_unpack(&pat.unpack_pat, pos),
            },
        };

        self.positions[index] += 1;

        item
    }
}

impl Display for Pat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Pat::Ignore(_) => "_".to_string(),
                Pat::Name(pat) => pat.to_string(),
                Pat::Unpack(pat) => pat.to_string(),
                Pat::Hybrid(pat) => format!("{} @ {}", pat.name_pat, pat.unpack_pat.to_string()),
            }
        )
    }
}

impl Display for NamePat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_mutable {
            f.write_str("mut ")?;
        }
        f.write_str(&self.name)
    }
}

impl Display for UnpackPat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{ {} }}",
            self.subpats
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl Display for UnpackSubPat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnpackSubPat::Name(pat) => write!(f, "{}", pat),
            UnpackSubPat::NameAndPat(name, pat) => write!(f, "{}: {}", name.name, pat),
        }
    }
}
