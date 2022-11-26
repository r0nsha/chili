use crate::{span::Span, workspace::BindingId};
use enum_as_inner::EnumAsInner;
use std::fmt::Display;
use ustr::Ustr;

use super::NameAndSpan;

#[derive(Debug, PartialEq, Clone, EnumAsInner)]
pub enum Pat {
    Ignore(Span),
    Name(NamePat),
    Struct(StructPat),
    Tuple(TuplePat),
    Hybrid(HybridPat),
}

impl Pat {
    pub fn span(&self) -> Span {
        match self {
            Pat::Ignore(span) => *span,
            Pat::Name(p) => p.span,
            Pat::Struct(p) => p.span,
            Pat::Tuple(p) => p.span,
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
pub struct StructPat {
    pub subpats: Vec<StructSubPat>,
    pub span: Span,
    pub glob: Option<GlobPat>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum StructSubPat {
    Name(NamePat),
    NameAndPat(NameAndSpan, Pat),
}

impl StructSubPat {
    pub fn name(&self) -> Ustr {
        match self {
            StructSubPat::Name(pat) => pat.name,
            StructSubPat::NameAndPat(name, _) => name.name,
        }
    }

    pub fn is_mutable(&self) -> bool {
        match self {
            StructSubPat::Name(pat) => pat.is_mutable,
            StructSubPat::NameAndPat(_, _) => false,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            StructSubPat::Name(pat) => pat.span,
            StructSubPat::NameAndPat(name, _) => name.span,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TuplePat {
    pub subpats: Vec<Pat>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct GlobPat {
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnpackPatKind {
    Struct(StructPat),
    Tuple(TuplePat),
}

#[derive(Debug, PartialEq, Clone)]
pub struct HybridPat {
    pub name_pat: NamePat,
    pub unpack_pat: UnpackPatKind,
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

    fn handle_struct_unpack(&mut self, pat: &'a StructPat, pos: usize) -> Option<<Self as Iterator>::Item> {
        match pat.subpats.get(pos) {
            Some(subpat) => match subpat {
                StructSubPat::Name(pat) => Some(pat),
                StructSubPat::NameAndPat(_, pat) => {
                    self.push(pat);
                    self.next()
                }
            },
            None => None,
        }
    }

    fn handle_tuple_unpack(&mut self, pat: &'a TuplePat, pos: usize) -> Option<<Self as Iterator>::Item> {
        match pat.subpats.get(pos) {
            Some(pat) => self.push(pat),
            None => self.pop(),
        }

        self.next()
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
            Pat::Struct(pat) => self.handle_struct_unpack(pat, pos),
            Pat::Tuple(pat) => self.handle_tuple_unpack(pat, pos),
            Pat::Hybrid(pat) => match pos {
                0 => Some(&pat.name_pat),
                _ => match &pat.unpack_pat {
                    UnpackPatKind::Struct(pat) => self.handle_struct_unpack(pat, pos),
                    UnpackPatKind::Tuple(pat) => self.handle_tuple_unpack(pat, pos),
                },
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
                Pat::Struct(pat) => pat.to_string(),
                Pat::Tuple(pat) => pat.to_string(),
                Pat::Hybrid(pat) => format!(
                    "{} @ {}",
                    pat.name_pat,
                    match &pat.unpack_pat {
                        UnpackPatKind::Struct(pat) => pat.to_string(),
                        UnpackPatKind::Tuple(pat) => pat.to_string(),
                    }
                ),
            }
        )
    }
}

impl Display for StructPat {
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

impl Display for StructSubPat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StructSubPat::Name(pat) => write!(f, "{}", pat),
            StructSubPat::NameAndPat(name, pat) => write!(f, "{}: {}", name.name, pat),
        }
    }
}

impl Display for TuplePat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({})",
            self.subpats
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join(", ")
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
