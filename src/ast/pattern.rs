use crate::{span::Span, workspace::BindingId};
use enum_as_inner::EnumAsInner;
use std::fmt::Display;
use ustr::Ustr;

#[derive(Debug, PartialEq, Clone, EnumAsInner)]
pub enum Pattern {
    Name(NamePattern),
    StructUnpack(StructUnpackPattern),
    TupleUnpack(TupleUnpackPattern),
    Hybrid(HybridPattern),
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructUnpackPattern {
    pub sub_patterns: Vec<NamePattern>,
    pub span: Span,
    pub wildcard: Option<Wildcard>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TupleUnpackPattern {
    pub sub_patterns: Vec<Pattern>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Wildcard {
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnpackPatternKind {
    Struct(StructUnpackPattern),
    Tuple(TupleUnpackPattern),
}

#[derive(Debug, PartialEq, Clone)]
pub struct HybridPattern {
    pub name_pattern: NamePattern,
    pub unpack_pattern: UnpackPatternKind,
    pub span: Span,
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Name(p) => p.span,
            Pattern::StructUnpack(p) => p.span,
            Pattern::TupleUnpack(p) => p.span,
            Pattern::Hybrid(p) => p.span,
        }
    }

    pub fn iter(&self) -> PatternIter {
        PatternIter {
            patterns: vec![self],
            positions: vec![0],
        }
    }

    #[allow(unused)]
    pub fn ids(&self) -> Vec<BindingId> {
        self.iter().map(|p| p.id).collect::<Vec<BindingId>>()
    }

    #[allow(unused)]
    pub fn count(&self) -> usize {
        match self {
            Pattern::Name(_) => 1,
            Pattern::StructUnpack(p) => p.sub_patterns.len(),
            Pattern::TupleUnpack(p) => p.sub_patterns.len(),
            Pattern::Hybrid(p) => match &p.unpack_pattern {
                UnpackPatternKind::Struct(p) => 1 + p.sub_patterns.len(),
                UnpackPatternKind::Tuple(p) => 1 + p.sub_patterns.len(),
            },
        }
    }

    pub fn is_mutable(&self) -> bool {
        self.iter().any(|p| p.is_mutable)
    }
}

pub struct PatternIter<'a> {
    patterns: Vec<&'a Pattern>,
    positions: Vec<usize>,
}

impl<'a> PatternIter<'a> {
    fn push(&mut self, pattern: &'a Pattern) {
        self.patterns.push(pattern);
        self.positions.push(0);
    }

    fn pop(&mut self) {
        self.patterns.pop();
        self.positions.pop();
    }

    fn handle_tuple_unpack(&mut self, pattern: &'a TupleUnpackPattern, pos: usize) -> Option<<Self as Iterator>::Item> {
        match pattern.sub_patterns.get(pos) {
            Some(pattern) => {
                *self.positions.last_mut().unwrap() += 1;
                self.push(pattern);
            }
            None => self.pop(),
        }

        self.next()
    }
}

impl<'a> Iterator for PatternIter<'a> {
    type Item = &'a NamePattern;

    fn next(&mut self) -> Option<Self::Item> {
        let pattern = self.patterns.last().unwrap();
        let pos = *self.positions.last_mut().unwrap();

        let item = match pattern {
            Pattern::Name(pattern) => match pos {
                0 => Some(pattern),
                _ => None,
            },
            Pattern::StructUnpack(pattern) => pattern.sub_patterns.get(pos),
            Pattern::TupleUnpack(pattern) => self.handle_tuple_unpack(pattern, pos),
            Pattern::Hybrid(pattern) => match pos {
                0 => Some(&pattern.name_pattern),
                _ => match &pattern.unpack_pattern {
                    UnpackPatternKind::Struct(pattern) => pattern.sub_patterns.get(pos - 1),
                    UnpackPatternKind::Tuple(pattern) => self.handle_tuple_unpack(pattern, pos),
                },
            },
        };

        *self.positions.last_mut().unwrap() += 1;

        item
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Pattern::Name(pattern) => pattern.to_string(),
                Pattern::StructUnpack(pattern) => pattern.to_string(),
                Pattern::TupleUnpack(pattern) => pattern.to_string(),
                Pattern::Hybrid(pattern) => format!(
                    "{} @ {}",
                    pattern.name_pattern,
                    match &pattern.unpack_pattern {
                        UnpackPatternKind::Struct(pattern) => pattern.to_string(),
                        UnpackPatternKind::Tuple(pattern) => pattern.to_string(),
                    }
                ),
            }
        )
    }
}

impl Display for StructUnpackPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{ {} }}",
            self.sub_patterns
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl Display for TupleUnpackPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({})",
            self.sub_patterns
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct NamePattern {
    pub id: BindingId,
    pub name: Ustr,
    pub alias: Option<Ustr>,
    pub span: Span,
    pub is_mutable: bool,
    pub ignore: bool,
}

impl Display for NamePattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", if self.is_mutable { "mut " } else { "" }, self.alias())
    }
}

impl NamePattern {
    pub fn alias(&self) -> Ustr {
        self.alias.unwrap_or(self.name)
    }
}
