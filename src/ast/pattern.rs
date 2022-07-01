use crate::{span::Span, workspace::BindingId};
use std::fmt::Display;
use ustr::Ustr;

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Name(NamePattern),
    StructUnpack(UnpackPattern),
    TupleUnpack(UnpackPattern),
    Hybrid(HybridPattern),
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnpackPattern {
    pub symbols: Vec<NamePattern>,
    pub span: Span,
    pub wildcard_symbol: Option<Span>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnpackPatternKind {
    Struct(UnpackPattern),
    Tuple(UnpackPattern),
}

impl UnpackPatternKind {
    pub fn as_inner(&self) -> &UnpackPattern {
        match self {
            UnpackPatternKind::Struct(p) | UnpackPatternKind::Tuple(p) => p,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct HybridPattern {
    pub name_pattern: NamePattern,
    pub unpack_pattern: UnpackPatternKind,
    pub span: Span,
}

impl Pattern {
    pub fn is_name(&self) -> bool {
        matches!(self, Pattern::Name(_))
    }

    pub fn as_name(&self) -> &NamePattern {
        match self {
            Pattern::Name(s) => s,
            _ => panic!("expected Symbol, got {}", self),
        }
    }

    pub fn as_name_mut(&mut self) -> &mut NamePattern {
        match self {
            Pattern::Name(s) => s,
            _ => panic!("expected Symbol, got {}", self),
        }
    }

    pub fn into_name(self) -> NamePattern {
        match self {
            Pattern::Name(s) => s,
            _ => panic!("expected Symbol, got {}", self),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Pattern::Name(p) => p.span,
            Pattern::StructUnpack(p) | Pattern::TupleUnpack(p) => p.span,
            Pattern::Hybrid(p) => p.span,
        }
    }

    pub fn iter(&self) -> PatternIter {
        PatternIter {
            pattern: self,
            pos: 0,
        }
    }

    pub fn ids(&self) -> Vec<BindingId> {
        self.iter().map(|p| p.id).collect::<Vec<BindingId>>()
    }

    pub fn count(&self) -> usize {
        match self {
            Pattern::Name(_) => 1,
            Pattern::StructUnpack(p) | Pattern::TupleUnpack(p) => p.symbols.len(),
            Pattern::Hybrid(p) => {
                let unpack_pattern = match &p.unpack_pattern {
                    UnpackPatternKind::Struct(p) => p,
                    UnpackPatternKind::Tuple(p) => p,
                };

                1 + unpack_pattern.symbols.len()
            }
        }
    }
}

pub struct PatternIter<'a> {
    pattern: &'a Pattern,
    pos: usize,
}

impl<'a> Iterator for PatternIter<'a> {
    type Item = &'a NamePattern;

    fn next(&mut self) -> Option<Self::Item> {
        let item = match &self.pattern {
            Pattern::Name(pattern) => match self.pos {
                0 => Some(pattern),
                _ => None,
            },
            Pattern::StructUnpack(pattern) | Pattern::TupleUnpack(pattern) => {
                pattern.symbols.get(self.pos)
            }
            Pattern::Hybrid(pattern) => match self.pos {
                0 => Some(&pattern.name_pattern),
                _ => pattern.unpack_pattern.as_inner().symbols.get(self.pos - 1),
            },
        };

        self.pos += 1;

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
                Pattern::StructUnpack(pattern) => format!("{{ {} }}", pattern),
                Pattern::TupleUnpack(pattern) => format!("({})", pattern),
                Pattern::Hybrid(pattern) => format!(
                    "{} @ {}",
                    pattern.name_pattern,
                    match &pattern.unpack_pattern {
                        UnpackPatternKind::Struct(pattern) => format!("{{ {} }}", pattern),
                        UnpackPatternKind::Tuple(pattern) => format!("({})", pattern),
                    }
                ),
            }
        )
    }
}

impl Display for UnpackPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.symbols
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

// bitflags! {
//     pub struct PatternFlags : u8 {
//         const IS_MUTABLE = 1 << 0;
//         const IGNORE = 1 << 1;
//     }
// }

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
        write!(
            f,
            "{}{}",
            if self.is_mutable { "mut " } else { "" },
            self.alias()
        )
    }
}

impl NamePattern {
    pub fn alias(&self) -> Ustr {
        self.alias.unwrap_or(self.name)
    }
}
