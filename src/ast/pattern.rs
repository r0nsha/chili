use crate::{span::Span, workspace::BindingId};
use enum_as_inner::EnumAsInner;
use std::fmt::Display;
use ustr::Ustr;

use super::NameAndSpan;

#[derive(Debug, PartialEq, Clone, EnumAsInner)]
pub enum Pattern {
    Name(NamePattern),
    StructUnpack(StructUnpackPattern),
    TupleUnpack(TupleUnpackPattern),
    Hybrid(HybridPattern),
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

    pub fn is_mutable(&self) -> bool {
        self.iter().any(|p| p.is_mutable)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructUnpackPattern {
    pub sub_patterns: Vec<StructUnpackSubPattern>,
    pub span: Span,
    pub glob: Option<GlobPat>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum StructUnpackSubPattern {
    Name(NamePattern),
    NameAndPattern(NameAndSpan, Pattern),
}

impl StructUnpackSubPattern {
    pub fn name(&self) -> Ustr {
        match self {
            StructUnpackSubPattern::Name(pattern) => pattern.name,
            StructUnpackSubPattern::NameAndPattern(name, _) => name.name,
        }
    }

    pub fn is_mutable(&self) -> bool {
        match self {
            StructUnpackSubPattern::Name(pattern) => pattern.is_mutable,
            StructUnpackSubPattern::NameAndPattern(_, _) => false,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            StructUnpackSubPattern::Name(pattern) => pattern.span,
            StructUnpackSubPattern::NameAndPattern(name, _) => name.span,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TupleUnpackPattern {
    pub sub_patterns: Vec<Pattern>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct GlobPat {
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

#[derive(Debug, PartialEq, Clone)]
pub struct NamePattern {
    pub id: BindingId,
    pub name: Ustr,
    pub span: Span,
    pub is_mutable: bool,
    pub ignore: bool,
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

    fn handle_struct_unpack(
        &mut self,
        pattern: &'a StructUnpackPattern,
        pos: usize,
    ) -> Option<<Self as Iterator>::Item> {
        match pattern.sub_patterns.get(pos) {
            Some(pattern) => match pattern {
                StructUnpackSubPattern::Name(pattern) => Some(pattern),
                StructUnpackSubPattern::NameAndPattern(_, pattern) => {
                    self.push(pattern);
                    self.next()
                }
            },
            None => None,
        }
    }

    fn handle_tuple_unpack(&mut self, pattern: &'a TupleUnpackPattern, pos: usize) -> Option<<Self as Iterator>::Item> {
        match pattern.sub_patterns.get(pos) {
            Some(pattern) => self.push(pattern),
            None => self.pop(),
        }

        self.next()
    }
}

impl<'a> Iterator for PatternIter<'a> {
    type Item = &'a NamePattern;

    fn next(&mut self) -> Option<Self::Item> {
        if self.patterns.is_empty() {
            return None;
        }

        let index = self.patterns.len() - 1;

        let pattern = &self.patterns[index];
        let pos = self.positions[index];

        let item = match pattern {
            Pattern::Name(pattern) => match pos {
                0 => Some(pattern),
                _ => None,
            },
            Pattern::StructUnpack(pattern) => self.handle_struct_unpack(pattern, pos),
            Pattern::TupleUnpack(pattern) => self.handle_tuple_unpack(pattern, pos),
            Pattern::Hybrid(pattern) => match pos {
                0 => Some(&pattern.name_pattern),
                _ => match &pattern.unpack_pattern {
                    UnpackPatternKind::Struct(pattern) => self.handle_struct_unpack(pattern, pos),
                    UnpackPatternKind::Tuple(pattern) => self.handle_tuple_unpack(pattern, pos),
                },
            },
        };

        self.positions[index] += 1;

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

impl Display for StructUnpackSubPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StructUnpackSubPattern::Name(pattern) => write!(f, "{}", pattern),
            StructUnpackSubPattern::NameAndPattern(name, pattern) => write!(f, "{}: {}", name.name, pattern),
        }
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

impl Display for NamePattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.ignore {
            write!(f, "_")
        } else {
            write!(f, "{}{}", if self.is_mutable { "mut " } else { "" }, self.name)
        }
    }
}

// pub trait PatternVisitor<T> {
//     fn visit_pattern(&mut self, pattern: &Pattern) -> ControlFlow<T> {
//         match pattern {
//             Pattern::Name(pattern) => self.visit_name_pattern(pattern),
//             Pattern::StructUnpack(pattern) => self.visit_struct_unpack_pattern(pattern),
//             Pattern::TupleUnpack(pattern) => self.visit_tuple_unpack_pattern(pattern),
//             Pattern::Hybrid(pattern) => {
//                 self.visit_name_pattern(&pattern.name_pattern)?;
//                 match &pattern.unpack_pattern {
//                     UnpackPatternKind::Struct(pattern) => self.visit_struct_unpack_pattern(pattern),
//                     UnpackPatternKind::Tuple(pattern) => self.visit_tuple_unpack_pattern(pattern),
//                 }
//             }
//         }
//     }

//     fn visit_struct_unpack_pattern(&mut self, pattern: &StructUnpackPattern) -> ControlFlow<T> {
//         pattern.sub_patterns.iter().try_for_each(|pattern| match pattern {
//             StructUnpackSubPattern::Name(pattern) => self.visit_name_pattern(pattern),
//             StructUnpackSubPattern::NameAndPattern(_, pattern) => self.visit_pattern(pattern),
//         })
//     }

//     fn visit_tuple_unpack_pattern(&mut self, pattern: &TupleUnpackPattern) -> ControlFlow<T> {
//         pattern
//             .sub_patterns
//             .iter()
//             .try_for_each(|pattern| self.visit_pattern(pattern))
//     }

//     fn visit_name_pattern(&mut self, pattern: &NamePattern) -> ControlFlow<T>;
// }

// struct MutablePatternVisitor;

// impl PatternVisitor<bool> for MutablePatternVisitor {
//     fn visit_name_pattern(&mut self, pattern: &NamePattern) -> ControlFlow<bool> {
//         if pattern.is_mutable {
//             ControlFlow::Break(true)
//         } else {
//             ControlFlow::Continue(())
//         }
//     }
// }
