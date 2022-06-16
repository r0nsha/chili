use crate::workspace::BindingInfoId;
use chili_span::Span;
use std::fmt::Display;
use ustr::Ustr;

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Symbol(SymbolPattern),
    StructUnpack(UnpackPattern),
    TupleUnpack(UnpackPattern),
    Hybrid(HybridPattern),
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnpackPattern {
    pub symbols: Vec<SymbolPattern>,
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
    pub symbol: SymbolPattern,
    pub unpack: UnpackPatternKind,
    pub span: Span,
}

impl Pattern {
    pub fn is_symbol(&self) -> bool {
        matches!(self, Pattern::Symbol(_))
    }

    pub fn as_symbol_ref(&self) -> &SymbolPattern {
        match self {
            Pattern::Symbol(s) => s,
            _ => panic!("expected Symbol, got {}", self),
        }
    }

    pub fn into_symbol(self) -> SymbolPattern {
        match self {
            Pattern::Symbol(s) => s,
            _ => panic!("expected Symbol, got {}", self),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Pattern::Symbol(p) => p.span,
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

    pub fn ids(&self) -> Vec<BindingInfoId> {
        self.iter().map(|p| p.id).collect::<Vec<BindingInfoId>>()
    }
}

pub struct PatternIter<'a> {
    pattern: &'a Pattern,
    pos: usize,
}

impl<'a> Iterator for PatternIter<'a> {
    type Item = &'a SymbolPattern;

    fn next(&mut self) -> Option<Self::Item> {
        let item = match &self.pattern {
            Pattern::Symbol(pat) => match self.pos {
                0 => Some(pat),
                _ => None,
            },
            Pattern::StructUnpack(pat) | Pattern::TupleUnpack(pat) => pat.symbols.get(self.pos),
            Pattern::Hybrid(pat) => match self.pos {
                0 => Some(&pat.symbol),
                _ => pat.unpack.as_inner().symbols.get(self.pos - 1),
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
                Pattern::Symbol(pat) => pat.to_string(),
                Pattern::StructUnpack(pat) => format!("{{ {} }}", pat),
                Pattern::TupleUnpack(pat) => format!("({})", pat),
                Pattern::Hybrid(pat) => format!(
                    "{} @ {}",
                    pat.symbol,
                    match &pat.unpack {
                        UnpackPatternKind::Struct(pat) => format!("{{ {} }}", pat),
                        UnpackPatternKind::Tuple(pat) => format!("({})", pat),
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
pub struct SymbolPattern {
    pub id: BindingInfoId,
    pub symbol: Ustr,
    pub alias: Option<Ustr>,
    pub span: Span,
    pub is_mutable: bool,
    pub ignore: bool,
}

impl Display for SymbolPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            if self.is_mutable { "mut " } else { "" },
            self.symbol
        )
    }
}
