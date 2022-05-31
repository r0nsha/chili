use crate::workspace::BindingInfoId;
use chili_span::Span;
use std::fmt::Display;
use ustr::Ustr;

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Single(SymbolPattern),
    StructUnpack(UnpackPattern),
    TupleUnpack(UnpackPattern),
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnpackPattern {
    pub symbols: Vec<SymbolPattern>,
    pub span: Span,
}

impl Pattern {
    pub fn is_single(&self) -> bool {
        matches!(self, Pattern::Single(_))
    }

    pub fn into_single(&self) -> SymbolPattern {
        match self {
            Pattern::Single(ps) => ps.clone(),
            _ => panic!("expected Single, got {}", self),
        }
    }

    pub fn as_single_ref(&self) -> &SymbolPattern {
        match self {
            Pattern::Single(ps) => ps,
            _ => panic!("expected Single, got {}", self),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Pattern::Single(p) => p.span,
            Pattern::StructUnpack(p) | Pattern::TupleUnpack(p) => p.span,
        }
    }

    pub fn iter(&self) -> PatternIter {
        PatternIter {
            pattern: self,
            pos: 0,
        }
    }

    pub fn ids(&self) -> Vec<BindingInfoId> {
        self.iter()
            .map(|p| p.binding_info_id)
            .collect::<Vec<BindingInfoId>>()
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
            Pattern::Single(pat) => {
                if self.pos == 0 {
                    Some(pat)
                } else {
                    None
                }
            }
            Pattern::StructUnpack(pat) | Pattern::TupleUnpack(pat) => pat.symbols.get(self.pos),
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
                Pattern::Single(s) => s.to_string(),
                Pattern::StructUnpack(s) => format!("{{{}}}", s),
                Pattern::TupleUnpack(s) => format!("({})", s),
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
    pub binding_info_id: BindingInfoId,
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
