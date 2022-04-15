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
        match self {
            Pattern::Single(_) => true,
            _ => false,
        }
    }

    pub fn into_single(&self) -> SymbolPattern {
        match self {
            Pattern::Single(ps) => ps.clone(),
            _ => panic!("expected a name, got {}", self),
        }
    }

    pub fn as_single_ref(&self) -> &SymbolPattern {
        match self {
            Pattern::Single(ps) => ps,
            _ => panic!("expected a name, got {}", self),
        }
    }

    pub fn as_single_mut(&mut self) -> &mut SymbolPattern {
        match self {
            Pattern::Single(ps) => ps,
            _ => panic!("expected a name, got {}", self),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Pattern::Single(p) => p.span,
            Pattern::StructUnpack(p) | Pattern::TupleUnpack(p) => p.span,
        }
    }

    pub fn symbols(&self) -> Vec<&SymbolPattern> {
        match self {
            Pattern::Single(p) => vec![p],
            Pattern::StructUnpack(p) | Pattern::TupleUnpack(p) => {
                p.symbols.iter().collect::<Vec<_>>()
            }
        }
    }

    pub fn symbols_mut(&mut self) -> Vec<&mut SymbolPattern> {
        match self {
            Pattern::Single(p) => vec![p],
            Pattern::StructUnpack(p) | Pattern::TupleUnpack(p) => {
                p.symbols.iter_mut().collect::<Vec<_>>()
            }
        }
    }
}

impl IntoIterator for Pattern {
    type Item = SymbolPattern;
    type IntoIter = <Vec<SymbolPattern> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        let vec = match self {
            Pattern::Single(p) => vec![p],
            Pattern::StructUnpack(p) | Pattern::TupleUnpack(p) => p.symbols,
        };
        vec.into_iter()
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Pattern::Single(s) => s.to_string(),
                Pattern::StructUnpack(s) => format!("{{{}}}", s.to_string()),
                Pattern::TupleUnpack(s) => format!("({})", s.to_string()),
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
