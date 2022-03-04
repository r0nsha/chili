use std::fmt::Display;

use chili_span::Span;
use ustr::Ustr;

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Single(SymbolPattern),
    StructDestructor(DestructorPattern),
    TupleDestructor(DestructorPattern),
}

#[derive(Debug, PartialEq, Clone)]
pub struct DestructorPattern {
    pub symbols: Vec<SymbolPattern>,
    pub exhaustive: bool,
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

    pub fn span(&self) -> Span {
        match self {
            Pattern::Single(p) => p.span,
            Pattern::StructDestructor(p) | Pattern::TupleDestructor(p) => {
                p.span
            }
        }
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Pattern::Single(s) => s.to_string(),
                Pattern::StructDestructor(s) =>
                    format!("{{{}}}", s.to_string()),
                Pattern::TupleDestructor(s) => format!("({})", s.to_string()),
            }
        )
    }
}

impl Display for DestructorPattern {
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

#[derive(Debug, PartialEq, Clone)]
pub struct SymbolPattern {
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
