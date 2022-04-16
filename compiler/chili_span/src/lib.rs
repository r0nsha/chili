use std::ops::Range;

pub type FileId = usize;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub file_id: FileId,
    pub start: Position,
    pub end: EndPosition,
}

impl Span {
    pub fn new(file_id: FileId, start: Position, end: EndPosition) -> Self {
        Self {
            file_id,
            start,
            end,
        }
    }

    pub fn unknown() -> Self {
        Self::new(usize::MAX, Default::default(), Default::default())
    }

    pub fn is_unknown(&self) -> bool {
        self.file_id == usize::MAX
    }

    pub fn range(&self) -> Range<usize> {
        self.start.index..self.end.index
    }

    pub fn with_start(&self, start: Position) -> Self {
        Self {
            file_id: self.file_id,
            start,
            end: self.end,
        }
    }

    pub fn with_end(&self, end: EndPosition) -> Self {
        Self {
            file_id: self.file_id,
            start: self.start,
            end,
        }
    }
}

impl Default for Span {
    fn default() -> Self {
        Self {
            file_id: Default::default(),
            start: Default::default(),
            end: Default::default(),
        }
    }
}

impl Ord for Span {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.start, self.end).cmp(&(other.start, other.end))
    }
}

impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position {
    pub index: usize,
    pub line: u32,
    pub column: u16,
}

impl Position {
    pub fn new(index: usize, line: u32, column: u16) -> Self {
        Self {
            index,
            line,
            column,
        }
    }

    pub fn initial() -> Self {
        Self {
            index: 0,
            line: 1,
            column: 1,
        }
    }
}

impl Default for Position {
    fn default() -> Self {
        Self {
            index: Default::default(),
            line: Default::default(),
            column: Default::default(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EndPosition {
    pub index: usize,
}

impl EndPosition {
    pub fn new(index: usize) -> Self {
        Self { index }
    }
}

impl Default for EndPosition {
    fn default() -> Self {
        Self {
            index: Default::default(),
        }
    }
}

pub trait To {
    fn to(&self, other: Self) -> Self;
}

impl To for Span {
    fn to(&self, other: Self) -> Self {
        if self.file_id != other.file_id {
            panic!("can't merge spans from different files");
        }

        let start = if self.start.index < other.start.index {
            self.start
        } else {
            other.start
        };

        let end = if self.end.index < other.end.index {
            other.end
        } else {
            self.end
        };

        Self {
            file_id: self.file_id,
            start,
            end,
        }
    }
}

pub trait Spannable {
    fn span(&self) -> Span;
}
