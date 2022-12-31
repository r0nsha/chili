use std::ops::Range;

pub type FileId = usize;

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub file_id: FileId,
    pub start: Position,
    pub end: EndPosition,
}

impl Span {
    pub fn new(file_id: FileId, start: Position, end: EndPosition) -> Self {
        Self { file_id, start, end }
    }

    pub fn initial(file_id: FileId) -> Self {
        Self::new(file_id, Position::initial(), EndPosition::initial())
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

    pub fn contains(&self, index: usize) -> bool {
        self.range().contains(&index)
    }

    pub fn to(&self, other: Self) -> Self {
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

    pub fn after(&self) -> Span {
        let start_pos = Position {
            index: self.end.index,
            line: self.start.line,
            column: self.start.column,
        };

        let end_pos = EndPosition { index: self.end.index };

        self.with_start(start_pos).with_end(end_pos)
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

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position {
    pub index: usize,
    pub line: u32,
    pub column: u32,
}

impl Position {
    pub fn new(index: usize, line: u32, column: u32) -> Self {
        Self { index, line, column }
    }

    pub fn initial() -> Self {
        Self {
            index: 0,
            line: 1,
            column: 1,
        }
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EndPosition {
    pub index: usize,
}

impl EndPosition {
    pub fn new(index: usize) -> Self {
        Self { index }
    }

    pub fn initial() -> Self {
        Self { index: 0 }
    }
}

pub trait Spannable {
    fn span(&self) -> Span;
}
