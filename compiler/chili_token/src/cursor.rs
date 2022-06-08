use std::ops::Range;

use chili_span::{EndPosition, FileId, Position, Span};

#[derive(Debug, Clone, Copy)]
pub(crate) struct Cursor {
    file_id: FileId,
    start: Position,
    end: Position,
}

impl Cursor {
    pub(crate) fn new(file_id: FileId) -> Self {
        Self {
            file_id,
            start: Position::initial(),
            end: Position::initial(),
        }
    }

    #[inline]
    pub(crate) fn continue_from_end(&mut self) {
        self.start = self.end;
    }

    #[inline]
    pub(crate) fn advance(&mut self, newline: bool) {
        self.end.index += 1;

        if newline {
            self.end.column = 0;
            self.end.line += 1;
        } else {
            self.end.column += 1;
        }
    }

    #[allow(unused)]
    #[inline]
    pub(crate) fn start_index(&self) -> usize {
        self.start.index
    }

    #[inline]
    pub(crate) fn end_index(&self) -> usize {
        self.end.index
    }

    #[inline]
    pub(crate) fn range(&self) -> Range<usize> {
        self.start.index..self.end.index
    }

    pub(crate) fn span(&self) -> Span {
        Span::new(
            self.file_id,
            self.start,
            EndPosition {
                index: self.end.index,
            },
        )
    }

    pub(crate) fn end_span(&self) -> Span {
        Span::new(
            self.file_id,
            Position {
                index: self.end.index,
                line: self.start.line, // FIXME: this is the wrong line...
                column: self.start.column,
            },
            EndPosition {
                index: self.end.index,
            },
        )
    }
}

impl From<Cursor> for Range<usize> {
    fn from(val: Cursor) -> Self {
        val.range()
    }
}

impl From<&Cursor> for Range<usize> {
    fn from(val: &Cursor) -> Self {
        val.range()
    }
}

impl From<&mut Cursor> for Range<usize> {
    fn from(val: &mut Cursor) -> Self {
        val.range()
    }
}
