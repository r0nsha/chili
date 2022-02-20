use std::ops::Range;

use chilic_span::{EndPosition, FileId, Position, Span};

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
}

impl Into<Range<usize>> for Cursor {
    fn into(self) -> Range<usize> {
        self.range()
    }
}

impl Into<Range<usize>> for &Cursor {
    fn into(self) -> Range<usize> {
        self.range()
    }
}

impl Into<Range<usize>> for &mut Cursor {
    fn into(self) -> Range<usize> {
        self.range()
    }
}
