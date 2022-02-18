use std::ops::Range;

use chilic_span::Span;

#[derive(Debug, Clone, Copy)]
pub(crate) struct Cursor {
    start: usize,
    end: usize,
    file_id: usize,
}

impl Cursor {
    pub(crate) fn new(start: usize, file_id: usize) -> Self {
        Self {
            start,
            end: start,
            file_id,
        }
    }

    #[inline]
    pub(crate) fn continue_from_end(&mut self) {
        self.start = self.end;
    }

    #[inline]
    pub(crate) fn bump(&mut self) {
        self.end += 1;
    }

    #[inline]
    #[allow(unused)]
    pub(crate) fn start(&self) -> usize {
        self.start
    }

    #[inline]
    pub(crate) fn end(&self) -> usize {
        self.end
    }

    #[inline]
    #[allow(unused)]
    pub(crate) fn file_id(&self) -> usize {
        self.file_id
    }

    #[inline]
    pub(crate) fn range(&self) -> Range<usize> {
        self.start..self.end
    }

    pub(crate) fn span(&self) -> Span {
        Span {
            range: self.range(),
            file_id: self.file_id,
        }
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
