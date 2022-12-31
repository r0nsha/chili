use std::ops::Range;

pub struct Source<'lx> {
    source: &'lx str,
    source_len: usize,
    source_bytes: &'lx [u8],
}

impl<'lx> Source<'lx> {
    pub fn new(source: &'lx str) -> Self {
        Self {
            source,
            source_len: source.len(),
            source_bytes: source.as_bytes(),
        }
    }

    pub fn range(&self, range: impl Into<Range<usize>>) -> &str {
        let range: Range<usize> = range.into();
        &self.source[range.start..range.start + (range.end - range.start)]
    }

    pub fn at(&self, n: usize) -> char {
        *self.source_bytes.get(n).unwrap_or(&b'\0') as char
    }

    pub fn len(&self) -> usize {
        self.source_len
    }
}
