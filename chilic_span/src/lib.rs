use std::ops::Range;

pub type FileId = usize;

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct Span {
    pub range: Range<usize>,
    pub file_id: FileId,
}

impl Span {
    pub fn new(cols: Range<usize>, file_id: FileId) -> Self {
        Self {
            range: cols,
            file_id,
        }
    }

    pub fn empty() -> Self {
        Self {
            range: 0..0,
            file_id: 0,
        }
    }

    pub fn merge(l1: &Span, l2: &Span) -> Self {
        if l1.file_id != l2.file_id {
            panic!("can't merge locations from different files");
        }

        Self {
            range: l1.range.start..l2.range.end,
            file_id: l1.file_id,
        }
    }

    pub fn end(&self) -> Self {
        Span::new(self.range.end..self.range.end, self.file_id)
    }

    pub fn with_start(&self, start: usize) -> Self {
        Span::new(start..self.range.end, self.file_id)
    }

    pub fn with_end(&self, end: usize) -> Self {
        Span::new(self.range.start..end, self.file_id)
    }

    pub fn trim_end(&self, amount: usize) -> Self {
        Span::new(self.range.start..self.range.end - amount, self.file_id)
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

#[derive(Debug, PartialEq, Clone)]
pub struct MaybeSpanned<T> {
    pub value: T,
    pub span: Option<Span>,
}

impl<T> MaybeSpanned<T> {
    pub fn new(value: T, span: Option<Span>) -> Self {
        Self { value, span }
    }

    pub fn not_spanned(value: T) -> Self {
        Self { value, span: None }
    }

    pub fn spanned(value: T, span: Span) -> Self {
        Self {
            value,
            span: Some(span),
        }
    }

    pub fn map<F: FnOnce(T) -> T>(self, f: F) -> Self {
        Self::new(f(self.value), self.span)
    }
}
