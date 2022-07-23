#[derive(Debug)]
pub struct Stack<T, const CAPACITY: usize> {
    inner: Vec<T>,
}

impl<T: ToString, const CAPACITY: usize> Stack<T, CAPACITY> {
    pub fn new() -> Self {
        Self {
            inner: Vec::with_capacity(CAPACITY),
        }
    }

    pub fn push(&mut self, value: T) {
        debug_assert!(self.inner.len() <= self.inner.capacity(), "stack overflow");
        self.inner.push(value);
    }

    #[inline]
    pub fn pop(&mut self) -> T {
        self.inner.pop().unwrap()
    }

    #[inline]
    pub fn peek(&self, offset: usize) -> &T {
        &self.inner[self.len() - 1 - offset]
    }

    #[inline]
    pub fn peek_mut(&mut self, offset: usize) -> &mut T {
        let len = self.len();
        &mut self.inner[len - 1 - offset]
    }

    #[allow(unused)]
    #[inline]
    pub fn last(&self) -> &T {
        self.inner.last().unwrap()
    }

    #[inline]
    pub fn last_mut(&mut self) -> &mut T {
        self.inner.last_mut().unwrap()
    }

    #[inline]
    pub fn swap(&mut self, a: usize, b: usize) {
        self.inner.swap(a, b)
    }

    #[inline]
    pub fn get(&self, index: usize) -> &T {
        &self.inner[index]
    }

    #[inline]
    pub fn get_mut(&mut self, index: usize) -> &mut T {
        &mut self.inner[index]
    }

    #[inline]
    pub fn set(&mut self, index: usize, value: T) {
        self.inner[index] = value;
    }

    #[inline]
    pub fn truncate(&mut self, len: usize) {
        self.inner.truncate(len)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    #[allow(unused)]
    #[inline]
    pub fn iter(&self) -> std::slice::Iter<T> {
        self.inner.iter()
    }
}
