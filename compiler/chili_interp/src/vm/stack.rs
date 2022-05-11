pub struct Stack<T, const CAPACITY: usize>(Vec<T>);

impl<T, const CAPACITY: usize> Stack<T, CAPACITY>
where
    T: ToString,
{
    pub fn new() -> Self {
        Self(Vec::with_capacity(CAPACITY))
    }

    pub fn push(&mut self, value: T) {
        assert!(self.0.len() <= self.0.capacity(), "stack overflow");
        self.0.push(value);
    }

    #[inline]
    pub fn pop(&mut self) -> T {
        self.0.pop().unwrap()
    }

    #[inline]
    pub fn peek(&self, offset: usize) -> &T {
        &self.0[self.len() - 1 - offset]
    }

    #[inline]
    pub fn peek_mut(&mut self, offset: usize) -> &mut T {
        let len = self.len();
        &mut self.0[len - 1 - offset]
    }

    #[inline]
    pub fn last(&self) -> &T {
        self.0.last().unwrap()
    }

    #[inline]
    pub fn last_mut(&mut self) -> &mut T {
        self.0.last_mut().unwrap()
    }

    #[inline]
    pub fn take(&mut self, offset: usize) -> T {
        self.0.remove(self.len() - 1 - offset)
    }

    #[inline]
    pub fn get(&self, index: usize) -> &T {
        &self.0[index]
    }

    #[inline]
    pub fn get_mut(&mut self, index: usize) -> &mut T {
        &mut self.0[index]
    }

    #[inline]
    pub fn set(&mut self, index: usize, value: T) {
        self.0[index] = value;
    }

    #[inline]
    pub fn truncate(&mut self, len: usize) {
        self.0.truncate(len)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[inline]
    pub fn iter(&self) -> std::slice::Iter<T> {
        self.0.iter()
    }
}
