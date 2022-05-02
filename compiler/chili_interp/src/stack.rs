pub struct Stack<T, const CAPACITY: usize>(Vec<T>);

impl<T, const CAPACITY: usize> Stack<T, CAPACITY>
where
    T: ToString,
{
    pub fn new() -> Self {
        Self(Vec::with_capacity(CAPACITY))
    }

    pub fn push(&mut self, value: T) {
        if self.0.len() >= self.0.capacity() {
            panic!("stack overflow")
        }

        self.0.push(value);
    }

    pub fn pop(&mut self) -> T {
        self.0.pop().unwrap()
    }

    pub fn peek(&self, offset: usize) -> &T {
        let size = self.len();
        &self.0[size - 1 - offset]
    }

    pub fn peek_mut(&mut self) -> &mut T {
        self.0.last_mut().unwrap()
    }

    pub fn get(&self, index: usize) -> &T {
        &self.0[index]
    }

    pub fn get_mut(&mut self, index: usize) -> &mut T {
        &mut self.0[index]
    }

    pub fn set(&mut self, index: usize, value: T) {
        self.0[index] = value;
    }

    pub fn truncate(&mut self, len: usize) {
        self.0.truncate(len)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn trace(&self) -> String {
        let values = self
            .0
            .iter()
            .map(|v| v.to_string())
            .collect::<Vec<String>>()
            .join(" , ");

        format!("[{}]", values)
    }
}
