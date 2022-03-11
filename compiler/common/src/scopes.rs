use std::{collections::HashMap, hash::Hash};

#[derive(Debug, Clone)]
pub struct Scopes<Idx: Hash + Eq, Value> {
    inner: Vec<Inner<Idx, Value>>,
}

type Inner<Idx, Value> = HashMap<Idx, Value>;

impl<Idx: Hash + Eq, Value> Scopes<Idx, Value> {
    pub fn new() -> Self {
        Self { inner: vec![] }
    }

    pub fn push_scope(&mut self) {
        self.inner.push(Default::default());
    }

    pub fn pop_scope(&mut self) -> Option<Inner<Idx, Value>> {
        self.inner.pop()
    }

    pub fn get(&self, idx: Idx) -> Option<&Value> {
        for scope in self.inner.iter().rev() {
            if let Some(decl) = scope.get(&idx) {
                return Some(decl);
            }
        }

        None
    }

    pub fn get_mut(&mut self, idx: Idx) -> Option<&mut Value> {
        for scope in self.inner.iter_mut().rev() {
            if let Some(decl) = scope.get_mut(&idx) {
                return Some(decl);
            }
        }

        None
    }

    pub fn insert(&mut self, idx: Idx, value: Value) -> Option<Value> {
        self.inner.last_mut().unwrap().insert(idx, value)
    }

    pub fn remove(&mut self, idx: Idx) -> Option<Value> {
        self.inner.last_mut().unwrap().remove(&idx)
    }

    pub fn depth(&self) -> usize {
        self.inner.len()
    }

    pub fn clear(&mut self) {
        self.inner.clear()
    }
}
