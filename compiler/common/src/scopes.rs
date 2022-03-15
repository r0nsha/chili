use std::{collections::HashMap, hash::Hash};

#[derive(Debug, Clone)]
pub struct Scopes<Id: Hash + Eq, Value> {
    inner: Vec<Inner<Id, Value>>,
}

type Inner<Id, Value> = HashMap<Id, Value>;

impl<Id: Hash + Eq, Value> Scopes<Id, Value> {
    pub fn new() -> Self {
        Self { inner: vec![] }
    }

    pub fn push_scope(&mut self) {
        self.inner.push(Default::default());
    }

    pub fn pop_scope(&mut self) -> Option<Inner<Id, Value>> {
        self.inner.pop()
    }

    pub fn get(&self, id: Id) -> Option<&Value> {
        for scope in self.inner.iter().rev() {
            if let Some(decl) = scope.get(&id) {
                return Some(decl);
            }
        }

        None
    }

    pub fn get_mut(&mut self, id: Id) -> Option<&mut Value> {
        for scope in self.inner.iter_mut().rev() {
            if let Some(decl) = scope.get_mut(&id) {
                return Some(decl);
            }
        }

        None
    }

    pub fn insert(&mut self, id: Id, value: Value) -> Option<Value> {
        self.inner.last_mut().unwrap().insert(id, value)
    }

    pub fn remove(&mut self, id: Id) -> Option<Value> {
        self.inner.last_mut().unwrap().remove(&id)
    }

    pub fn depth(&self) -> usize {
        self.inner.len()
    }

    pub fn clear(&mut self) {
        self.inner.clear()
    }
}
