use std::{collections::HashMap, hash::Hash};

#[derive(Clone)]
pub struct Scopes<K: Hash + Eq, V> {
    inner: Vec<HashMap<K, V>>,
}

impl<K: Hash + Eq, V> Scopes<K, V> {
    pub fn new() -> Self {
        Self { inner: vec![] }
    }

    pub fn push_scope(&mut self) {
        self.inner.push(HashMap::default());
    }

    pub fn pop_scope(&mut self) {
        self.inner.pop();
    }

    pub fn insert(&mut self, k: K, v: V) {
        self.inner.last_mut().unwrap().insert(k, v);
    }

    pub fn get(&self, k: K) -> Option<(usize, &V)> {
        for (depth, scope) in self.inner.iter().enumerate().rev() {
            if let Some(value) = scope.get(&k) {
                return Some((depth + 1, value));
            }
        }
        None
    }

    pub fn get_mut(&mut self, k: K) -> Option<(usize, &mut V)> {
        for (depth, scope) in self.inner.iter_mut().enumerate().rev() {
            if let Some(value) = scope.get_mut(&k) {
                return Some((depth + 1, value));
            }
        }
        None
    }

    pub fn depth(&self) -> usize {
        self.inner.len()
    }
}
