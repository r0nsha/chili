use std::{collections::HashMap, hash::Hash};

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

    pub fn get(&self, k: K) -> Option<&V> {
        self.inner.last().unwrap().get(&k)
    }

    pub fn get_mut(&mut self, k: K) -> Option<&mut V> {
        self.inner.last_mut().unwrap().get_mut(&k)
    }

    pub fn insert(&mut self, k: K, v: V) {
        self.inner.last_mut().unwrap().insert(k, v);
    }
}
