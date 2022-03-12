use std::{collections::HashMap, hash::Hash};

pub(crate) trait Union {
    fn union(&self, other: &Self) -> Self;
}

/// Implement union for HashMap such that the value in `self` is used over the value in `other` in
/// the event of a collision.
impl<K, V> Union for HashMap<K, V>
where
    K: Clone + Eq + Hash,
    V: Clone,
{
    fn union(&self, other: &Self) -> Self {
        let mut res = self.clone();
        for (key, value) in other {
            res.entry(key.clone()).or_insert(value.clone());
        }
        res
    }
}
