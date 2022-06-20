use slab::Slab;
use std::{marker::PhantomData, ops};

#[macro_export]
macro_rules! define_id_type {
    ($name:ident) => {
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
        pub struct $name(usize);

        impl From<usize> for $name {
            fn from(x: usize) -> Self {
                Self(x)
            }
        }

        impl Into<usize> for $name {
            fn into(self) -> usize {
                self.0
            }
        }

        impl Default for $name {
            fn default() -> Self {
                Self(usize::MAX)
            }
        }

        impl $name {
            pub fn unknown() -> Self {
                Self(usize::MAX)
            }

            pub fn inner(&self) -> usize {
                self.0
            }
        }
    };
}

pub struct IdCache<I, V> {
    inner: Slab<V>,
    marker: PhantomData<I>,
}

impl<I, V> Default for IdCache<I, V>
where
    I: From<usize> + Into<usize>,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<I, V> IdCache<I, V> {
    pub fn new() -> Self {
        Self {
            inner: Slab::new(),
            marker: PhantomData,
        }
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn iter(&self) -> slab::Iter<V> {
        self.inner.iter()
    }

    pub fn iter_mut(&mut self) -> slab::IterMut<V> {
        self.inner.iter_mut()
    }
}

impl<I, V> IdCache<I, V>
where
    I: Into<usize>,
{
    pub fn get(&self, id: I) -> Option<&V> {
        self.inner.get(id.into())
    }

    pub fn get_mut(&mut self, id: I) -> Option<&mut V> {
        self.inner.get_mut(id.into())
    }
}

impl<I, V> IdCache<I, V>
where
    I: From<usize>,
{
    pub fn insert(&mut self, v: V) -> I {
        I::from(self.inner.insert(v))
    }
}

impl<I, V> IdCache<I, V>
where
    I: From<usize> + Copy,
    V: WithId<I>,
{
    pub fn insert_with_id(&mut self, mut v: V) -> I {
        let vacant_entry = self.inner.vacant_entry();

        let id = I::from(vacant_entry.key());

        *v.id_mut() = id;
        vacant_entry.insert(v);

        id
    }
}

impl<I, V> ops::Index<I> for IdCache<I, V>
where
    I: Into<usize>,
{
    type Output = V;

    fn index(&self, index: I) -> &Self::Output {
        &self.inner[index.into()]
    }
}

impl<I, V> ops::IndexMut<I> for IdCache<I, V>
where
    I: Into<usize>,
{
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.inner[index.into()]
    }
}

pub trait WithId<I> {
    fn id(&self) -> &I;
    fn id_mut(&mut self) -> &mut I;
}
