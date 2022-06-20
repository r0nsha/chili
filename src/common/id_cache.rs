use slab::Slab;
use std::marker::PhantomData;

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

pub struct IdCache<K, V> {
    inner: Slab<V>,
    marker: PhantomData<K>,
}

impl<K: Into<usize>, V> IdCache<K, V> {
    pub fn new() -> Self {
        Self {
            inner: Slab::new(),
            marker: PhantomData,
        }
    }

    pub fn get_binding(&self, id: K) -> Option<&V> {
        self.inner.get(id.into())
    }

    pub fn get_binding_mut(&mut self, id: K) -> Option<&mut V> {
        self.inner.get_mut(id.into())
    }
}
