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

pub mod ast;
pub mod compiler_info;
pub mod const_value;
pub mod path;
pub mod pattern;
pub mod ty;
pub mod workspace;
