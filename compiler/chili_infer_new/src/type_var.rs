use core::fmt;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeVar(usize);

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'t{}", self.0)
    }
}
