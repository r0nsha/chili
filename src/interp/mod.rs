pub mod ffi;
pub mod interp;
mod lower;
pub mod vm;
pub mod workspace;

const WORD_SIZE: usize = std::mem::size_of::<usize>();
const IS_64BIT: bool = WORD_SIZE == 8;
