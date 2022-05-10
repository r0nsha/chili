mod byte_seq;
mod display;
pub mod ffi;
pub mod instruction;
pub mod interp;
mod lower;
mod stack;
pub mod value;
pub mod vm;

const WORD_SIZE: usize = std::mem::size_of::<usize>();
const IS_64BIT: bool = WORD_SIZE == 8;
