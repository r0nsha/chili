mod display;
pub mod ffi;
pub mod instruction;
pub mod interp;
mod lower;
mod stack;
pub mod value;
pub mod vm;

const IS_64BIT: bool = std::mem::size_of::<usize>() == 8;
