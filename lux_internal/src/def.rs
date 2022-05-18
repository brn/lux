use std::mem::size_of;

pub const ALIGNMENT: usize = size_of::<&i32>();
pub type Byte = u8;
pub type UintPtr = usize;
pub const PTR_SIZE: usize = size_of::<usize>();
pub type Addr = *mut Byte;
