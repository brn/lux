use std::mem;

pub const ALIGNMENT: usize = mem::size_of::<&i32>();
pub type Byte = u8;
