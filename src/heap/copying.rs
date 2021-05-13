use super::allocator::*;
use super::heap::*;
use crate::def::*;
use std::mem::size_of;

pub struct CopyingHeap {
  from_space: Heap,
  to_space: Heap,
}

impl CopyingHeap {
  pub fn new() -> CopyingHeap {
    return CopyingHeap {
      from_space: Heap::new(),
      to_space: Heap::new(),
    };
  }

  pub fn allocate<'a>(&mut self, size: usize) -> Option<*mut Byte> {
    return self.from_space.allocate(size);
  }

  pub fn gc(&mut self) {}
}
