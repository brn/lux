use super::allocator::*;
use super::heap::*;

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

  pub fn allocate<'a, T>(&mut self) -> Option<Managed<'a, T>> {
    let ret = self.from_space.allocate::<T>();
    if ret.is_none() {
      return None;
    }

    return Some(Managed::wrap(ret.unwrap()));
  }

  pub fn gc(&mut self) {}
}
