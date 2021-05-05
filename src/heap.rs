mod allocator;
mod copying;
mod heap;

pub use super::heap::allocator::Managed;
use super::heap::copying::*;

pub struct Heap {
  heap: CopyingHeap,
}

impl Heap {
  pub fn allocate<'a, T>(&mut self) -> Managed<'a, T> {
    let result = self.heap.allocate::<T>();
    if result.is_some() {
      return result.unwrap();
    }

    self.heap.gc();
    return self.allocate();
  }
}
