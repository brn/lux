use super::heap::*;

pub struct Context {
  pub heap: Heap,
}

impl Context {
  pub fn allocate<T>(&mut self) -> Managed<T> {
    return self.heap.allocate::<T>();
  }
}
