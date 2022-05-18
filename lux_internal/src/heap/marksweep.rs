use super::heap::Heap;
use crate::def::*;
use std::boxed::Box;
use std::vec::Vec;

pub struct MarkSweep {
  young: Heap,
  old: Heap,
  persistent: Vec<Box<Heap>>,
}

impl MarkSweep {
  pub fn new() -> MarkSweep {
    return MarkSweep {
      young: Heap::new(),
      old: Heap::new(),
      persistent: vec![Box::new(Heap::new())],
    };
  }

  pub fn allocate_young(&mut self, size: usize) -> Option<*mut Byte> {
    return self.young.allocate(size);
  }

  pub fn allocate_old(&mut self, size: usize) -> Option<*mut Byte> {
    return self.old.allocate(size);
  }

  pub fn allocate_persistent(&mut self, size: usize) -> Option<*mut Byte> {
    {
      let heap = &mut *self.persistent.last_mut().unwrap().as_mut();
      let allocated = heap.allocate(size);
      if !allocated.is_none() {
        return allocated;
      }
    }
    self.grow_persistent();
    return (&mut *self.persistent.last_mut().unwrap().as_mut()).allocate(size);
  }

  pub fn grow_persistent(&mut self) {
    self.persistent.push(Box::new(Heap::new()));
  }

  pub fn gc() {}
}
