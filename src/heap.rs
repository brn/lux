mod allocator;
mod copying;
mod handle;
mod heap;
mod marksweep;

pub use self::handle::*;
use super::heap::copying::*;
use super::heap::marksweep::*;
use crate::def::*;

pub struct Heap {
  first: CopyingHeap,
  second: MarkSweep,
}
unsafe impl Sync for Heap {}

impl Heap {
  pub fn new() -> Heap {
    return Heap {
      first: CopyingHeap::new(),
      second: MarkSweep::new(),
    };
  }

  pub fn allocate<'a>(&mut self, size: usize) -> *mut Byte {
    let result = self.first.allocate(size);
    if result.is_some() {
      return result.unwrap();
    }

    self.first.gc();
    return self.allocate(size);
  }

  pub fn allocate_persist<'a>(&mut self, size: usize) -> *mut Byte {
    let result = self.second.allocate_persistent(size);
    if result.is_some() {
      return result.unwrap();
    }

    panic!("Failed to allocate persistent memory");
  }
}
