use super::allocator::*;
use crate::def;

pub struct Heap {
  space: *mut def::Byte,
  current: *mut def::Byte,
  used: usize,
  size: usize,
}

impl Heap {
  pub fn new() -> Heap {
    let space_result = unsafe { allocate_aligned(mb!(1), mb!(1)) };
    if space_result.is_none() {
      panic!("Failed to allocate heap");
    }
    let space = space_result.unwrap() as *mut def::Byte;
    return Heap {
      space,
      current: space,
      used: 0,
      size: mb!(1),
    };
  }

  pub fn allocate<'a>(&mut self, size: usize) -> Option<*mut def::Byte> {
    if !self.has_enough_space(size) {
      return None;
    }
    unsafe {
      let ret = self.current;
      self.current = self.current.offset(size as isize);
      self.used += size;
      return Some(ret);
    }
  }

  fn has_enough_space(&self, size: usize) -> bool {
    return self.used + size < self.size;
  }
}
