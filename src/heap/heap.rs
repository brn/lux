use super::allocator::*;
use crate::def;
use std::alloc::Layout;

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

  pub fn allocate<'a, T>(&mut self) -> Option<&'a mut T> {
    let layout = Layout::new::<T>();
    if !self.has_enough_space(layout.size()) {
      return None;
    }
    unsafe {
      let ret = &mut *(self.space as *mut T);
      self.current = self.current.offset(layout.size() as isize);
      self.used += layout.size();
      return Some(ret);
    }
  }

  fn has_enough_space(&self, size: usize) -> bool {
    return self.used + size < self.size;
  }
}
