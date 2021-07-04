use super::exotic::Exotic;
use crate::def::*;
use std::alloc::{alloc, dealloc, Layout};
use std::marker::PhantomData;
use std::ptr;

const BLOCK_SIZE: usize = kb!(1);
const IDEAL_STORABLE_OBJECT_COUNT: usize = 20;

struct Block {
  heap: Addr,
  used: usize,
  layout: Layout,
  next: Option<Box<Block>>,
}
impl Block {
  fn new(size: usize) -> Box<Block> {
    let layout = Layout::from_size_align(size, ALIGNMENT).unwrap();
    Box::new(Block {
      heap: unsafe { alloc(layout) },
      used: 0,
      layout,
      next: None,
    })
  }

  fn alloc<T>(&mut self, object: &T, layout: &Layout) -> Result<*mut T, usize> {
    let size = layout.size();
    if self.used + size < self.layout.size() {
      let h = unsafe { self.heap.offset(self.used as isize) as *mut T };
      self.used += size;
      unsafe { ptr::copy_nonoverlapping(object as *const T, h, 1) };
      return Ok(h as *mut T);
    }
    return Err(size);
  }
}
impl Drop for Block {
  fn drop(&mut self) {
    unsafe { dealloc(self.heap, self.layout) };
  }
}

pub struct RawBlock {
  ptr: *mut Block,
}
impl RawBlock {
  fn new(b: *mut Block) -> RawBlock {
    RawBlock { ptr: b }
  }

  fn null() -> RawBlock {
    RawBlock { ptr: ptr::null_mut() }
  }

  fn set<'a>(&mut self, b: &'a mut Block) {
    self.ptr = b as *mut Block;
  }
}
impl AsRef<Block> for RawBlock {
  fn as_ref(&self) -> &Block {
    return unsafe { self.ptr.as_ref().unwrap() };
  }
}
impl AsMut<Block> for RawBlock {
  fn as_mut(&mut self) -> &mut Block {
    return unsafe { self.ptr.as_mut().unwrap() };
  }
}

pub struct RegionalAllocator {
  head: Box<Block>,
  tail: RawBlock,
  sum_size: f64,
  alloc_count: u64,
}

impl RegionalAllocator {
  pub fn new() -> RegionalAllocator {
    let tmp_block = Block::new(BLOCK_SIZE);
    let leak = Box::leak(tmp_block);
    return RegionalAllocator {
      head: unsafe { Box::from_raw(leak) },
      tail: RawBlock::new(leak),
      sum_size: 0.0,
      alloc_count: 0,
    };
  }

  pub fn alloc<T>(&mut self, obj: &T) -> *mut T {
    let block = self.tail.as_mut();
    let layout = Layout::for_value(obj);
    let alloc_result = block.alloc(obj, &layout);
    if let Ok(result) = alloc_result {
      self.alloc_count += 1;
      self.sum_size += layout.size() as f64;
      return result;
    }
    self.grow(alloc_result.unwrap_err());
    return self.tail.as_mut().alloc(obj, &layout).unwrap();
  }

  pub fn grow(&mut self, failed_size: usize) {
    let new_block = Block::new(if failed_size < BLOCK_SIZE {
      if self.alloc_count > (IDEAL_STORABLE_OBJECT_COUNT as u64) {
        ((self.sum_size / (self.alloc_count as f64)) as usize) * IDEAL_STORABLE_OBJECT_COUNT
      } else {
        BLOCK_SIZE
      }
    } else {
      failed_size
    });
    let leak = Box::leak(new_block);
    self.tail.as_mut().next = unsafe { Some(Box::from_raw(leak)) };
    self.tail = RawBlock::new(leak);
  }
}

pub struct Region(RegionalAllocator);

impl Region {
  pub fn new() -> Self {
    return Region(RegionalAllocator::new());
  }

  pub fn alloc<O>(&mut self, object: O) -> Exotic<O> {
    return Exotic::new(self.0.alloc(&object));
  }
}

#[cfg(test)]
mod regional_allocator_test {
  use super::*;

  struct TestReprRs {
    field_1: u8,
    field_2: u32,
    field_3: u64,
  }

  #[repr(C)]
  struct TestReprC {
    field_1: u8,
    field_2: u32,
    field_3: u64,
  }

  #[test]
  fn alloc_test() {
    let mut region = Region::new();
    let o = region.alloc(TestReprRs {
      field_1: 8,
      field_2: 32,
      field_3: 64,
    });
    assert_eq!(o.field_1, 8);
    assert_eq!(o.field_2, 32);
    assert_eq!(o.field_3, 64);
  }

  #[test]
  fn alloc_a_lot_of_slab_test() {
    let mut region = Region::new();
    for _ in 0..1000 {
      let o = region.alloc(TestReprRs {
        field_1: 8,
        field_2: 32,
        field_3: 64,
      });
      assert_eq!(o.field_1, 8);
      assert_eq!(o.field_2, 32);
      assert_eq!(o.field_3, 64);

      let o2 = region.alloc(TestReprC {
        field_1: 8,
        field_2: 32,
        field_3: 64,
      });
      assert_eq!(o2.field_1, 8);
      assert_eq!(o2.field_2, 32);
      assert_eq!(o2.field_3, 64);
    }
  }
}
