use super::repr::*;
use super::shape::Shape;
use crate::context::Context;
use crate::def::*;
use crate::impl_repr_convertion;
use crate::utility::*;
use std::mem::size_of;

#[repr(C)]
pub struct Header {
  ///
  /// Use as object size information and used as forwarded pointer and mark bit.
  ///
  /// |--data---|--size tag--|--type tag--|--mark bit--|
  /// |--56bit--|----1bit----|----6bit----|----1bit----|
  ///
  field: Bitset<u64>,
}

const MARK_BIT_START: usize = 1;
const MARK_BIT_SIZE: usize = 1;
const TYPE_TAG_START: usize = MARK_BIT_START + MARK_BIT_SIZE;
const TYPE_TAG_SIZE: usize = 6;
const SIZE_TAG_START: usize = TYPE_TAG_START + TYPE_TAG_SIZE;
const SIZE_TAG_SIZE: usize = 1;
const DATA_FIELD_START: usize = SIZE_TAG_START + SIZE_TAG_SIZE;
const DATA_FIELD_SIZE: usize = 56;

impl Header {
  #[inline]
  pub fn set_size(&mut self, size: usize) {
    debug_assert!(size <= 0xffffffffffffff, "Size must be less than 56bit integer");
    self.field.set(SIZE_TAG_START);
    let mut range = self.field.mask_lower(DATA_FIELD_START);
    range.assign(size as u64);
    self.field = range.into();
  }

  #[inline]
  pub fn size(&self) -> usize {
    if self.is_size_used_as_size() {
      let mask = self.field.mask_lower(DATA_FIELD_START);
      return mask.bits() as usize;
    }
    return 0;
  }

  #[inline]
  pub fn set_forwarded_pointer(&mut self, addr: *mut Byte) {
    self.field.unset(SIZE_TAG_START);
    let mut mask = self.field.mask_lower(DATA_FIELD_START);
    mask.assign(addr as u64);
    self.field = mask.into();
  }

  #[inline]
  pub fn forwarded_pointer(&self) -> *mut Byte {
    if !self.is_size_used_as_size() {
      return self.field.mask_lower(DATA_FIELD_START).bits() as *mut Byte;
    }
    return 0xdeadbeef as *mut Byte;
  }

  #[inline]
  pub fn mark(&mut self) {
    self.field.set(MARK_BIT_START);
  }

  #[inline]
  pub fn unmark(&mut self) {
    self.field.unset(MARK_BIT_START);
  }

  #[inline]
  pub fn is_marked(&self) -> bool {
    return self.field.get(MARK_BIT_START);
  }

  #[inline]
  pub fn set_shape(&mut self, shape: Shape) {
    let mut mask = self.field.mask_range(TYPE_TAG_START, SIZE_TAG_START);
    mask.assign(shape.into());
    self.field = mask.into();
  }

  #[inline]
  pub fn shape(&mut self) -> Shape {
    let mask = self.field.mask_range(TYPE_TAG_START, SIZE_TAG_START);
    return Shape::from_tag(mask.bits() as u8);
  }

  #[inline]
  fn is_size_used_as_size(&self) -> bool {
    return self.field.get(SIZE_TAG_START);
  }
}

#[cfg(test)]
mod header_tests {
  use super::*;
  #[test]
  fn header_set_size_test() {
    let mut h = Header {
      field: Bitset::<u64>::new(),
    };
    h.set_size(72057594037927935);
    assert_eq!(h.size(), 72057594037927935);
  }

  #[test]
  fn header_set_forwarded_pointer() {
    let p = 0xdeadbeef as *mut Byte;
    let mut h = Header {
      field: Bitset::<u64>::new(),
    };
    h.set_forwarded_pointer(p);
    assert_eq!(h.forwarded_pointer() as usize, p as usize);
  }

  #[test]
  fn header_mark() {
    let mut h = Header {
      field: Bitset::<u64>::new(),
    };
    h.mark();
    assert_eq!(h.is_marked(), true);
  }

  #[test]
  fn header_unmark() {
    let mut h = Header {
      field: Bitset::<u64>::new(),
    };
    h.mark();
    assert_eq!(h.is_marked(), true);
    h.unmark();
    assert_eq!(h.is_marked(), false);
  }

  #[test]
  fn header_set_shape() {
    let mut h = Header {
      field: Bitset::<u64>::new(),
    };
    h.set_shape(Shape::undefined());
    assert_eq!(h.shape(), Shape::undefined());
  }
}

pub trait HeapObject: Into<Repr> + Copy {
  fn raw_heap(&self) -> Addr;
  fn size(&self) -> usize;
  fn from_ptr(heap: *mut Byte) -> Self;

  fn shape(&self) -> Shape {
    let header = self.raw_heap() as *mut Header;
    return unsafe { (*header).shape() };
  }

  fn cell(&self) -> Cell {
    return Cell { heap: self.raw_heap() };
  }

  fn out(&mut self) -> Addr;

  fn is_out(&self) -> bool {
    return self.raw_heap() == std::ptr::null_mut();
  }
}

#[macro_export]
macro_rules! impl_heap_object {
  ($name:ident $(< $( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+ >)?) => {
    impl $(< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? HeapObject for $name $(< $( $lt ),+ >)? {
      fn size(&self) -> usize {
        return self.byte_length();
      }

      fn from_ptr(heap: *mut Byte) -> Self {
        return Self::wrap(heap);
      }

      fn raw_heap(&self) -> Addr {
        return self.heap;
      }

      fn out(&mut self) -> Addr {
        let ret = self.heap;
        self.heap = std::ptr::null_mut();
        return ret;
      }
    }
  };
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct Cell {
  heap: Addr,
}

pub fn field_addr(this: Addr, offset: usize) -> Addr {
  unsafe { this.offset(offset as isize) }
}

impl Cell {
  pub const TYPE: Shape = Shape::cell();
  pub const SIZE: usize = size_of::<Header>();
  pub fn new(context: &mut Context, size: usize, shape: Shape) -> Cell {
    let heap = context.allocate(size);
    return Cell::new_into_heap(heap, size, shape);
  }

  pub fn persist(context: &mut Context, size: usize, shape: Shape) -> Cell {
    let heap = context.allocate_persist(size);
    return Cell::new_into_heap(heap, size, shape);
  }

  pub fn new_into_heap(heap: *mut Byte, size: usize, shape: Shape) -> Cell {
    let header = heap as *mut Header;
    unsafe {
      (*header).set_size(size);
      (*header).set_shape(shape);
      return Cell { heap };
    }
  }

  pub fn get_body(&self) -> Addr {
    return field_addr(self.heap, Cell::SIZE);
  }

  fn byte_length(&self) -> usize {
    return Cell::SIZE;
  }

  fn wrap(heap: Addr) -> Cell {
    return Cell { heap };
  }
}

impl_heap_object!(Cell);
impl_repr_convertion!(Cell);
