use super::repr::*;
use super::shape::Shape;
use crate::context::Context;
use crate::def::*;
use crate::utility::*;
use std::marker::PhantomData;
use std::mem::size_of;

#[repr(C)]
#[derive(Copy, Clone)]
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
  fn init(&mut self) {
    self.field.assign(0);
  }

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
  pub fn shape(&self) -> Shape {
    let mask = self.field.mask_range(TYPE_TAG_START, SIZE_TAG_START);
    return Shape::from_tag(mask.bits() as u8);
  }

  #[inline]
  fn is_size_used_as_size(&self) -> bool {
    return self.field.get(SIZE_TAG_START);
  }
}

impl std::fmt::Debug for Header {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return write!(
      f,
      "
{:?} {{
  size: {:?},
  shape: {:?},
  mark: {:?},
  is_size_used_as_size: {:?}
}}",
      std::any::type_name::<Header>(),
      self.size(),
      self.shape(),
      self.is_marked(),
      self.is_size_used_as_size(),
    );
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

#[derive(Copy, Clone, Debug)]
#[repr(transparent)]
pub struct BareHeapLayout<T: Copy>(Addr, PhantomData<T>);

impl<T: Copy> BareHeapLayout<T> {
  pub fn new(context: &mut impl Context, size: usize) -> BareHeapLayout<T> {
    return BareHeapLayout::<T>(context.allocate(size), PhantomData);
  }

  pub fn new_into_heap(heap: Addr) -> BareHeapLayout<T> {
    return BareHeapLayout::<T>(heap, PhantomData);
  }

  pub fn persist(context: &mut impl Context, size: usize) -> BareHeapLayout<T> {
    return BareHeapLayout::<T>(context.allocate_persist(size), PhantomData);
  }

  pub fn as_ref(&self) -> &T {
    return unsafe { std::mem::transmute::<Addr, &T>(self.0) };
  }

  pub fn as_ref_mut(&mut self) -> &mut T {
    return unsafe { std::mem::transmute::<Addr, &mut T>(self.0) };
  }

  pub fn as_addr(&self) -> Addr {
    return self.0;
  }

  pub fn set(&mut self, data: Addr) {
    self.0 = data;
  }

  pub fn out(&mut self) {
    self.0 = std::ptr::null_mut();
  }
}

#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct HeapLayout<T: Copy>(Addr, PhantomData<T>);

#[derive(Copy, Clone)]
pub struct VoidHeapBody {}

impl<T: Copy> HeapLayout<T> {
  pub fn new(context: &mut impl Context, size: usize, shape: Shape) -> HeapLayout<T> {
    return HeapLayout::<T>(Cell::init_heap(context, size, shape), PhantomData);
  }

  pub fn new_into_heap(heap: Addr, size: usize, shape: Shape) -> HeapLayout<T> {
    return HeapLayout::<T>(Cell::init_into_heap(heap, size, shape), PhantomData);
  }

  pub fn persist(context: &mut impl Context, size: usize, shape: Shape) -> HeapLayout<T> {
    return HeapLayout::<T>(Cell::init_persistent_heap(context, size, shape), PhantomData);
  }

  pub fn wrap(heap: Addr) -> HeapLayout<T> {
    return HeapLayout::<T>(heap, PhantomData);
  }

  pub fn as_ref(&self) -> &T {
    return unsafe { std::mem::transmute::<Addr, &T>(self.0.offset(Cell::SIZE as isize)) };
  }

  pub fn as_ref_mut(&mut self) -> &mut T {
    return unsafe { std::mem::transmute::<Addr, &mut T>(self.0.offset(Cell::SIZE as isize)) };
  }

  pub fn as_addr(&self) -> Addr {
    return self.0;
  }

  pub fn out(&mut self) {
    self.0 = std::ptr::null_mut();
  }

  pub fn set(&self, value: &T) {
    unsafe {
      *(self.0.offset(Cell::SIZE as isize) as *mut T) = *value;
    };
  }

  pub fn size(&self) -> usize {
    return Cell::wrap(self.0).get_header().size();
  }

  pub fn cell(&self) -> Cell {
    return Cell::wrap(self.0);
  }
}

impl<T: Copy> std::fmt::Debug for HeapLayout<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return write!(
      f,
      "{:?}
{:?} {{
  header_addr: {:p},
  body_addr: {:p}
}}",
      self.cell().get_header(),
      std::any::type_name::<HeapLayout<T>>(),
      self.0,
      unsafe { self.0.offset(Cell::SIZE as isize) }
    );
  }
}

impl<T: Copy> std::fmt::Display for HeapLayout<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return write!(f, "{:?}", self);
  }
}

pub trait HeapObject: Into<Repr> + Copy {
  fn raw_heap(&self) -> Addr;
  fn size(&self) -> usize {
    return self.cell().get_header().size();
  }

  fn from_ptr(heap: *mut Byte) -> Self;

  fn shape(&self) -> Shape {
    return self.cell().get_header().shape();
  }

  fn cell(&self) -> Cell {
    return Cell::wrap(self.raw_heap());
  }

  fn out(&mut self) -> Addr;

  fn is_out(&self) -> bool {
    return self.raw_heap() == std::ptr::null_mut();
  }
}

macro_rules! _priv_impl_heap_object_body {
  () => {
    fn from_ptr(heap: *mut Byte) -> Self {
      return Self::wrap(heap);
    }

    fn raw_heap(&self) -> Addr {
      return self.0.as_addr();
    }

    fn out(&mut self) -> Addr {
      let ret = self.0.as_addr();
      self.0.out();
      return ret;
    }
  };
}

macro_rules! impl_heap_object {
  ($name:ident) => {
    impl HeapObject for $name {
      _priv_impl_heap_object_body!();
    }
  };
  () => {
    _priv_impl_heap_object_body!();
  };
}

#[repr(transparent)]
#[derive(Copy, Clone, Debug)]
pub struct Cell(BareHeapLayout<Header>);

impl Cell {
  pub const TYPE: Shape = Shape::cell();
  pub const SIZE: usize = size_of::<Header>();
  pub fn init_heap(context: &mut impl Context, size: usize, shape: Shape) -> Addr {
    let heap = context.allocate(size);
    return Cell::init_into_heap(heap, size, shape);
  }

  pub fn init_persistent_heap(context: &mut impl Context, size: usize, shape: Shape) -> Addr {
    let heap = context.allocate_persist(size);
    return Cell::init_into_heap(heap, size, shape);
  }

  pub fn init_into_heap(heap: Addr, size: usize, shape: Shape) -> Addr {
    let mut layout = BareHeapLayout::<Header>::new_into_heap(heap);
    let header = layout.as_ref_mut();
    header.init();
    header.set_size(size);
    header.set_shape(shape);
    return heap;
  }

  pub fn get_header_mut(&mut self) -> &mut Header {
    return self.0.as_ref_mut();
  }

  pub fn get_header(&self) -> &Header {
    return self.0.as_ref();
  }

  pub fn get_body(&self) -> Addr {
    return unsafe { self.0.as_addr().offset(Cell::SIZE as isize) };
  }

  fn wrap(heap: Addr) -> Cell {
    return Cell(BareHeapLayout::<Header>::new_into_heap(heap));
  }
}

impl_heap_object!(Cell);
impl_repr_convertion!(Cell);
