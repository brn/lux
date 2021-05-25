use super::repr::*;
use super::shape::Shape;
use super::string::JsString;
use crate::context::AllocationOnlyContext;
use crate::def::*;
use crate::utility::*;
use std::marker::PhantomData;
use std::mem::size_of;
use std::ops::{Deref, DerefMut};

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

impl From<Addr> for Header {
  fn from(a: Addr) -> Header {
    unreachable!();
  }
}

impl Into<Addr> for Header {
  fn into(self) -> Addr {
    unreachable!();
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
#[repr(C)]
pub struct BareHeapLayout<T: Copy + From<Addr>>(u64, PhantomData<T>);

impl<T: Copy + From<Addr> + Into<Addr>> BareHeapLayout<T> {
  pub fn new(context: &mut impl AllocationOnlyContext, size: usize) -> BareHeapLayout<T> {
    return BareHeapLayout::<T>(context.allocate(size) as u64, PhantomData);
  }

  pub fn persist(context: &mut impl AllocationOnlyContext, size: usize) -> BareHeapLayout<T> {
    return BareHeapLayout::<T>(context.allocate_persist(size) as u64, PhantomData);
  }

  pub fn wrap(heap: Addr) -> BareHeapLayout<T> {
    return BareHeapLayout::<T>(heap as u64, PhantomData);
  }

  pub fn null() -> BareHeapLayout<T> {
    return BareHeapLayout::<T>(std::ptr::null_mut::<T>() as u64, PhantomData);
  }

  pub fn handle(&self) -> T {
    return T::from(self.addr());
  }

  pub fn is_null(&self) -> bool {
    return (self.addr()).is_null();
  }

  pub unsafe fn ref_unchecked(&self) -> &T {
    return &*(self.addr() as *mut T);
  }

  pub unsafe fn ref_mut_unchecked(&mut self) -> &mut T {
    return &mut *(self.addr() as *mut T);
  }

  pub fn as_addr(&self) -> Addr {
    return self.addr();
  }

  pub fn as_value(&self) -> u64 {
    return self.addr() as u64;
  }

  pub fn set(&mut self, data: T) {
    self.0 = data.into() as u64;
  }

  pub fn set_ptr(&mut self, data: Addr) {
    self.0 = data as u64;
  }

  pub fn set_value(&mut self, data: u64) {
    self.0 = data;
  }

  pub fn set_null(&mut self) {
    self.0 = std::ptr::null_mut::<T>() as u64;
  }

  pub fn set_flag(&mut self) {
    self.0 = self.0 | 0x1;
  }

  pub fn get_flag(&self) -> bool {
    return self.0 & 1 == 1;
  }

  fn addr(&self) -> Addr {
    return ((self.0 >> 1) << 1) as Addr;
  }
}

impl<T: Copy + From<Addr> + Into<Addr>> From<Addr> for BareHeapLayout<T> {
  fn from(a: Addr) -> BareHeapLayout<T> {
    return BareHeapLayout::<T>(a as u64, PhantomData);
  }
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct HeapLayout<T: Copy>(Addr, PhantomData<T>);

#[derive(Copy, Clone)]
pub struct VoidHeapBody {}

impl<T: Copy> HeapLayout<T> {
  pub fn new(context: &mut impl AllocationOnlyContext, size: usize, shape: Shape) -> HeapLayout<T> {
    return HeapLayout::<T>(Cell::init_heap(context, size, shape), PhantomData);
  }

  pub fn new_into_heap(heap: Addr, size: usize, shape: Shape) -> HeapLayout<T> {
    return HeapLayout::<T>(Cell::init_into_heap(heap, size, shape), PhantomData);
  }

  pub fn persist(context: &mut impl AllocationOnlyContext, size: usize, shape: Shape) -> HeapLayout<T> {
    return HeapLayout::<T>(Cell::init_persistent_heap(context, size, shape), PhantomData);
  }

  pub fn wrap(heap: Addr) -> HeapLayout<T> {
    return HeapLayout::<T>(heap, PhantomData);
  }

  pub fn null() -> HeapLayout<T> {
    return HeapLayout::<T>(std::ptr::null_mut(), PhantomData);
  }

  pub fn is_null(&self) -> bool {
    return self.0.is_null();
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

  pub fn set(this: &HeapLayout<T>, value: &T) {
    unsafe {
      *(this.0.offset(Cell::SIZE as isize) as *mut T) = *value;
    };
  }

  pub fn size(this: &HeapLayout<T>) -> usize {
    return this.cell().get_header().size();
  }

  pub fn cell(&self) -> Cell {
    return Cell::from(self.0);
  }

  pub fn shape(&self) -> Shape {
    return self.cell().get_header().shape();
  }
}

impl<T: Copy> Deref for HeapLayout<T> {
  type Target = T;
  fn deref(&self) -> &T {
    return self.as_ref();
  }
}

impl<T: Copy> DerefMut for HeapLayout<T> {
  fn deref_mut(&mut self) -> &mut T {
    return self.as_ref_mut();
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

pub trait HeapObject: Into<Repr> + Copy + From<Addr> {
  fn raw_heap(&self) -> Addr;
  fn is_js_value(&self) -> bool {
    return Shape::is_js_value(self.cell().get_header().shape());
  }

  fn size(&self) -> usize {
    return self.cell().get_header().size();
  }

  fn shape(&self) -> Shape {
    return self.cell().get_header().shape();
  }

  fn cell(&self) -> Cell {
    return Cell::from(self.raw_heap());
  }

  fn is_out(&self) -> bool {
    return self.raw_heap() == std::ptr::null_mut();
  }

  fn is_same_heap_object(&self, other: impl HeapObject) -> bool {
    return self.raw_heap() == other.raw_heap();
  }
}

macro_rules! _priv_impl_heap_object_body {
  () => {
    fn raw_heap(&self) -> crate::def::Addr {
      return self.0.as_addr();
    }
  };
}

macro_rules! impl_heap_object {
  ($name:ident) => {
    impl crate::structs::HeapObject for $name {
      _priv_impl_heap_object_body!();
    }
  };
  ($name:ident<$($type:tt : $bound:ident),+$(,)?>) => {
    impl<$($type : $bound),*> HeapObject for $name<$($type,)*> {
      _priv_impl_heap_object_body!();
    }
  };
}

macro_rules! impl_from_addr {
  ($name:tt, $layout:tt) => {
    impl From<crate::def::Addr> for $name {
      fn from(a: crate::def::Addr) -> $name {
        return $name($layout::wrap(a));
      }
    }
  };
  ($name:ident<$($type:tt : $bound:ident),+$(,)?>, $layout:tt, $($args:expr),+) => {
    impl<$($type : $bound),*> From<crate::def::Addr> for $name<$($type,)*> {
      fn from(a: crate::def::Addr) -> Self {
        return Self($layout::wrap(a), $($args ,)*);
      }
    }
  };
  ($name:ident<$($type:tt : $bound:ident),+$(,)?>, $layout:tt) => {
    impl<$($type : $bound),*> From<crate::def::Addr> for $name<$($type,)*> {
      fn from(a: crate::def::Addr) -> Self {
        return Self($layout::wrap(a));
      }
    }
  }
}

macro_rules! impl_into_addr {
  ($name:tt) => {
    impl Into<crate::def::Addr> for $name {
      fn into(self) -> crate::def::Addr {
        return self.0.as_addr();
      }
    }
  };
  ($name:ident<$($type:tt : $bound:ident),+$(,)?>) => {
    impl<$($type: $bound),*> Into<crate::def::Addr> for $name<$($type,)*> {
      fn into(self) -> crate::def::Addr {
        return self.0.as_addr();
      }
    }
  };
}

macro_rules! impl_deref_heap {
  ($name:tt, $layout:ident, $body:ty) => {
    impl std::ops::Deref for $name {
      type Target = $layout<$body>;
      fn deref(&self) -> &$layout<$body> {
        return &self.0;
      }
    }

    impl std::ops::DerefMut for $name {
      fn deref_mut(&mut self) -> &mut $layout<$body> {
        return &mut self.0;
      }
    }
  };
  ($name:ident<$($type:tt: $bound:ident),+$(,)?>, $layout:ident, $body:ty) => {
    impl<$($type : $bound),*> std::ops::Deref for $name<$($type,)+> {
      type Target = $layout<$body>;
      fn deref(&self) -> &$layout<$body> {
        return &self.0;
      }
    }
    impl<$($type : $bound,)*> std::ops::DerefMut for $name<$($type,)+> {
      fn deref_mut(&mut self) -> &mut $layout<$body> {
        return &mut self.0;
      }
    }
  };
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct Cell(BareHeapLayout<Header>);

impl Cell {
  pub const TYPE: Shape = Shape::cell();
  pub const SIZE: usize = size_of::<Header>();
  pub fn init_heap(context: &mut impl AllocationOnlyContext, size: usize, shape: Shape) -> Addr {
    let heap = context.allocate(size);
    return Cell::init_into_heap(heap, size, shape);
  }

  pub fn init_persistent_heap(context: &mut impl AllocationOnlyContext, size: usize, shape: Shape) -> Addr {
    let heap = context.allocate_persist(size);
    return Cell::init_into_heap(heap, size, shape);
  }

  pub fn init_into_heap(heap: Addr, size: usize, shape: Shape) -> Addr {
    let mut layout = BareHeapLayout::<Header>::wrap(heap);
    let header = unsafe { layout.ref_mut_unchecked() };
    header.init();
    header.set_size(size);
    header.set_shape(shape);
    return heap;
  }

  pub fn get_header(&self) -> &Header {
    return unsafe { self.0.ref_unchecked() };
  }

  pub fn get_header_mut(&mut self) -> &mut Header {
    return unsafe { self.0.ref_mut_unchecked() };
  }

  pub fn get_body(&self) -> Addr {
    return unsafe { self.0.as_addr().offset(Cell::SIZE as isize) };
  }
}

macro_rules! impl_bare_object {
  ($name:ident, $layout:ident<$body:ty>) => {
    impl_from_addr!($name, $layout);
    impl_into_addr!($name);
    impl_heap_object!($name);
    impl_repr_convertion!($name, $layout);
  };
}

macro_rules! impl_object {
  ($name:ident, $layout:ident<$body:ty>) => {
    impl_from_addr!($name, $layout);
    impl_into_addr!($name);
    impl_heap_object!($name);
    impl_repr_convertion!($name, $layout);
    impl_deref_heap!($name, $layout, $body);
  };
  ($name:ident<$($type:tt : $bound:ident),+$(,)?>, $layout:ident<$body:ty>) => {
    impl_from_addr!($name<$($type: $bound),+>, $layout);
    impl_into_addr!($name<$($type: $bound),+>);
    impl_heap_object!($name<$($type: $bound),+>);
    impl_repr_convertion!($name<$($type: $bound),+>,$layout);
    impl_deref_heap!($name<$($type: $bound),+>, $layout, $body);
  };
  ($name:ident<$($type:tt : $bound:ident),+$(,)?>, $layout:ident<$body:ty>, $($args:expr),+) => {
    impl_from_addr!($name<$($type: $bound),+>, $layout, $($args),*);
    impl_into_addr!($name<$($type: $bound),+>);
    impl_heap_object!($name<$($type: $bound),+>);
    impl_repr_convertion!($name<$($type: $bound),+>,$layout, $($args),*);
    impl_deref_heap!($name<$($type: $bound),+>, $layout, $body);
  };
}

impl_bare_object!(Cell, BareHeapLayout<Header>);
