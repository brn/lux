use super::cell::{field_addr, Cell, HeapLayout, HeapObject};
use super::repr::Repr;
use super::shape::Shape;
use crate::context::Context;
use crate::def::*;
use crate::impl_repr_convertion;
use crate::utility::align;
use std::marker::PhantomData;
use std::mem::size_of;
use std::ops::{Index, IndexMut};

#[repr(C)]
#[derive(Copy, Clone)]
struct InternalArrayBody<T: Copy> {
  capacity: usize,
  length: usize,
  data: HeapLayout<T>,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct InternalArray<T: Copy> {
  heap: HeapLayout<InternalArrayBody<T>>,
  _marker: PhantomData<T>,
}
impl<T: Copy> HeapObject for InternalArray<T> {
  impl_heap_object!();
}
impl<T: Copy> From<InternalArray<T>> for Repr {
  impl_repr_convertion!(InternalArray<T>);
}

impl<T: Copy> Index<usize> for InternalArray<T> {
  type Output = T;
  fn index(&self, index: usize) -> &Self::Output {
    return &self.at(index);
  }
}

impl<T: Copy> IndexMut<usize> for InternalArray<T> {
  fn index_mut(&mut self, index: usize) -> &mut T {
    return self.at_mut(index);
  }
}

const CAPACITY_ADDR_OFFSET: usize = Cell::SIZE;
const LENGTH_ADDR_OFFSET: usize = CAPACITY_ADDR_OFFSET + size_of::<usize>();
const DATA_ADDR_OFFSET: usize = LENGTH_ADDR_OFFSET + size_of::<usize>();

impl<T: Copy> InternalArray<T> {
  pub const TYPE: Shape = Shape::internal_array();
  pub fn new(context: &mut impl Context, capacity: usize) -> InternalArray<T> {
    debug_assert!(capacity > 0, "InternalArray size must be greather than 0");
    return InternalArray::<T>::init(
      Cell::new(
        context,
        InternalArray::<T>::calc_size(capacity),
        Shape::internal_array(),
      ),
      capacity,
    );
  }

  #[cfg(test)]
  pub fn alloc_for_test(capacity: usize) -> InternalArray<T> {
    debug_assert!(capacity > 0, "InternalArray size must be greather than 0");
    use std::alloc::{alloc, Layout};
    let layout = Layout::from_size_align(InternalArray::<T>::calc_size(capacity), ALIGNMENT).unwrap();
    return InternalArray::<T>::init(
      Cell::new_into_heap(
        unsafe { alloc(layout) },
        InternalArray::<T>::calc_size(capacity),
        Shape::internal_array(),
      ),
      capacity,
    );
  }

  pub fn wrap(heap: Addr) -> InternalArray<T> {
    return InternalArray::<T> {
      heap: HeapLayout::<InternalArrayBody<T>>::new(heap),
      _marker: PhantomData,
    };
  }

  pub fn calc_size(capacity: usize) -> usize {
    return align(Cell::SIZE + (size_of::<usize>() * 2) + (capacity * PTR_SIZE), ALIGNMENT);
  }

  pub fn slice(&self, context: &mut impl Context, start: usize, end: usize) -> InternalArray<T> {
    debug_assert!(end < self.capacity() - 1);
    debug_assert!(end - start > 0);
    let mut a = InternalArray::<T>::new(context, end - start);
    for i in start..end {
      a.push(*self.at(i));
    }
    return a;
  }

  pub fn push(&mut self, data: T) {
    self.write(self.length(), data);
    self.set_length(self.length() + 1);
  }

  pub fn at(&self, index: usize) -> &T {
    debug_assert!(index < self.capacity());
    return unsafe { &*(self.data().offset(index as isize) as *mut T) };
  }

  pub fn at_mut(&mut self, index: usize) -> &mut T {
    debug_assert!(index < self.capacity());
    return unsafe { &mut *(self.data().offset(index as isize) as *mut T) };
  }

  pub fn write(&mut self, index: usize, data: T) {
    debug_assert!(self.capacity() > index, "Specified index exceeded array capacity");
    unsafe {
      let addr = self.data().offset(index as isize) as *mut T;
      *addr = data;
    };
  }

  pub fn capacity(&self) -> usize {
    return unsafe { *(field_addr(self.heap, CAPACITY_ADDR_OFFSET) as *mut usize) };
  }

  pub fn length(&self) -> usize {
    return unsafe { *(field_addr(self.heap, LENGTH_ADDR_OFFSET) as *mut usize) };
  }

  fn byte_length(&self) -> usize {
    return InternalArray::<T>::calc_size(self.capacity());
  }

  fn set_capacity(&mut self, capacity: usize) {
    self.heap.as_ref_mut().capacity = capacity;
  }

  fn set_length(&self, length: usize) {
    unsafe { *(field_addr(self.heap, LENGTH_ADDR_OFFSET) as *mut usize) = length };
  }

  fn init(cell: Cell, capacity: usize) -> InternalArray<T> {
    let array = InternalArray::<T>::wrap(cell.raw_heap());
    array.set_capacity(capacity);
    array.set_length(0);
    return array;
  }

  fn data(&self) -> *mut *mut UintPtr {
    return field_addr(self.heap, DATA_ADDR_OFFSET) as *mut *mut UintPtr;
  }
}

pub struct InternalArrayIterator<T: Copy> {
  array: InternalArray<T>,
  index: usize,
}

impl<T: Copy> Iterator for InternalArrayIterator<T> {
  type Item = T;
  fn next(&mut self) -> Option<Self::Item> {
    if self.index >= self.array.length() {
      return None;
    }
    let result = self.array[self.index];
    self.index += 1;
    return Some(result);
  }
}

impl<T: Copy> IntoIterator for InternalArray<T> {
  type Item = T;
  type IntoIter = InternalArrayIterator<T>;

  fn into_iter(self) -> Self::IntoIter {
    return InternalArrayIterator::<T> { array: self, index: 0 };
  }
}

#[cfg(test)]
mod internal_array_test {
  use super::super::js_object::testing::*;
  use super::*;

  #[test]
  fn push_test() {
    let mut array = InternalArray::<TestObject>::alloc_for_test(10);
    assert_eq!(array.length(), 0);
    assert_eq!(array.capacity(), 10);
    for i in 0..10 {
      let p = TestObject::new(i);
      array.push(p);
    }
    for i in 0..10 {
      let t = array[i];
      assert_eq!(t.value(), i as u8);
    }
    assert_eq!(array.length(), 10);
    assert_eq!(array.capacity(), 10);
  }

  #[test]
  fn write_test() {
    let mut array = InternalArray::<TestObject>::alloc_for_test(10);
    assert_eq!(array.length(), 0);
    assert_eq!(array.capacity(), 10);
    for i in 0..10 {
      let p = TestObject::new(i);
      array.push(p);
    }
    for i in 0..10 {
      array.write(i, TestObject::new((i as u8) * 2));
    }
    for i in 0..10 {
      let t = array[i];
      assert_eq!(t.value(), (i * 2) as u8);
    }
    assert_eq!(array.length(), 10);
    assert_eq!(array.capacity(), 10);
  }

  #[test]
  fn iterator_test() {
    let mut array = InternalArray::<TestObject>::alloc_for_test(10);
    assert_eq!(array.length(), 0);
    assert_eq!(array.capacity(), 10);
    for i in 0..10 {
      let p = TestObject::new(i);
      array.push(p);
    }
    let mut i = 0;
    for v in array {
      assert_eq!(v.value(), i as u8);
      i += 1;
    }
    assert_eq!(array.length(), 10);
    assert_eq!(array.capacity(), 10);
  }
}
