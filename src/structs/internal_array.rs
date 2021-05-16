use super::cell::{Cell, HeapLayout, HeapObject};
use super::repr::*;
use super::shape::Shape;
use crate::context::Context;
use crate::def::*;
use crate::utility::align;
use std::marker::PhantomData;
use std::mem::size_of;
use std::ops::{Index, IndexMut};

#[repr(C)]
#[derive(Copy, Clone)]
struct InternalArrayBody {
  capacity: usize,
  length: usize,
}

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct InternalArray<T: Copy>(HeapLayout<InternalArrayBody>, PhantomData<T>);

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

impl<T: Copy> InternalArray<T> {
  pub const TYPE: Shape = Shape::internal_array();
  pub fn new(context: &mut impl Context, capacity: usize) -> InternalArray<T> {
    debug_assert!(capacity > 0, "InternalArray size must be greather than 0");
    return InternalArray::<T>::init(
      HeapLayout::<InternalArrayBody>::new(
        context,
        InternalArray::<T>::calc_size(capacity),
        Shape::internal_array(),
      ),
      capacity,
    );
  }

  pub fn construct(context: &mut impl Context, capacity: usize, length: usize, data: *mut T) -> InternalArray<T> {
    let mut array = InternalArray::<T>::init(
      HeapLayout::<InternalArrayBody>::new(
        context,
        InternalArray::<T>::calc_size(PTR_SIZE),
        Shape::internal_array(),
      ),
      capacity,
    );
    array.set_data(data);
    return array;
  }

  #[cfg(test)]
  pub fn alloc_for_test(capacity: usize) -> InternalArray<T> {
    debug_assert!(capacity > 0, "InternalArray size must be greather than 0");
    use std::alloc::{alloc, Layout};
    let layout = Layout::from_size_align(InternalArray::<T>::calc_size(capacity), ALIGNMENT).unwrap();
    return InternalArray::<T>::init(
      HeapLayout::<InternalArrayBody>::new_into_heap(
        unsafe { alloc(layout) },
        InternalArray::<T>::calc_size(capacity),
        Shape::internal_array(),
      ),
      capacity,
    );
  }

  pub fn wrap(heap: Addr) -> InternalArray<T> {
    return InternalArray::<T>(HeapLayout::<InternalArrayBody>::wrap(heap), PhantomData);
  }

  pub fn calc_size(capacity: usize) -> usize {
    return align(
      Cell::SIZE + size_of::<InternalArrayBody>() + (capacity * PTR_SIZE),
      ALIGNMENT,
    );
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
      let addr = self.data().offset(index as isize);
      *addr = data;
    };
  }

  pub fn capacity(&self) -> usize {
    return self.0.as_ref().capacity;
  }

  pub fn length(&self) -> usize {
    return self.0.as_ref().length;
  }

  pub fn data(&self) -> *mut T {
    return unsafe {
      self
        .0
        .as_addr()
        .offset((Cell::SIZE + size_of::<InternalArrayBody>()) as isize) as *mut T
    };
  }

  fn set_data(&self, data: *mut T) {
    unsafe {
      let data = self
        .0
        .as_addr()
        .offset((Cell::SIZE + size_of::<InternalArrayBody>()) as isize) as *mut T;
      *data = *data;
    };
  }

  fn byte_length(&self) -> usize {
    return InternalArray::<T>::calc_size(self.capacity());
  }

  fn set_capacity(&mut self, capacity: usize) {
    self.0.as_ref_mut().capacity = capacity;
  }

  fn set_length(&mut self, length: usize) {
    self.0.as_ref_mut().length = length;
  }

  fn init(layout: HeapLayout<InternalArrayBody>, capacity: usize) -> InternalArray<T> {
    let mut array = InternalArray::<T>(layout, PhantomData);
    array.set_capacity(capacity);
    array.set_length(0);
    return array;
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

impl<T: Copy> std::fmt::Debug for InternalArray<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return write!(f, "{:?}", self.0);
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
      assert_eq!(t.value(), i as u32);
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
      array.write(i, TestObject::new((i as u32) * 2));
    }
    for i in 0..10 {
      let t = array[i];
      assert_eq!(t.value(), (i * 2) as u32);
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
      assert_eq!(v.value(), i as u32);
      i += 1;
    }
    assert_eq!(array.length(), 10);
    assert_eq!(array.capacity(), 10);
  }
}
