use super::cell::{HeapLayout, HeapObject};
use super::repr::*;
use super::shape::Shape;
use crate::context::AllocationOnlyContext;
use crate::def::*;
use crate::utility::align;
use crate::utility::Len;
use std::marker::PhantomData;
use std::mem::size_of;
use std::ops::{Index, IndexMut};

macro_rules! fixed_array {
  (type: $t:ty, context: $context:expr, capacity: $capacity:expr, $($exp:expr),*) => {{
    let mut array = crate::structs::InternalArray::<$t>::new($context, $capacity);
    $(
      {array.push($exp)};
    )*
    array
  }}
}

pub trait InternalArrayElement: Copy {}
impl<T: Copy> InternalArrayElement for T {}

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct InternalArrayLayout {
  capacity: usize,
  length: usize,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct InternalArray<T: InternalArrayElement>(HeapLayout<InternalArrayLayout>, PhantomData<T>);
impl_object!(
  InternalArray<T: InternalArrayElement>,
  HeapLayout<InternalArrayLayout>,
  PhantomData
);

impl<T: InternalArrayElement> Index<usize> for InternalArray<T> {
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

impl<T: Copy> Len for InternalArray<T> {
  fn len(&self) -> usize {
    return self.length;
  }
}

impl<T: Copy> InternalArray<T> {
  pub const TYPE: Shape = Shape::internal_array();
  pub fn new(context: impl AllocationOnlyContext, capacity: usize) -> InternalArray<T> {
    return InternalArray::<T>::init(
      HeapLayout::<InternalArrayLayout>::new(
        context,
        InternalArray::<T>::calc_size(capacity),
        Shape::internal_array(),
      ),
      capacity,
    );
  }

  pub fn construct(
    context: impl AllocationOnlyContext,
    capacity: usize,
    length: usize,
    data: *mut T,
  ) -> InternalArray<T> {
    let mut array = InternalArray::<T>::init(
      HeapLayout::<InternalArrayLayout>::new(
        context,
        InternalArray::<T>::calc_size(PTR_SIZE),
        Shape::internal_array(),
      ),
      capacity,
    );
    array.set_data(data);
    array.length = length;
    return array;
  }

  pub fn wrap(heap: Addr) -> InternalArray<T> {
    return InternalArray::<T>(HeapLayout::<InternalArrayLayout>::wrap(heap), PhantomData);
  }

  pub fn calc_size(capacity: usize) -> usize {
    return align(size_of::<InternalArrayLayout>() + capacity * size_of::<T>(), ALIGNMENT);
  }

  pub fn slice(&self, context: impl AllocationOnlyContext, start: usize, end: usize) -> InternalArray<T> {
    assert!(end > start);
    assert!(end - start > 0);
    let mut a = InternalArray::<T>::new(context, end - start);
    let actual_end = if end > self.length() { self.length() } else { end };
    for i in start..actual_end {
      a.push(*self.at(i));
    }
    return a;
  }

  pub fn split(&self, context: impl AllocationOnlyContext, index: usize) -> (InternalArray<T>, InternalArray<T>) {
    assert!(index < self.capacity);
    let left_cap = self.capacity - index;
    let mut left = InternalArray::<T>::new(context, left_cap);
    let mut right = InternalArray::<T>::new(context, self.capacity - left_cap);
    for i in 0..left_cap {
      left.push(*self.at(i));
    }
    for i in left_cap..self.capacity {
      right.push(*self.at(i));
    }
    return (left, right);
  }

  pub fn concat(&self, context: impl AllocationOnlyContext, array: InternalArray<T>) -> InternalArray<T> {
    let ret = InternalArray::<T>::new(context, self.length() + array.length());
    unsafe {
      std::ptr::copy_nonoverlapping(self.data(), ret.data(), self.length());
      std::ptr::copy_nonoverlapping(array.data(), ret.data().offset(self.length() as isize), array.length());
    };
    return ret;
  }

  pub fn append(&mut self, array: InternalArray<T>) {
    assert!(array.length() + self.length() < self.capacity());
    let len = self.length;
    self.length = 0;
    unsafe {
      std::ptr::copy_nonoverlapping(array.data(), self.data().offset(len as isize), array.length());
    };
    self.length = len + array.length();
  }

  pub fn push(&mut self, data: T) {
    self.write(self.length(), data);
    self.length = self.length + 1;
  }

  pub fn at(&self, index: usize) -> &T {
    debug_assert!(index < self.capacity());
    return unsafe { &*(self.data().offset(index as isize)) };
  }

  pub fn at_mut(&mut self, index: usize) -> &mut T {
    debug_assert!(index < self.capacity());
    return unsafe { &mut *(self.data().offset(index as isize)) };
  }

  pub fn write(&mut self, index: usize, data: T) {
    debug_assert!(self.capacity() > index, "Specified index exceeded array capacity");
    unsafe {
      let addr = self.data().offset(index as isize);
      *addr = data;
    };
  }

  pub fn fill(&mut self, value: T) {
    for i in 0..self.capacity() {
      self.write(i, value);
    }
  }

  pub fn capacity(&self) -> usize {
    return self.capacity;
  }

  pub fn length(&self) -> usize {
    return self.length;
  }

  pub fn set_length(&mut self, len: usize) {
    assert!(self.capacity > len);
    self.length = len;
  }

  pub fn data(&self) -> *mut T {
    return unsafe {
      self
        .cell()
        .get_body()
        .offset((size_of::<InternalArrayLayout>()) as isize) as *mut T
    };
  }

  fn set_data(&self, data: *mut T) {
    unsafe {
      let d = self
        .cell()
        .get_body()
        .offset((size_of::<InternalArrayLayout>()) as isize) as *mut T;
      *d = *data;
    };
  }

  fn init(layout: HeapLayout<InternalArrayLayout>, capacity: usize) -> InternalArray<T> {
    let mut array = InternalArray::<T>(layout, PhantomData);
    array.capacity = capacity;
    array.length = 0;
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
  use crate::context::LuxContext;

  #[test]
  fn push_test() {
    let context = LuxContext::new();
    let mut array = InternalArray::<TestObject>::new(context, 10);
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
    let context = LuxContext::new();
    let mut array = InternalArray::<TestObject>::new(context, 10);
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
    let context = LuxContext::new();
    let mut array = InternalArray::<TestObject>::new(context, 10);
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
