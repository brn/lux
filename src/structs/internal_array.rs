use super::cell::{field_addr, Cell, HeapObject};
use super::repr::Repr;
use super::shape::Shape;
use crate::context::Context;
use crate::def::*;
use crate::impl_repr_convertion;
use crate::utility::align;
use std::boxed::Box;
use std::marker::PhantomData;
use std::mem::size_of;
use std::ops::{Index, IndexMut};

#[repr(C)]
#[derive(Copy, Clone)]
struct InternalArray<T: HeapObject> {
  heap: Addr,
  _marker: PhantomData<T>,
}
impl_heap_object!(InternalArray<T: HeapObject>);
impl_repr_convertion!(InternalArray<T: HeapObject>);

impl<T: HeapObject> Index<usize> for InternalArray<T> {
  type Output = T;
  fn index(&self, index: usize) -> &Self::Output {
    return &self.at(index);
  }
}

impl<T: HeapObject> IndexMut<usize> for InternalArray<T> {
  fn index_mut(&mut self, index: usize) -> &mut T {
    return self.at_mut(index);
  }
}

const CAPACITY_ADDR_OFFSET: usize = Cell::SIZE;
const LENGTH_ADDR_OFFSET: usize = CAPACITY_ADDR_OFFSET + size_of::<usize>();
const DATA_ADDR_OFFSET: usize = LENGTH_ADDR_OFFSET + size_of::<usize>();

impl<T: HeapObject> InternalArray<T> {
  pub const TYPE: Shape = Shape::internal_array();
  pub fn new(context: &mut Context, capacity: usize) -> InternalArray<T> {
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

  pub fn new_into_heap(heap: *mut Byte, capacity: usize) -> InternalArray<T> {
    debug_assert!(capacity > 0, "InternalArray size must be greather than 0");
    return InternalArray::<T>::init(
      Cell::new_into_heap(heap, InternalArray::<T>::calc_size(capacity), Shape::internal_array()),
      capacity,
    );
  }

  pub fn wrap(heap: Addr) -> InternalArray<T> {
    return InternalArray::<T> {
      heap,
      _marker: PhantomData,
    };
  }

  pub fn calc_size(capacity: usize) -> usize {
    return align(Cell::SIZE + (size_of::<usize>() * 2) + (capacity * PTR_SIZE), ALIGNMENT);
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

  fn set_capacity(&self, capacity: usize) {
    unsafe { *(field_addr(self.heap, CAPACITY_ADDR_OFFSET) as *mut usize) = capacity };
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

struct InternalArrayIterator<T: HeapObject> {
  array: InternalArray<T>,
  index: usize,
}

impl<T: HeapObject> IntoIterator for InternalArray<T> {
  type Item = T;
  type IntoIter = InternalArrayIterator<T>;

  fn into_iter(self) -> Self::IntoIter {
    return InternalArrayIterator::<T> { array: self, index: 0 };
  }
}

impl<T: HeapObject> Iterator for InternalArrayIterator<T> {
  type Item = T;
  fn next(&mut self) -> Option<Self::Item> {
    if self.index >= self.array.length() {
      return None;
    }
    let result = T::from_ptr(self.array[self.index].raw_heap());
    self.index += 1;
    return Some(result);
  }
}

#[cfg(test)]
mod internal_array_test {
  use super::*;
  use std::alloc::{alloc, Layout};

  #[derive(Copy, Clone)]
  struct Test {
    heap: Addr,
  }
  impl_heap_object!(Test);
  impl_repr_convertion!(Test);
  impl Test {
    pub const TYPE: Shape = Shape::boolean();
    pub fn new(value: u8) -> Test {
      let heap = unsafe { alloc(Layout::from_size_align(Cell::SIZE + size_of::<u8>(), ALIGNMENT).unwrap()) };
      let h = Cell::new_into_heap(heap, Cell::SIZE + size_of::<u8>(), Shape::undefined());
      let body = h.get_body();
      unsafe {
        *body = value;
      };
      return Test { heap: h.raw_heap() };
    }
    pub fn value(&self) -> u8 {
      return unsafe { *Cell::from_ptr(self.heap).get_body() };
    }
    fn byte_length(&self) -> usize {
      return size_of::<u8>();
    }
    fn wrap(heap: Addr) -> Test {
      return Test { heap };
    }
  }

  #[test]
  fn push_test() {
    let layout = Layout::from_size_align(InternalArray::<Test>::calc_size(10), ALIGNMENT).unwrap();
    let heap = unsafe { alloc(layout) };
    let mut array = InternalArray::<Test>::new_into_heap(heap, 10);
    assert_eq!(array.length(), 0);
    assert_eq!(array.capacity(), 10);
    for i in 0..10 {
      let p = Test::new(i);
      array.push(p);
    }
    for i in 0..10 {
      let t = array[i];
      assert_eq!(t.value(), i as u8);
    }
    assert_eq!(array.length(), 10);
    assert_eq!(array.capacity(), 10);
  }
}
