use super::cell::*;
use super::repr::*;
use crate::def::*;
use crate::impl_heap_object;
use crate::impl_repr_convertion;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

#[cfg(test)]
pub mod testing {
  use super::super::cell::{Cell, HeapObject};
  use super::super::repr::Repr;
  use super::super::shape::Shape;
  use crate::def::*;
  use crate::impl_heap_object;
  use crate::impl_repr_convertion;
  use std::alloc::{alloc, Layout};
  use std::mem::size_of;

  #[derive(Copy, Clone)]
  pub struct TestObject {
    heap: Addr,
  }
  impl_heap_object!(TestObject);
  impl_repr_convertion!(TestObject);

  impl TestObject {
    pub const TYPE: Shape = Shape::boolean();
    pub fn new(value: u8) -> TestObject {
      let heap = unsafe { alloc(Layout::from_size_align(Cell::SIZE + size_of::<u8>(), ALIGNMENT).unwrap()) };
      let h = Cell::new_into_heap(heap, Cell::SIZE + size_of::<u8>(), Shape::undefined());
      let body = h.get_body();
      unsafe {
        *body = value;
      };
      return TestObject { heap: h.raw_heap() };
    }
    pub fn value(&self) -> u8 {
      return unsafe { *Cell::from_ptr(self.heap).get_body() };
    }
    pub fn set_value(&self, value: u8) {
      unsafe { (*Cell::from_ptr(self.heap).get_body()) = value }
    }
    fn byte_length(&self) -> usize {
      return size_of::<u8>();
    }
    fn wrap(heap: Addr) -> TestObject {
      return TestObject { heap };
    }
  }
}

#[derive(Debug, PartialEq, FromPrimitive)]
enum WellKnownSymbolType {
  AsyncIterator,
  HasInstance,
  IsConcatSpreadable,
  Iterator,
  Match,
  MatchAll,
  Replace,
  Search,
  Species,
  Split,
  ToPrimitive,
  ToStringTag,
  Unscopables,
}

#[derive(Copy, Clone)]
pub struct JsSymbol {
  heap: Addr,
}

impl_heap_object!(JsSymbol);
impl_repr_convertion!(JsSymbol);

impl JsSymbol {
  const SIZE: usize = Cell::SIZE;
  fn byte_length(&self) -> usize {
    return JsSymbol::SIZE;
  }

  fn wrap(heap: Addr) -> JsSymbol {
    return JsSymbol { heap };
  }
}
