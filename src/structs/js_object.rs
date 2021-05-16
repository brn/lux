use super::cell::*;
use super::repr::*;
use crate::def::*;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

#[cfg(test)]
pub mod testing {
  use super::super::cell::*;
  use super::super::repr::Repr;
  use super::super::shape::Shape;
  use crate::def::*;
  use std::alloc::{alloc, Layout};
  use std::mem::size_of;

  #[repr(C)]
  #[derive(Copy, Clone)]
  struct TestObjectBody {
    value: u32,
  }

  #[repr(transparent)]
  #[derive(Copy, Clone)]
  pub struct TestObject(HeapLayout<TestObjectBody>);
  impl_from_addr!(TestObject, HeapLayout);
  impl_heap_object!(TestObject);
  impl_repr_convertion!(TestObject);

  impl TestObject {
    pub const TYPE: Shape = Shape::boolean();
    pub fn new(value: u32) -> TestObject {
      let heap = unsafe { alloc(Layout::from_size_align(Cell::SIZE + size_of::<u8>(), ALIGNMENT).unwrap()) };
      let mut layout =
        HeapLayout::<TestObjectBody>::new_into_heap(heap, Cell::SIZE + size_of::<u8>(), Shape::undefined());
      layout.as_ref_mut().value = value;
      return TestObject(layout);
    }
    pub fn value(&self) -> u32 {
      return self.0.as_ref().value;
    }
    pub fn set_value(&mut self, value: u32) {
      self.0.as_ref_mut().value = value;
    }
    fn byte_length(&self) -> usize {
      return size_of::<u8>();
    }

    fn wrap(heap: Addr) -> TestObject {
      return TestObject(HeapLayout::<TestObjectBody>::wrap(heap));
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

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct JsSymbol(HeapLayout<VoidHeapBody>);
impl_from_addr!(JsSymbol, HeapLayout);
impl_heap_object!(JsSymbol);
impl_repr_convertion!(JsSymbol);

impl JsSymbol {
  const SIZE: usize = Cell::SIZE;

  fn wrap(heap: Addr) -> JsSymbol {
    return JsSymbol(HeapLayout::<VoidHeapBody>::wrap(heap));
  }
}
