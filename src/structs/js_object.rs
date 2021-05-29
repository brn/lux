use super::object::{JsFunction, Property, WellKnownSymbolType};
use crate::context::*;

#[cfg(test)]
pub mod testing {
  use super::super::cell::*;
  use super::super::repr::Repr;
  use super::super::shape::Shape;
  use crate::def::*;
  use std::alloc::{alloc, Layout};
  use std::mem::size_of;

  #[repr(C)]
  #[derive(Copy, Clone, Default)]
  pub struct TestObjectBody {
    value: u32,
  }

  #[repr(C)]
  #[derive(Copy, Clone)]
  pub struct TestObject(HeapLayout<TestObjectBody>);
  impl_object!(TestObject, HeapLayout<TestObjectBody>);

  impl TestObject {
    pub const TYPE: Shape = Shape::boolean();
    pub fn new(value: u32) -> TestObject {
      let heap = unsafe { alloc(Layout::from_size_align(size_of::<u8>(), ALIGNMENT).unwrap()) };
      let mut layout = HeapLayout::<TestObjectBody>::new_into_heap(heap, size_of::<u8>(), Shape::undefined());
      layout.value = value;
      return TestObject(layout);
    }
    pub fn value(&self) -> u32 {
      return self.0.value;
    }
    pub fn set_value(&mut self, value: u32) {
      self.value = value;
    }
    fn byte_length(&self) -> usize {
      return size_of::<u8>();
    }

    fn wrap(heap: Addr) -> TestObject {
      return TestObject(HeapLayout::<TestObjectBody>::wrap(heap));
    }
  }
}

pub struct Builtins {
  symbol_constructor: JsFunction,
}

impl Builtins {
  pub fn new(context: impl Context) {}

  //  fn init_object_prototype(context: impl Context) -> JsObject {}

  fn init_symbol_constructor(context: impl Context) -> JsFunction {
    let symbol_constructor_props = fixed_array!(
      type: Property,
      context: context,
      capacity: 13,
      new_property!(
        context,
        str: "asyncIterator",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::AsyncIterator)
          .into()
      ),
      new_property!(
        context,
        str: "hasInstance",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::HasInstance)
          .into()
      ),
      new_property!(
        context,
        str: "isConcatSpreadable",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::IsConcatSpreadable)
          .into()
      ),
      new_property!(
        context,
        str: "iterator",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::Iterator)
          .into()
      ),
      new_property!(
        context,
        str: "match",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::Match)
          .into()
      ),
      new_property!(
        context,
        str: "matchAll",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::MatchAll)
          .into()
      ),
      new_property!(
        context,
        str: "replace",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::Replace)
          .into()
      ),
      new_property!(
        context,
        str: "search",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::Search)
          .into()
      ),
      new_property!(
        context,
        str: "species",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::Species)
          .into()
      ),
      new_property!(
        context,
        str: "split",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::Split)
          .into()
      ),
      new_property!(
        context,
        str: "toPrimitive",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::ToPrimitive)
          .into()
      ),
      new_property!(
        context,
        str: "toStringTag",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::ToStringTag)
          .into()
      ),
      new_property!(
        context,
        str: "unscopables",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::Unscopables)
          .into()
      )
    );
    return JsFunction::new(context, symbol_constructor_props);
  }
}
