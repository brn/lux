use super::cell::*;
use super::repr::Repr;
use super::shape::*;
use crate::context::Context;
use crate::def::*;
use crate::utility::align;
use std::mem::size_of;

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct JsUndefined(HeapLayout<VoidHeapBody>);
impl_heap_object!(JsUndefined);
impl_repr_convertion!(JsUndefined);

impl JsUndefined {
  pub const TYPE: Shape = Shape::undefined();
  fn byte_length(&self) -> usize {
    return Cell::SIZE;
  }
  pub fn persist(context: &mut impl Context) -> JsUndefined {
    return JsUndefined(HeapLayout::<VoidHeapBody>::new(context, Cell::SIZE, Shape::undefined()));
  }
  pub fn wrap(heap: Addr) -> JsUndefined {
    return JsUndefined(HeapLayout::<VoidHeapBody>::wrap(heap));
  }
}

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct JsNull(HeapLayout<VoidHeapBody>);
impl_heap_object!(JsNull);
impl_repr_convertion!(JsNull);

impl JsNull {
  pub const TYPE: Shape = Shape::null();
  fn byte_length(&self) -> usize {
    return Cell::SIZE;
  }
  pub fn persist(context: &mut impl Context) -> JsNull {
    return JsNull(HeapLayout::<VoidHeapBody>::persist(context, Cell::SIZE, Shape::null()));
  }
  pub fn wrap(heap: Addr) -> JsNull {
    return JsNull(HeapLayout::<VoidHeapBody>::wrap(heap));
  }
}

#[repr(C)]
#[derive(Copy, Clone)]
struct JsBooleanBody {
  value: u8,
}

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct JsBoolean(HeapLayout<JsBooleanBody>);
impl_heap_object!(JsBoolean);
impl_repr_convertion!(JsBoolean);

impl JsBoolean {
  pub const TYPE: Shape = Shape::boolean();
  pub const SIZE: usize = Cell::SIZE + size_of::<JsBooleanBody>();

  pub fn persist<'a>(context: &mut impl Context, val: bool) -> JsBoolean {
    let layout = HeapLayout::<JsBooleanBody>::persist(context, JsBoolean::SIZE, Shape::boolean());
    return JsBoolean::init(layout, val);
  }

  pub fn is_true(&self) -> bool {
    return self.0.as_ref().value == 1;
  }

  pub fn wrap(heap: Addr) -> JsBoolean {
    return JsBoolean(HeapLayout::<JsBooleanBody>::wrap(heap));
  }

  #[inline]
  fn init(mut layout: HeapLayout<JsBooleanBody>, val: bool) -> JsBoolean {
    let body = layout.as_ref_mut();
    body.value = if val { 1 } else { 0 };
    debug_assert!(layout.size() == JsBoolean::SIZE);
    return JsBoolean(layout);
  }
}

#[cfg(test)]
mod js_global_test {
  use super::*;
  use crate::context::testing::*;

  #[test]
  fn js_boolean_init_test() {
    let mut mc = MockedContext::new();
    let js_true = JsBoolean::persist(&mut mc, true);
    assert_eq!(js_true.size(), JsBoolean::SIZE);
    assert_eq!(js_true.shape(), Shape::boolean());
  }

  #[test]
  fn js_boolean_true_test() {
    let mut mc = MockedContext::new();
    let js_true = JsBoolean::persist(&mut mc, true);
    assert_eq!(js_true.is_true(), true);
  }

  #[test]
  fn js_boolean_false_test() {
    let mut mc = MockedContext::new();
    let js_true = JsBoolean::persist(&mut mc, false);
    assert_eq!(js_true.is_true(), false);
  }
}
