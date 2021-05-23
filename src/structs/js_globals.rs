use super::cell::*;
use super::convs::AbstractConvs;
use super::repr::Repr;
use super::shape::*;
use super::string::JsString;
use crate::context::Context;
use crate::def::*;
use std::mem::size_of;

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct JsUndefined(HeapLayout<VoidHeapBody>);
impl_object!(JsUndefined, HeapLayout<VoidHeapBody>);

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

impl AbstractConvs for JsUndefined {
  fn to_string(&self, context: &mut impl Context) -> JsString {
    return context.undefined_str();
  }

  fn to_number(&self, _: &mut impl Context) -> Result<Repr, Repr> {
    return Err(Repr::from_nan());
  }
}

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct JsNull(HeapLayout<VoidHeapBody>);
impl_object!(JsNull, HeapLayout<VoidHeapBody>);

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

impl AbstractConvs for JsNull {
  fn to_string(&self, context: &mut impl Context) -> JsString {
    return context.null_str();
  }

  fn to_number(&self, _: &mut impl Context) -> Result<Repr, Repr> {
    return Err(Repr::from_nan());
  }
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct JsBooleanLayout {
  value: u8,
}

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct JsBoolean(HeapLayout<JsBooleanLayout>);
impl_object!(JsBoolean, HeapLayout<JsBooleanLayout>);

impl JsBoolean {
  pub const TYPE: Shape = Shape::boolean();
  pub const SIZE: usize = Cell::SIZE + size_of::<JsBooleanLayout>();

  pub fn persist<'a>(context: &mut impl Context, val: bool) -> JsBoolean {
    let layout = HeapLayout::<JsBooleanLayout>::persist(context, JsBoolean::SIZE, Shape::boolean());
    return JsBoolean::init(layout, val);
  }

  pub fn is_true(&self) -> bool {
    return self.0.as_ref().value == 1;
  }

  pub fn wrap(heap: Addr) -> JsBoolean {
    return JsBoolean(HeapLayout::<JsBooleanLayout>::wrap(heap));
  }

  #[inline]
  fn init(mut layout: HeapLayout<JsBooleanLayout>, val: bool) -> JsBoolean {
    layout.value = if val { 1 } else { 0 };
    debug_assert!(HeapLayout::size(&layout) == JsBoolean::SIZE);
    return JsBoolean(layout);
  }
}

impl AbstractConvs for JsBoolean {
  fn to_string(&self, context: &mut impl Context) -> JsString {
    return if self.is_true() {
      context.true_str()
    } else {
      context.false_str()
    };
  }

  fn to_number(&self, _: &mut impl Context) -> Result<Repr, Repr> {
    return if self.is_true() {
      Ok(Repr::from(1.0_f64))
    } else {
      Ok(Repr::from(0.0_f64))
    };
  }
}

#[cfg(test)]
mod js_global_test {
  use super::*;

  use crate::context::isolate;

  #[test]
  fn js_boolean_init_test() {
    let mut mc = isolate::context();
    let js_true = JsBoolean::persist(&mut mc, true);
    assert_eq!(js_true.size(), JsBoolean::SIZE);
    assert_eq!(js_true.shape(), Shape::boolean());
  }

  #[test]
  fn js_boolean_true_test() {
    let mut mc = isolate::context();
    let js_true = JsBoolean::persist(&mut mc, true);
    assert_eq!(js_true.is_true(), true);
  }

  #[test]
  fn js_boolean_false_test() {
    let mut mc = isolate::context();
    let js_true = JsBoolean::persist(&mut mc, false);
    assert_eq!(js_true.is_true(), false);
  }
}
