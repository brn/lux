use super::super::cell::*;
use super::super::repr::Repr;
use super::super::shape::ShapeTag;
use super::function::JsFunction;
use crate::utility::*;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct JsReceiver(Repr);

impl JsReceiver {
  pub fn new(repr: Repr) -> JsReceiver {
    assert!(repr.is_heap_object() && repr.is_object());
    return JsReceiver(repr);
  }

  pub fn is_function(&self) -> bool {
    return self.0.is_function();
  }

  pub fn to_function(&self) -> JsFunction {
    debug_assert!(self.is_function());
    return JsFunction::from(self.0);
  }

  pub fn is_object_objects(&self) -> bool {
    return self.0.is_object_type();
  }

  pub fn object(&self) -> Repr {
    return self.0;
  }
}
