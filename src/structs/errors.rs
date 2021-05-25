use super::cell::*;
use super::shape::Shape;
use super::string::JsString;
use crate::context::Context;
use std::mem::size_of;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct JsErrorLayout {
  message: BareHeapLayout<JsString>,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct JsError(HeapLayout<JsErrorLayout>);
impl_object!(JsError, HeapLayout<JsErrorLayout>);

impl JsError {
  const SIZE: usize = Cell::SIZE + size_of::<JsErrorLayout>();
  pub fn new(context: &mut impl Context, message: JsString) -> JsError {
    let mut layout = HeapLayout::<JsErrorLayout>::new(context, JsError::SIZE, Shape::error());
    layout.message.set(message);
    return JsError(layout);
  }
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct JsTypeErrorLayout {
  message: BareHeapLayout<JsString>,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct JsTypeError(HeapLayout<JsTypeErrorLayout>);
impl_object!(JsTypeError, HeapLayout<JsTypeErrorLayout>);

impl JsTypeError {
  const SIZE: usize = Cell::SIZE + size_of::<JsTypeErrorLayout>();
  pub fn new(context: &mut impl Context, message: JsString) -> JsTypeError {
    let mut layout = HeapLayout::<JsTypeErrorLayout>::new(context, JsTypeError::SIZE, Shape::type_error());
    layout.message.set(message);
    return JsTypeError(layout);
  }

  pub fn from_utf8(context: &mut impl Context, message: &str) -> JsTypeError {
    let m = JsString::from_utf8(context, message);
    return JsTypeError::new(context, m);
  }
}
