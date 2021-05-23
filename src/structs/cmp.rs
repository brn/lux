use super::cell::{Cell, HeapObject};
use super::errors::JsTypeError;
use super::repr::Repr;
use super::shape::Shape;
use crate::context::Context;

#[inline]
pub fn require_object_coercible(context: &mut impl Context, o: Repr, fn_name: &str) -> Result<Repr, JsTypeError> {
  if o.is_boxed() {
    let cell = Cell::from(o);
    if cell.shape() == Shape::null() || cell.shape() == Shape::undefined() {
      let str = format!("{} Called null or undefined", fn_name);
      return Err(JsTypeError::from_utf8(context, &str));
    }
    return Ok(o);
  }
  return Ok(o);
}
