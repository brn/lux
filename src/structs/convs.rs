use super::cell::{Cell, HeapObject};
use super::errors::JsTypeError;
use super::js_globals::*;
use super::repr::Repr;
use super::shape::ShapeTag;
use super::string::JsString;
use crate::context::Context;

pub trait AbstractConvs {
  fn to_string(&self, context: &mut impl Context) -> JsString;
  fn to_number(&self, context: &mut impl Context) -> Result<Repr, Repr>;
}
