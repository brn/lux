use super::super::cell::HeapObject;
use super::super::repr::Repr;
use super::string::JsString;
use super::u16_str::FixedU16CodePointArray;
use crate::context::Context;

pub trait StringBackend: HeapObject + From<Repr> {
  fn at(&self, index: usize) -> Option<u16>;

  fn concat(&self, context: &mut impl Context, str: Repr) -> JsString;

  fn slice(&self, context: &mut impl Context, start_index: usize, end_index: usize) -> FixedU16CodePointArray;

  fn flatten(&mut self, context: &mut impl Context) -> FixedU16CodePointArray;

  fn len(&self) -> usize;
}
