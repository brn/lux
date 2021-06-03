use super::super::cell::HeapObject;
use super::super::repr::Repr;
use super::string::JsString;
use super::u16_str::FixedU16CodePointArray;
use crate::context::{Context, ObjectRecordsInitializedContext};

pub trait StringBackend: HeapObject + From<Repr> {
  fn at(&self, index: usize) -> Option<u16>;

  fn concat(&self, context: impl Context, str: Repr) -> JsString;

  fn slice(&self, context: impl Context, start_index: usize, end_index: usize) -> FixedU16CodePointArray;

  fn flatten(&mut self, context: impl ObjectRecordsInitializedContext) -> FixedU16CodePointArray;

  fn len(&self) -> usize;
}
