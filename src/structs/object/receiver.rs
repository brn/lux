use super::super::cell::*;
use super::super::repr::Repr;
use super::super::shape::ShapeTag;
use super::function::JsFunction;
use crate::utility::*;

enum Bits {
  Null = 1,
  Undefined,
  Boolean,
  String,
  Number,
  Object,
  Function,
  RegExp,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct JsReceiver {
  object: Repr,
  bits: Bitset<u8>,
}

impl JsReceiver {
  pub fn new(repr: Repr) -> Option<JsReceiver> {
    if repr.is_invalid() {
      return None;
    }
    if repr.is_heap_object() || !repr.is_boxed() {
      let bitset = Bitset::<u8>::new();
      bitset.set(JsReceiver::get_bit_flag(repr));
      return Some(JsReceiver {
        object: repr,
        bits: bitset,
      });
    }
    return None;
  }

  pub fn is_function(&self) -> bool {
    return self.bits.get(Bits::Function as usize);
  }

  pub fn to_function(&self) -> JsFunction {
    debug_assert!(self.is_function());
    return JsFunction::from(self.object);
  }

  pub fn is_object(&self) -> bool {
    return self.bits.get(Bits::Object as usize);
  }

  pub fn is_number(&self) -> bool {
    return self.bits.get(Bits::Number as usize);
  }

  pub fn is_boolean(&self) -> bool {
    return self.bits.get(Bits::Boolean as usize);
  }

  pub fn is_null(&self) -> bool {
    return self.bits.get(Bits::Boolean as usize);
  }

  pub fn is_undefined(&self) -> bool {
    return self.bits.get(Bits::Undefined as usize);
  }

  fn get_bit_flag(repr: Repr) -> usize {
    if repr.is_js_undefined() {
      return Bits::Undefined as usize;
    }
    if repr.is_js_null() {
      return Bits::Null as usize;
    }
    if repr.is_js_boolean() {
      return Bits::Boolean as usize;
    }
    if !repr.is_heap_object() {
      return Bits::Number as usize;
    }
    match Cell::from(repr).shape().tag() {
      ShapeTag::String => return Bits::String as usize,
      ShapeTag::Number => return Bits::Number as usize,
      ShapeTag::Object => return Bits::Object as usize,
      ShapeTag::Function => return Bits::Function as usize,
      _ => unreachable!(),
    }
    unreachable!();
  }
}
