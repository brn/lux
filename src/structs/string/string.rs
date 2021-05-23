use super::super::cell::{BareHeapLayout, Cell, HeapLayout, HeapObject};
use super::super::cmp::*;
use super::super::convs::AbstractConvs;
use super::super::repr::Repr;
use super::super::shape::{Shape, ShapeTag};
use super::backend::StringBackend;
use super::rope::{FlattenString, StringRope};
use super::small_string::{OneByteChar, SmallString};
use super::to_number::to_number;
use super::u16_str::FixedU16CodePointArray;
use crate::context::{AllocationOnlyContext, Context};
use crate::unicode::{chars, is_white_space};
use crate::utility::{BitOperator, Bitset};
use std::mem::size_of;

macro_rules! dispatch {
  ($name:ident, $string:expr, $($args:expr),*) => {
    match Cell::from($string.backend.handle()).shape().tag() {
      ShapeTag::StringRope => StringRope::from($string.backend.handle()).$name($($args,)*),
      ShapeTag::SmallString => SmallString::from($string.backend.handle()).$name($($args,)*),
      ShapeTag::FlattenString => FlattenString::from($string.backend.handle()).$name($($args,)*),
      ShapeTag::OneByteChar => OneByteChar::from($string.backend.handle()).$name($($args,)*),
      _ => unreachable!()
    }
  };
  ($name:ident, $string:expr) => {
    match Cell::from($string.backend.handle()).shape().tag() {
      ShapeTag::StringRope => StringRope::from($string.backend.handle()).$name(),
      ShapeTag::SmallString => SmallString::from($string.backend.handle()).$name(),
      ShapeTag::FlattenString => FlattenString::from($string.backend.handle()).$name(),
      ShapeTag::OneByteChar => OneByteChar::from($string.backend.handle()).$name(),
      _ => unreachable!()
    }
  }
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct JsStringLayout {
  backend: BareHeapLayout<Repr>,
}

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct JsString(HeapLayout<JsStringLayout>);
impl_object!(JsString, HeapLayout<JsStringLayout>);

impl JsString {
  const SIZE: usize = Cell::SIZE + size_of::<JsStringLayout>();
  pub fn new(context: &mut impl AllocationOnlyContext, str: FixedU16CodePointArray) -> JsString {
    let mut layout = HeapLayout::<JsStringLayout>::new(context, JsString::SIZE, Shape::string());
    layout.backend.set(if str.length() > 10 {
      StringRope::new(context, str).into()
    } else if str.length() == 1 {
      OneByteChar::new(context, str[0]).into()
    } else {
      SmallString::new(context, str).into()
    });
    return JsString(layout);
  }

  pub fn from_utf8(context: &mut impl AllocationOnlyContext, str: &str) -> JsString {
    let u16_vec = str.encode_utf16().collect::<Vec<_>>();
    let mut array = FixedU16CodePointArray::new(context, u16_vec.len());
    for w in &u16_vec {
      array.push(*w);
    }
    return JsString::new(context, array);
  }

  pub fn len(&self) -> usize {
    return dispatch!(len, *self);
  }

  pub fn concat(&self, context: &mut impl Context, str: JsString) -> JsString {
    return dispatch!(concat, *self, context, str.backend.handle());
  }

  pub fn flatten(&self, context: &mut impl Context) -> FixedU16CodePointArray {
    return dispatch!(flatten, *self, context);
  }

  fn to_string_internal(&self) -> JsString {
    return *self;
  }

  fn backend(&self) -> Repr {
    return self.backend.handle();
  }

  fn char_at(context: &mut impl Context, this: Repr, index: Repr) -> Result<u16, Repr> {
    require_object_coercible(context, this, "String.prototype.charAt");
    return Ok(0);
  }
}

impl<T: StringBackend> From<T> for JsString {
  fn from(o: T) -> JsString {
    return JsString(HeapLayout::wrap(o.raw_heap()));
  }
}

impl AbstractConvs for JsString {
  fn to_string(&self, _: &mut impl AllocationOnlyContext) -> JsString {
    return *self;
  }

  fn to_number(&self, context: &mut impl Context) -> Result<Repr, Repr> {
    return to_number(self.flatten(context));
  }
}
