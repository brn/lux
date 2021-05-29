use super::super::cell::{Cell, HeapLayout, HeapObject};
use super::super::cmp::*;
use super::super::hash_map::{DefaultHasher, Hasher, PredefinedHash};
use super::super::object::Property;
use super::super::repr::Repr;
use super::super::shadow_class::{ShadowClass, ShadowInstance};
use super::super::shape::{Shape, ShapeTag};
use super::backend::StringBackend;
use super::rope::{FlattenString, StringRope};
use super::small_string::{OneByteChar, SmallString};
use super::u16_str::FixedU16CodePointArray;
use crate::context::{AllocationOnlyContext, Context};
use crate::utility::*;
use std::mem::size_of;

macro_rules! dispatch {
  ($name:ident, $string:expr, $($args:expr),*) => {
    match Cell::from($string.backend).shape().tag() {
      ShapeTag::StringRope => StringRope::from($string.backend).$name($($args,)*),
      ShapeTag::SmallString => SmallString::from($string.backend).$name($($args,)*),
      ShapeTag::FlattenString => FlattenString::from($string.backend).$name($($args,)*),
      ShapeTag::OneByteChar => OneByteChar::from($string.backend).$name($($args,)*),
      _ => unreachable!()
    }
  };
  ($name:ident, $string:expr) => {
    match Cell::from($string.backend).shape().tag() {
      ShapeTag::StringRope => StringRope::from($string.backend).$name(),
      ShapeTag::SmallString => SmallString::from($string.backend).$name(),
      ShapeTag::FlattenString => FlattenString::from($string.backend).$name(),
      ShapeTag::OneByteChar => OneByteChar::from($string.backend).$name(),
      _ => unreachable!()
    }
  }
}

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct JsStringLayout {
  backend: Repr,
  flat_content: FixedU16CodePointArray,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct JsString(HeapLayout<JsStringLayout>);
impl_object!(JsString, HeapLayout<JsStringLayout>);

impl JsString {
  const IS_OBJECT_BIT: usize = 1;
  pub fn new(context: impl AllocationOnlyContext, str: FixedU16CodePointArray) -> JsString {
    let mut layout = HeapLayout::<JsStringLayout>::new_object(context, size_of::<JsStringLayout>(), Shape::string());
    layout.backend = JsString::select_backend(context, str);
    layout
      .class()
      .define_own_property(context, new_property!(context, str: "length", Repr::from(str.length())));
    let string = JsString(layout);
    JsString::to_object(string.into());
    return string;
  }

  pub fn from_utf8(context: impl AllocationOnlyContext, str: &str) -> JsString {
    let array = JsString::from_utf8_to_array(context, str);
    return JsString::new(context, array);
  }

  pub fn len(&self) -> usize {
    return dispatch!(len, *self);
  }

  pub fn concat(&self, context: impl Context, str: JsString) -> JsString {
    return dispatch!(concat, *self, context, str.backend);
  }

  pub fn has_flat_content(&self) -> bool {
    return !self.flat_content.is_null();
  }

  pub fn flat_content(&self) -> FixedU16CodePointArray {
    debug_assert!(self.has_flat_content());
    return self.flat_content;
  }

  pub fn flatten(&mut self, context: impl AllocationOnlyContext) -> FlatString {
    if !self.flat_content.is_null() {
      return FlatString::from(*self);
    }
    let a = *self;
    self.flat_content = dispatch!(flatten, a, context);
    return FlatString::from(*self);
  }

  pub fn is_object(this: Repr) -> bool {
    debug_assert!(Cell::from(this).shape().is_string());
    let m = JsString::from(this);
    let data_field = HeapObject::get_data_field(&m);
    return data_field.get(JsString::IS_OBJECT_BIT);
  }

  pub fn to_object(this: Repr) -> JsString {
    debug_assert!(Cell::from(this).shape().is_string());
    let m = JsString::from(this);
    let mut data_field = HeapObject::get_data_field(&m);
    data_field.set(JsString::IS_OBJECT_BIT);
    return JsString::from(this);
  }

  fn new_primitive(context: impl AllocationOnlyContext, str: FixedU16CodePointArray) -> JsString {
    let mut layout = HeapLayout::<JsStringLayout>::new_object(context, size_of::<JsStringLayout>(), Shape::string());
    layout.backend = JsString::select_backend(context, str).into();
    layout.flat_content = FixedU16CodePointArray::default();
    layout
      .class()
      .define_own_property(context, new_property!(context, str: "length", Repr::from(str.length())));
    return JsString(layout);
  }

  fn to_string_internal(&self) -> JsString {
    return *self;
  }

  fn backend(&self) -> Repr {
    return self.backend;
  }

  fn char_at(context: impl Context, this: Repr, index: Repr) -> Result<u16, Repr> {
    require_object_coercible(context, this, "String.prototype.charAt");
    return Ok(0);
  }

  fn select_backend(context: impl AllocationOnlyContext, str: FixedU16CodePointArray) -> Repr {
    if str.length() > 10 {
      return StringRope::new(context, str).into();
    } else if str.length() == 1 {
      return OneByteChar::new(context, str[0]).into();
    }
    return SmallString::new(context, str).into();
  }

  fn from_utf8_to_array(context: impl AllocationOnlyContext, str: &str) -> FixedU16CodePointArray {
    let u16_vec = str.encode_utf16().collect::<Vec<_>>();
    let mut array = FixedU16CodePointArray::new(context, u16_vec.len());
    for w in &u16_vec {
      array.push(*w);
    }
    return array;
  }
}

impl ShadowInstance for JsString {
  fn class(&self) -> ShadowClass {
    return self.0.class();
  }
}

impl PredefinedHash for JsString {
  fn prepare_hash(&mut self, context: impl Context) {
    if self.class().hash() > 0 {
      return;
    }
    let mut flat_content = self.flatten(context);
    ShadowClass::set_hash(*self, flat_content.predefined_hash());
  }

  fn predefined_hash(&self) -> u64 {
    return self.class().hash();
  }
}

impl<T: StringBackend> From<T> for JsString {
  fn from(o: T) -> JsString {
    return JsString(HeapLayout::wrap(o.raw_heap()));
  }
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct FlatString(HeapLayout<JsStringLayout>);
impl_object!(FlatString, HeapLayout<JsStringLayout>);

impl PredefinedHash for FlatString {
  fn prepare_hash(&mut self, context: impl Context) {
    if self.class().hash() > 0 {
      return;
    }
    ShadowClass::set_hash(*self, self.flat_content.predefined_hash());
  }

  fn predefined_hash(&self) -> u64 {
    return self.class().hash();
  }
}

impl FlatString {
  pub fn new(context: impl AllocationOnlyContext, str: FixedU16CodePointArray) -> FlatString {
    let mut js_str = JsString::new_primitive(context, str);
    js_str.flatten(context);
    return FlatString(js_str.0);
  }

  pub fn from_utf8(context: impl AllocationOnlyContext, str: &str) -> FlatString {
    let array = JsString::from_utf8_to_array(context, str);
    return FlatString::new(context, array);
  }

  fn flat_content(&self) -> FixedU16CodePointArray {
    return self.flat_content;
  }
}

impl ShadowInstance for FlatString {
  fn class(&self) -> ShadowClass {
    return self.0.class();
  }
}

impl std::cmp::PartialEq for FlatString {
  fn eq(&self, other: &FlatString) -> bool {
    return self.flat_content() == other.flat_content();
  }
}

impl std::cmp::Eq for FlatString {}

impl From<FlatString> for JsString {
  fn from(p: FlatString) -> JsString {
    return JsString(p.0);
  }
}

impl From<JsString> for FlatString {
  fn from(p: JsString) -> FlatString {
    assert!(p.has_flat_content());
    return FlatString(p.0);
  }
}
