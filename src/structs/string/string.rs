use super::super::cell::{Cell, HeapLayout, HeapObject};
use super::super::hash_map::PredefinedHash;
use super::super::object::Property;
use super::super::object_record::{FullObjectRecord, ObjectRecord, ObjectSkin};
use super::super::repr::Repr;
use super::super::shape::{Shape, ShapeTag};
use super::backend::StringBackend;
use super::rope::{FlattenString, StringRope};
use super::small_string::{OneByteChar, SmallString};
use super::u16_str::FixedU16CodePointArray;
use crate::context::{AllocationOnlyContext, Context, LuxContext, ObjectRecordsInitializedContext};
use crate::utility::*;
use std::convert::TryFrom;
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
  pub const SIZE: usize = size_of::<JsStringLayout>();

  pub fn new(context: impl ObjectRecordsInitializedContext, str: FixedU16CodePointArray) -> JsString {
    let mut layout = HeapLayout::<JsStringLayout>::new_object(context, context.object_records().string_record());
    layout.backend = JsString::select_backend(context, str);
    let string = JsString(layout);
    JsString::to_object(string.into());
    return string;
  }

  pub fn from_utf8(context: impl ObjectRecordsInitializedContext, str: &str) -> JsString {
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

  pub fn flatten(&mut self, context: impl ObjectRecordsInitializedContext) -> FlatString {
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
    return HeapObject::get_data_field(&m, JsString::IS_OBJECT_BIT);
  }

  pub fn to_object(this: Repr) -> JsString {
    debug_assert!(Cell::from(this).shape().is_string());
    let mut m = JsString::from(this);
    HeapObject::set_data_field(&mut m, JsString::IS_OBJECT_BIT);
    return JsString::from(this);
  }

  pub fn string_piece_record(context: impl AllocationOnlyContext) -> ObjectRecord {
    return ObjectRecord::new(context, StringRope::PIECE_SIZE as u32, Shape::string_piece());
  }

  pub fn string_rope_record(context: impl AllocationOnlyContext) -> ObjectRecord {
    return ObjectRecord::new(context, StringRope::SIZE as u32, Shape::string_rope());
  }

  pub fn flatten_string_record(context: impl AllocationOnlyContext) -> ObjectRecord {
    return ObjectRecord::new(context, FlattenString::SIZE as u32, Shape::flatten_string());
  }

  pub fn small_string_record(context: impl AllocationOnlyContext) -> ObjectRecord {
    return ObjectRecord::new(context, SmallString::SIZE as u32, Shape::small_string());
  }

  pub fn one_byte_char_record(context: impl AllocationOnlyContext) -> ObjectRecord {
    return ObjectRecord::new(context, OneByteChar::SIZE as u32, Shape::one_byte_char());
  }

  fn new_primitive(context: impl ObjectRecordsInitializedContext, str: FixedU16CodePointArray) -> JsString {
    let mut layout = HeapLayout::<JsStringLayout>::new_object(context, context.object_records().string_record());
    layout.backend = JsString::select_backend(context, str).into();
    layout.flat_content = FixedU16CodePointArray::default();
    //    let prop = new_property!(context, str: "length", Repr::from(str.length()));
    //    layout.record().define_own_property(context, prop);
    return JsString(layout);
  }

  fn select_backend(context: impl ObjectRecordsInitializedContext, str: FixedU16CodePointArray) -> Repr {
    if str.length() > 10 {
      return StringRope::new(context, str).into();
    } else if str.length() == 1 {
      return OneByteChar::new(context, str[0]).into();
    }
    return SmallString::new(context, str).into();
  }

  fn from_utf8_to_array(context: impl ObjectRecordsInitializedContext, str: &str) -> FixedU16CodePointArray {
    let u16_vec = str.encode_utf16().collect::<Vec<_>>();
    let mut array = FixedU16CodePointArray::new(context, u16_vec.len());
    for w in &u16_vec {
      array.push(*w);
    }
    return array;
  }
}

impl ObjectSkin for JsString {
  fn set_record(&mut self, r: ObjectRecord) {
    self.0.set_record(r);
  }
  fn record(&self) -> ObjectRecord {
    return self.0.record();
  }
}

impl PredefinedHash for JsString {
  fn prepare_hash(&mut self, context: impl AllocationOnlyContext) {
    let mut r = FullObjectRecord::try_from(self.record()).unwrap();
    if r.hash() > 0 {
      return;
    }
    let flat_content = self.flatten(LuxContext::from_allocation_only_context(context));
    r.set_hash(flat_content.predefined_hash());
  }

  fn predefined_hash(&self) -> u64 {
    return FullObjectRecord::try_from(self.record()).unwrap().hash();
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
  fn prepare_hash(&mut self, _: impl AllocationOnlyContext) {
    let mut r = FullObjectRecord::try_from(self.record()).unwrap();
    if r.hash() > 0 {
      return;
    }
    r.set_hash(self.flat_content.predefined_hash());
  }

  fn predefined_hash(&self) -> u64 {
    return FullObjectRecord::try_from(self.record()).unwrap().hash();
  }
}

impl FlatString {
  pub fn new(context: impl ObjectRecordsInitializedContext, str: FixedU16CodePointArray) -> FlatString {
    return JsString::new_primitive(context, str).flatten(context);
  }

  pub fn from_utf8(context: impl ObjectRecordsInitializedContext, str: &str) -> FlatString {
    let array = JsString::from_utf8_to_array(context, str);
    return FlatString::new(context, array);
  }

  fn flat_content(&self) -> FixedU16CodePointArray {
    return self.flat_content;
  }
}

impl std::fmt::Debug for FlatString {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    use std::char::decode_utf16;
    let flat_content = self.flat_content();
    let chars = decode_utf16(flat_content.into_iter()).map(|r| r.unwrap_or('#')).collect::<String>();
    return write!(f, "{}", chars);
  }
}

impl ObjectSkin for FlatString {
  fn set_record(&mut self, r: ObjectRecord) {
    self.0.set_record(r);
  }
  fn record(&self) -> ObjectRecord {
    return self.0.record();
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
