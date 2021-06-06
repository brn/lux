use super::super::cell::*;
use super::super::hash_map::PredefinedHash;
use super::super::internal_array::InternalArray;
use super::super::object_record::ObjectSkin;
use super::super::repr::Repr;
use super::super::string::{FlatString, JsString};
use super::property_descriptor::PropertyDescriptor;
use super::symbol::JsSymbol;
use crate::context::{AllocationOnlyContext, Context, ObjectRecordsInitializedContext};
use crate::def::*;
use property::Property as Prop;
use std::cmp::PartialEq;
use std::mem::size_of;

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct PropertyName(Repr);

impl PropertyName {
  pub fn new(a: Repr) -> PropertyName {
    assert!(a.is_number() || (a.is_string() && JsString::from(a).has_flat_content()) || a.is_symbol());
    return PropertyName(a);
  }

  pub fn to_number(&self) -> Option<f64> {
    return self.0.to_number();
  }

  pub fn to_number_unchecked(&self) -> f64 {
    return self.0.to_number_unchecked();
  }

  pub fn is_number(&self) -> bool {
    return self.0.is_number();
  }

  pub fn is_symbol(&self) -> bool {
    return self.0.is_symbol();
  }

  pub fn is_string(&self) -> bool {
    return self.0.is_string();
  }

  pub fn from_utf8_string(context: impl ObjectRecordsInitializedContext, str: &str) -> PropertyName {
    let str = FlatString::from_utf8(context, str);
    return PropertyName::new(str.into());
  }

  pub fn from_utf8_symbol(context: impl Context, str: &str) -> PropertyName {
    let desc = FlatString::from_utf8(context, str);
    let symbol = JsSymbol::new(context, desc);
    return PropertyName::new(symbol.into());
  }
}

impl PartialEq for PropertyName {
  fn eq(&self, other: &Self) -> bool {
    let is_boxed = self.0.is_boxed();
    if is_boxed != other.0.is_boxed() {
      return false;
    }
    if !is_boxed {
      return self.0.to_number_unchecked() == other.0.to_number_unchecked();
    }
    if Cell::from(self.0).shape().tag() != Cell::from(other.0).shape().tag() {
      return false;
    }
    if self.is_string() {
      return FlatString::from(self.0) == FlatString::from(other.0);
    }
    return false;
  }
}

impl PredefinedHash for PropertyName {
  fn prepare_hash(&mut self, context: impl AllocationOnlyContext) {
    if self.predefined_hash() != 0 {
      return;
    }
    if self.is_symbol() {
      JsSymbol::from(self.0).prepare_hash(context);
    } else {
      FlatString::from(self.0).prepare_hash(context);
    }
  }
  fn predefined_hash(&self) -> u64 {
    return Cell::from(self.0).full_record_unchecked().hash();
  }
}

impl From<JsSymbol> for PropertyName {
  fn from(a: JsSymbol) -> PropertyName {
    return PropertyName(a.into());
  }
}

impl From<JsString> for PropertyName {
  fn from(a: JsString) -> PropertyName {
    assert!(a.has_flat_content());
    return PropertyName(a.into());
  }
}

impl From<FlatString> for PropertyName {
  fn from(a: FlatString) -> PropertyName {
    return PropertyName(a.into());
  }
}

impl From<PropertyName> for JsSymbol {
  fn from(a: PropertyName) -> JsSymbol {
    if a.is_symbol() {
      return JsSymbol::from(a.0);
    }
    return JsSymbol::from(Repr::invalid());
  }
}

impl From<PropertyName> for JsString {
  fn from(a: PropertyName) -> JsString {
    if !a.is_string() {
      return JsString::from(a.0);
    }
    return JsString::from(Repr::invalid());
  }
}

impl From<PropertyName> for FlatString {
  fn from(a: PropertyName) -> FlatString {
    if !a.is_string() {
      return FlatString::from(a.0);
    }
    return FlatString::from(Repr::invalid());
  }
}

impl From<Addr> for PropertyName {
  fn from(a: Addr) -> PropertyName {
    return PropertyName(Repr::new(a));
  }
}

impl From<PropertyName> for Addr {
  fn from(a: PropertyName) -> Addr {
    return a.0.into();
  }
}

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct PropertyLayout {
  name: PropertyName,
  desc: PropertyDescriptor,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct Property(HeapLayout<PropertyLayout>);
impl_object!(Property, HeapLayout<PropertyLayout>);

impl Property {
  pub const SIZE: usize = size_of::<PropertyLayout>();
  pub fn new(context: impl ObjectRecordsInitializedContext, name: PropertyName, desc: PropertyDescriptor) -> Property {
    let mut layout = HeapLayout::<PropertyLayout>::new(context, context.object_records().property_record());
    layout.name = name;
    layout.desc = desc;
    return Property(layout);
  }

  pub fn name(&self) -> PropertyName {
    return self.name;
  }

  pub fn desc(&self) -> PropertyDescriptor {
    return self.desc;
  }
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct FastOwnProperties(*mut Property);

impl FastOwnProperties {
  pub fn add_property(&mut self, cap: u32, len: u32, property: Property) {
    debug_assert!(len < cap);
    unsafe {
      *(self.properties().offset(len as isize)) = property;
    }
  }

  pub fn get_property_descriptor(&self, len: u32, property: Property) -> Option<(isize, PropertyDescriptor)> {
    for index in 0..len {
      let p = self.offset(index as isize);
      if !p.is_null() && property.name() == self.offset_ref(index as isize).name() {
        return Some((index as isize, self.offset_ref(index as isize).desc()));
      }
    }
    return None;
  }

  pub fn collect_own_property_keys(&mut self, len: u32, mut keys: InternalArray<PropertyName>) {
    for index in 0..len {
      keys.push(self.offset_ref(index as isize).name());
    }
  }

  pub fn wrap(ptr: Addr) -> FastOwnProperties {
    return FastOwnProperties(ptr as *mut Property);
  }

  pub fn properties(&self) -> *mut Property {
    return self.0 as *mut Property;
  }

  pub fn offset(&self, offset: isize) -> *mut Property {
    return unsafe { self.properties().offset(offset) };
  }

  pub fn offset_ref(&self, offset: isize) -> &Property {
    return unsafe { &*self.offset(offset) };
  }
}

impl From<Addr> for FastOwnProperties {
  fn from(a: Addr) -> FastOwnProperties {
    return FastOwnProperties::wrap(a);
  }
}
