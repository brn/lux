use super::super::cell::*;
use super::super::hash_map::{HashMap, PredefinedHash};
use super::super::internal_array::InternalArray;
use super::super::repr::Repr;
use super::super::shadow_class::ShadowInstance;
use super::super::shape::Shape;
use super::super::string::{FlatString, JsString};
use super::property_descriptor::PropertyDescriptor;
use super::symbol::JsSymbol;
use crate::context::{AllocationOnlyContext, Context};
use crate::def::*;
use crate::utility::{BitOperator, Bitset};
use std::cmp::PartialEq;
use std::mem::size_of;

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct PropertyName(Repr);

impl PropertyName {
  pub fn new(a: Repr) -> PropertyName {
    assert!((a.is_string() && JsString::from(a).has_flat_content()) || a.is_symbol());
    return PropertyName(a);
  }

  pub fn is_symbol(&self) -> bool {
    return self.is_symbol();
  }

  pub fn is_string(&self) -> bool {
    return self.is_string();
  }

  pub fn from_utf8_string(context: impl AllocationOnlyContext, str: &str) -> PropertyName {
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
    debug_assert!(self.0.is_boxed() && other.0.is_boxed());
    if Cell::from(self.0).shape().tag() != Cell::from(other.0).shape().tag() {
      return false;
    }
    if self.is_string() {
      return FlatString::from(*self) == FlatString::from(*other);
    }
    return false;
  }
}

impl PredefinedHash for PropertyName {
  fn prepare_hash(&mut self, context: impl Context) {
    if self.is_symbol() {
      JsSymbol::from(self.0).prepare_hash(context);
    } else {
      FlatString::from(self.0).prepare_hash(context);
    }
  }
  fn predefined_hash(&self) -> u64 {
    return Cell::from(self.0).class().hash();
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
  const SIZE: usize = size_of::<PropertyLayout>();
  pub fn new(context: impl AllocationOnlyContext, name: PropertyName, desc: PropertyDescriptor) -> Property {
    let mut layout = HeapLayout::<PropertyLayout>::new(context, Property::SIZE, Shape::property());
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

macro_rules! new_property {
  ($context:expr, value: $name:expr, $value:expr) => {{
    let a = crate::structs::object::PropertyDescriptor::new_data_descriptor(
      $context,
      crate::structs::object::PropertyDescriptor::DEFAULT,
      $value,
    );
    crate::structs::object::Property::new($context, $name, a)
  }};
  ($context:expr, str: $name:expr, $value:expr) => {{
    let a = crate::structs::object::PropertyDescriptor::new_data_descriptor(
      $context,
      crate::structs::object::PropertyDescriptor::DEFAULT,
      $value,
    );
    crate::structs::object::Property::new(
      $context,
      crate::structs::object::PropertyName::from_utf8_string($context, $name),
      a,
    )
  }};
}

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct FastOwnPropertiesLayout {
  flags: Bitset<u64>,
  own_property_keys: InternalArray<PropertyName>,
  properties: InternalArray<Property>,
}

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct SlowOwnPropertiesLayout {
  flags: Bitset<u64>,
  own_property_keys: InternalArray<PropertyName>,
  properties: HashMap<PropertyName, Property>,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct OwnProperties(HeapLayout<FastOwnPropertiesLayout>);
impl_object!(OwnProperties, HeapLayout<FastOwnPropertiesLayout>);

#[derive(Copy, Clone)]
pub struct OwnPropertyDescriptorSearchResult {
  descriptor: PropertyDescriptor,
  is_fast: bool,
  index: isize,
}

impl OwnPropertyDescriptorSearchResult {
  pub fn new(descriptor: PropertyDescriptor, is_fast: bool, index: isize) -> OwnPropertyDescriptorSearchResult {
    return OwnPropertyDescriptorSearchResult {
      descriptor,
      is_fast,
      index,
    };
  }

  pub fn descriptor(&self) -> PropertyDescriptor {
    return self.descriptor;
  }

  pub fn is_fast(&self) -> bool {
    return self.is_fast;
  }

  pub fn index(&self) -> isize {
    return self.index;
  }

  pub fn usable_as_cache(&self) -> bool {
    return self.is_fast() && self.index >= 0;
  }
}

#[derive(Copy, Clone)]
pub struct PropertySearchHint {
  key: PropertyName,
  index: isize,
}

impl PropertySearchHint {
  pub fn new(key: PropertyName) -> PropertySearchHint {
    return PropertySearchHint { key, index: -1 };
  }
  pub fn new_with_index(key: PropertyName, index: isize) -> PropertySearchHint {
    return PropertySearchHint { key, index };
  }
}

impl OwnProperties {
  const SIZE: usize = size_of::<FastOwnPropertiesLayout>();
  const SLOW_PASS_THRESHOLD: u64 = 5;
  const SLOW_MODE_BIT_INDEX: usize = 1;
  pub fn new(context: impl AllocationOnlyContext) -> OwnProperties {
    let mut layout = HeapLayout::<FastOwnPropertiesLayout>::new(context, OwnProperties::SIZE, Shape::own_properties());
    layout.flags.assign(0);
    return OwnProperties(layout);
  }

  pub fn define_property(
    &mut self,
    context: impl AllocationOnlyContext,
    property: Property,
  ) -> OwnPropertyDescriptorSearchResult {
    self.add_len();
    if !self.flags.get(OwnProperties::SLOW_MODE_BIT_INDEX) {
      self.own_property_keys = InternalArray::<PropertyName>::default();
      self.properties.push(property);
      if self.len() == OwnProperties::SLOW_PASS_THRESHOLD {
        self.flags.set(OwnProperties::SLOW_MODE_BIT_INDEX);
        self.copy_properties(context);
        return OwnPropertyDescriptorSearchResult::new(property.desc(), false, 0);
      }
      return OwnPropertyDescriptorSearchResult::new(property.desc(), true, (self.properties.length() - 1) as isize);
    } else {
      let mut slow = HeapLayout::<SlowOwnPropertiesLayout>::wrap(self.raw_heap());
      slow.own_property_keys = InternalArray::<PropertyName>::default();
      slow.properties.insert(context, property.name, property);
      return OwnPropertyDescriptorSearchResult::new(property.desc(), false, 0);
    }
  }

  pub fn get_property_descriptor(&self, hint: PropertySearchHint) -> Option<OwnPropertyDescriptorSearchResult> {
    if !self.flags.get(OwnProperties::SLOW_MODE_BIT_INDEX) {
      if hint.index > 0 && !self.properties[hint.index as usize].is_null() {
        return Some(OwnPropertyDescriptorSearchResult::new(
          self.properties[hint.index as usize].desc(),
          true,
          hint.index,
        ));
      }
      for (index, p) in self.properties.into_iter().enumerate() {
        if hint.key == p.name() {
          return Some(OwnPropertyDescriptorSearchResult::new(p.desc, true, index as isize));
        }
      }
      return None;
    } else {
      let slow = HeapLayout::<SlowOwnPropertiesLayout>::wrap(self.raw_heap());
      match slow.properties.find(hint.key) {
        Some(p) => return Some(OwnPropertyDescriptorSearchResult::new(p.desc, false, 0)),
        _ => return None,
      };
    }
  }

  pub fn own_property_keys(&mut self, context: impl Context) -> InternalArray<PropertyName> {
    if !self.flags.get(OwnProperties::SLOW_MODE_BIT_INDEX) {
      if !self.own_property_keys.is_null() {
        return self.own_property_keys;
      }
      let properties = self.properties;
      let mut keys = InternalArray::<PropertyName>::new(context, properties.length());
      for p in properties {
        keys.push(p.name);
      }
      self.own_property_keys = keys;
      return keys;
    } else {
      let slow = HeapLayout::<SlowOwnPropertiesLayout>::wrap(self.raw_heap());
      if !slow.own_property_keys.is_null() {
        return slow.own_property_keys;
      }
      let properties = slow.properties;
      let mut keys = InternalArray::<PropertyName>::new(context, properties.len());
      for (_, p) in properties {
        keys.push(p.name);
      }
      self.own_property_keys = keys;
      return keys;
    }
  }

  fn copy_properties(&self, context: impl AllocationOnlyContext) {
    let mut slow = HeapLayout::<SlowOwnPropertiesLayout>::wrap(self.raw_heap());
    let mut hm = HashMap::<PropertyName, Property>::new(context);
    slow.properties = hm;
    for prop in self.properties {
      hm.insert(context, prop.name, prop);
    }
  }

  pub fn len(&self) -> u64 {
    return self.flags.mask_lower(OwnProperties::SLOW_MODE_BIT_INDEX).bits();
  }

  fn add_len(&mut self) {
    let mut range = self.flags.mask_lower(OwnProperties::SLOW_MODE_BIT_INDEX);
    range.assign(self.len() + 1);
    self.flags = range.into();
  }
}
