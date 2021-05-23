use super::super::cell::*;
use super::super::hash_map::HashMap;
use super::super::internal_array::InternalArray;
use super::super::js_object::Name;
use super::super::shape::Shape;
use super::property_descriptor::PropertyDescriptor;
use crate::context::{AllocationOnlyContext, Context};
use crate::utility::{BitOperator, Bitset};
use std::mem::size_of;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct PropertyLayout {
  name: BareHeapLayout<Name>,
  desc: BareHeapLayout<PropertyDescriptor>,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct Property(HeapLayout<PropertyLayout>);
impl_object!(Property, HeapLayout<PropertyLayout>);

#[repr(C)]
#[derive(Copy, Clone)]
pub struct FastOwnPropertiesLayout {
  flags: Bitset<u64>,
  own_property_keys: BareHeapLayout<InternalArray<Name>>,
  properties: BareHeapLayout<InternalArray<Property>>,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct SlowOwnPropertiesLayout {
  flags: Bitset<u64>,
  own_property_keys: BareHeapLayout<InternalArray<Name>>,
  properties: BareHeapLayout<HashMap<Name, Property>>,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct OwnProperties(HeapLayout<FastOwnPropertiesLayout>);
impl_object!(OwnProperties, HeapLayout<FastOwnPropertiesLayout>);

#[derive(Copy, Clone)]
pub struct OwnPropertyDescriptorSearchResult {
  descriptor: PropertyDescriptor,
  is_fast: bool,
  index: usize,
}

impl OwnPropertyDescriptorSearchResult {
  pub fn new(descriptor: PropertyDescriptor, is_fast: bool, index: usize) -> OwnPropertyDescriptorSearchResult {
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

  pub fn index(&self) -> usize {
    return self.index;
  }
}

impl OwnProperties {
  const SIZE: usize = Cell::SIZE + size_of::<FastOwnPropertiesLayout>();
  const SLOW_PASS_THRESHOLD: u64 = 5;
  const SLOW_MODE_BIT_INDEX: usize = 1;
  pub fn new(context: &mut impl AllocationOnlyContext) -> OwnProperties {
    let mut layout = HeapLayout::<FastOwnPropertiesLayout>::new(context, OwnProperties::SIZE, Shape::own_properties());
    layout.flags.assign(0);
    layout.own_property_keys.set_null();
    return OwnProperties(layout);
  }

  pub fn define_property(&mut self, context: &mut impl Context, property: Property) {
    self.add_len();
    if !self.flags.get(OwnProperties::SLOW_MODE_BIT_INDEX) {
      self.own_property_keys.set_null();
      self.properties.handle().push(property);
      if self.len() == OwnProperties::SLOW_PASS_THRESHOLD {
        self.flags.set(OwnProperties::SLOW_MODE_BIT_INDEX);
        self.copy_properties(context);
      }
    } else {
      let mut slow = HeapLayout::<SlowOwnPropertiesLayout>::wrap(self.raw_heap());
      slow.own_property_keys.set_null();
      slow
        .properties
        .handle()
        .insert(context, property.name.handle(), property);
    }
  }

  pub fn get_property_descriptor(
    &self,
    context: &mut impl Context,
    name: Name,
  ) -> Option<OwnPropertyDescriptorSearchResult> {
    if !self.flags.get(OwnProperties::SLOW_MODE_BIT_INDEX) {
      for (index, p) in self.properties.handle().into_iter().enumerate() {
        if p.name.handle() == name {
          return Some(OwnPropertyDescriptorSearchResult::new(p.desc.handle(), true, index));
        }
      }
      return None;
    } else {
      let slow = HeapLayout::<SlowOwnPropertiesLayout>::wrap(self.raw_heap());
      match slow.properties.handle().find(context, name) {
        Some(p) => return Some(OwnPropertyDescriptorSearchResult::new(p.desc.handle(), false, 0)),
        _ => return None,
      };
    }
  }

  pub fn own_property_keys(&mut self, context: &mut impl Context) -> InternalArray<Name> {
    if !self.flags.get(OwnProperties::SLOW_MODE_BIT_INDEX) {
      if !self.own_property_keys.is_null() {
        return self.own_property_keys.handle();
      }
      let properties = self.properties.handle();
      let mut keys = InternalArray::<Name>::new(context, properties.length());
      for p in properties {
        keys.push(p.name.handle());
      }
      self.own_property_keys.set(keys);
      return keys;
    } else {
      let slow = HeapLayout::<SlowOwnPropertiesLayout>::wrap(self.raw_heap());
      if !slow.own_property_keys.is_null() {
        return slow.own_property_keys.handle();
      }
      let properties = slow.properties.handle();
      let mut keys = InternalArray::<Name>::new(context, properties.len());
      for (_, p) in properties {
        keys.push(p.name.handle());
      }
      self.own_property_keys.set(keys);
      return keys;
    }
  }

  fn copy_properties(&self, context: &mut impl Context) {
    let mut slow = HeapLayout::<SlowOwnPropertiesLayout>::wrap(self.raw_heap());
    let mut hm = HashMap::<Name, Property>::new(context);
    slow.properties.set(hm);
    for prop in self.properties.handle() {
      hm.insert(context, prop.name.handle(), prop);
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
