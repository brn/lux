use super::cell::*;
use super::internal_array::InternalArray;
use super::object::{
  JsObject, OwnProperties, OwnPropertyDescriptorSearchResult, Property, PropertyName, PropertySearchHint,
};
use super::shape::Shape;
use crate::context::{AllocationOnlyContext, Context};
use crate::utility::{BitOperator, Bitset};
use std::mem::size_of;

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct ShadowClassLayout {
  flags: Bitset<u8>,
  child: ShadowClass,
  properties: OwnProperties,
  prototype: JsObject,
  hash: u64,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct ShadowClass(HeapLayout<ShadowClassLayout>);
impl_object!(ShadowClass, HeapLayout<ShadowClassLayout>);

const EXTENSIBLE_INDEX: usize = 1;

pub struct PropertyDescriptorSearchResult {
  own_property_descriptor_search_result: OwnPropertyDescriptorSearchResult,
  shadow_class: ShadowClass,
}

impl PropertyDescriptorSearchResult {
  pub fn new(
    own_property_descriptor_search_result: &OwnPropertyDescriptorSearchResult,
    shadow_class: ShadowClass,
  ) -> PropertyDescriptorSearchResult {
    return PropertyDescriptorSearchResult {
      own_property_descriptor_search_result: *own_property_descriptor_search_result,
      shadow_class,
    };
  }
}

impl ShadowClass {
  const BASE_SIZE: usize = size_of::<ShadowClassLayout>();
  pub fn new(context: impl Context, properties: InternalArray<Property>) -> ShadowClass {
    let mut layout = HeapLayout::<ShadowClassLayout>::new(
      context,
      ShadowClass::BASE_SIZE + properties.length() * size_of::<Property>(),
      Shape::shadow_class(),
    );
    layout.flags.set(EXTENSIBLE_INDEX);
    ShadowClass::init_properties(context, layout, properties);
    return ShadowClass(layout);
  }

  pub fn empty(context: impl AllocationOnlyContext) -> ShadowClass {
    let mut layout = HeapLayout::<ShadowClassLayout>::persist(context, ShadowClass::BASE_SIZE, Shape::shadow_class());
    layout.properties = OwnProperties::new(context);
    return ShadowClass(layout);
  }

  pub fn hash(&self) -> u64 {
    return self.hash;
  }

  pub fn set_hash(mut this: impl ShadowInstance, hash: u64) {
    this.class().hash = hash;
  }

  pub fn is_extended(&self) -> bool {
    return !self.child.is_null();
  }

  pub fn extends(&mut self, shadow_class: ShadowClass) {
    self.child = shadow_class;
  }

  pub fn prototype(&self) -> Option<JsObject> {
    return if !self.prototype.is_null() {
      Some(self.prototype)
    } else {
      None
    };
  }

  pub fn set_prototype(&mut self, proto: JsObject) -> bool {
    self.prototype = proto;
    return true;
  }

  pub fn is_extensible(&self) -> bool {
    return self.flags.get(EXTENSIBLE_INDEX);
  }

  pub fn prevent_extensions(&mut self) -> bool {
    self.flags.unset(EXTENSIBLE_INDEX);
    return true;
  }

  pub fn get_own_property(&self, hint: PropertySearchHint) -> Option<OwnPropertyDescriptorSearchResult> {
    return self.properties.get_property_descriptor(hint);
  }

  pub fn define_own_property(
    &mut self,
    context: impl AllocationOnlyContext,
    property: Property,
  ) -> OwnPropertyDescriptorSearchResult {
    return self.properties.define_property(context, property);
  }

  pub fn define_own_properties(&self, context: impl Context, properties: InternalArray<Property>) {
    ShadowClass::init_properties(context, **self, properties);
  }

  pub fn has_property(&self, hint: PropertySearchHint) -> Option<PropertyDescriptorSearchResult> {
    let mut shadow_class = *self;
    loop {
      match shadow_class.properties.get_property_descriptor(hint) {
        Some(p) => return Some(PropertyDescriptorSearchResult::new(&p, shadow_class)),
        _ => match shadow_class.prototype() {
          Some(p) => shadow_class = p.class(),
          _ => break,
        },
      }
    }
    return None;
  }

  pub fn own_property_keys(&mut self, context: impl Context) -> InternalArray<PropertyName> {
    return self.properties.own_property_keys(context);
  }

  fn init_properties(
    context: impl Context,
    layout: HeapLayout<ShadowClassLayout>,
    properties: InternalArray<Property>,
  ) {
    let mut own_props = OwnProperties::new(context);
    for p in properties {
      own_props.define_property(context, p);
    }
  }
}

pub trait ShadowInstance {
  fn class(&self) -> ShadowClass;
}

macro_rules! impl_shadow_instance {
  ($name:ty) => {
    impl ShadowInstance for $name {
      fn class(&self) -> ShadowClass {
        return self.0.class();
      }
    }
  };
}
