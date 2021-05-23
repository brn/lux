use super::cell::*;
use super::internal_array::InternalArray;
use super::js_object::Name;
use super::object::{JsObject, OwnProperties, OwnPropertyDescriptorSearchResult, Property, PropertyDescriptor};
use super::shape::Shape;
use crate::context::{AllocationOnlyContext, Context};
use crate::utility::{BitOperator, Bitset};
use std::mem::size_of;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct ShadowClassLayout {
  flags: Bitset<u8>,
  child: BareHeapLayout<ShadowClass>,
  properties: BareHeapLayout<OwnProperties>,
  prototype: BareHeapLayout<JsObject>,
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
  const BASE_SIZE: usize = Cell::SIZE + size_of::<ShadowClassLayout>();
  pub fn new(context: &mut impl Context, properties: InternalArray<Property>) -> ShadowClass {
    let mut layout = HeapLayout::<ShadowClassLayout>::new(
      context,
      ShadowClass::BASE_SIZE + properties.length() * size_of::<Property>(),
      Shape::shadow_class(),
    );
    layout.flags.set(EXTENSIBLE_INDEX);
    layout.child.set_null();
    layout.prototype.set_null();
    ShadowClass::init_properties(context, layout, properties);
    return ShadowClass(layout);
  }

  pub fn empty(context: &mut impl AllocationOnlyContext) -> ShadowClass {
    let mut layout = HeapLayout::<ShadowClassLayout>::persist(context, ShadowClass::BASE_SIZE, Shape::shadow_class());
    layout.child.set_null();
    layout.properties.set(OwnProperties::new(context));
    return ShadowClass(layout);
  }

  pub fn is_extended(&self) -> bool {
    return !self.child.is_null();
  }

  pub fn extends(&mut self, shadow_class: ShadowClass) {
    self.child.set(shadow_class);
  }

  pub fn prototype(&self) -> Option<JsObject> {
    return if !self.prototype.is_null() {
      Some(self.prototype.handle())
    } else {
      None
    };
  }

  pub fn set_prototype(&mut self, proto: JsObject) -> bool {
    self.prototype.set(proto);
    return true;
  }

  pub fn is_extensible(&self) -> bool {
    return self.flags.get(EXTENSIBLE_INDEX);
  }

  pub fn prevent_extensions(&mut self) -> bool {
    self.flags.unset(EXTENSIBLE_INDEX);
    return true;
  }

  pub fn get_own_property(&self, context: &mut impl Context, name: Name) -> Option<OwnPropertyDescriptorSearchResult> {
    return self.properties.handle().get_property_descriptor(context, name);
  }

  pub fn define_own_property(&self, context: &mut impl Context, property: Property) -> bool {
    self.properties.handle().define_property(context, property);
    return true;
  }

  pub fn has_property(&self, context: &mut impl Context, name: Name) -> Option<PropertyDescriptorSearchResult> {
    let mut shadow_class = *self;
    loop {
      match shadow_class.properties.handle().get_property_descriptor(context, name) {
        Some(p) => return Some(PropertyDescriptorSearchResult::new(&p, shadow_class)),
        _ => match shadow_class.prototype() {
          Some(p) => shadow_class = p.class(),
          _ => break,
        },
      }
    }
    return None;
  }

  pub fn own_property_keys(&self, context: &mut impl Context) -> InternalArray<Name> {
    return self.properties.handle().own_property_keys(context);
  }

  fn init_properties(
    context: &mut impl Context,
    layout: HeapLayout<ShadowClassLayout>,
    properties: InternalArray<Property>,
  ) {
    let mut own_props = OwnProperties::new(context);
    for p in properties {
      own_props.define_property(context, p);
    }
  }
}
