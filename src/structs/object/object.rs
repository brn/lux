use super::super::cell::*;
use super::super::internal_array::InternalArray;
use super::super::js_object::Name;
use super::super::repr::Repr;
use super::super::shadow_class::ShadowClass;
use super::super::shape::Shape;
use super::property::{OwnPropertyDescriptorSearchResult, Property};
use crate::context::Context;
use crate::utility::{BitOperator, Bitset};
use std::mem::size_of;

#[derive(Copy, Clone)]
pub struct JsObjectLayout {
  shadow_class: BareHeapLayout<ShadowClass>,
}

#[derive(Copy, Clone)]
pub struct JsObject(HeapLayout<JsObjectLayout>);
impl_object!(JsObject, HeapLayout<JsObjectLayout>);

impl JsObject {
  const SIZE: usize = Cell::SIZE + size_of::<JsObjectLayout>();
  pub fn new(context: &mut impl Context) -> JsObject {
    let mut layout = HeapLayout::<JsObjectLayout>::new(context, JsObject::SIZE, Shape::object());
    layout.shadow_class.set(context.empty_shadow_class());
    return JsObject(layout);
  }

  pub fn class(&self) -> ShadowClass {
    debug_assert!(!self.shadow_class.is_null());
    return self.shadow_class.handle();
  }

  pub fn get_prototype_of(&self) -> Option<JsObject> {
    debug_assert!(!self.shadow_class.is_null());
    return self.shadow_class.handle().prototype();
  }

  pub fn set_prototype_of(&mut self, prototype: JsObject) -> bool {
    debug_assert!(!self.shadow_class.is_null());
    return self.shadow_class.handle().set_prototype(prototype);
  }

  pub fn is_extensible(&self) -> bool {
    debug_assert!(!self.shadow_class.is_null());
    return self.shadow_class.handle().is_extensible();
  }

  pub fn prevent_extensions(&self) -> bool {
    debug_assert!(!self.shadow_class.is_null());
    return self.shadow_class.handle().prevent_extensions();
  }

  pub fn get_own_property(&self, context: &mut impl Context, name: Name) -> Option<OwnPropertyDescriptorSearchResult> {
    return self.shadow_class.handle().get_own_property(context, name);
  }

  pub fn define_own_property(&self, context: &mut impl Context, property: Property) -> bool {
    return self.shadow_class.handle().define_own_property(context, property);
  }

  pub fn has_property(&self, context: &mut impl Context, name: Name) -> bool {
    match self.shadow_class.handle().has_property(context, name) {
      Some(_) => return true,
      Noen => return false,
    }
  }

  pub fn own_property_keys(&self, context: &mut impl Context) -> InternalArray<Name> {
    return self.shadow_class.handle().own_property_keys(context);
  }

  pub fn get(context: &mut impl Context, name: Name, receiver: Repr) -> Option<Repr> {
    return None;
  }

  pub fn set(context: &mut impl Context, name: Name, value: Repr, receiver: Repr) -> bool {
    return false;
  }

  pub fn delete(context: &mut impl Context, name: Name, receiver: Repr) -> bool {
    return false;
  }

  pub fn call(context: &mut impl Context, receiver: Repr, args: InternalArray<Repr>) -> Repr {
    return Repr::invalid();
  }

  pub fn construct(context: &mut impl Context, args: InternalArray<Repr>, target: JsObject) -> JsObject {
    return JsObject::from(Repr::invalid());
  }
}
