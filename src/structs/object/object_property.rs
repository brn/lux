use super::super::internal_array::InternalArray;
use super::super::object_record::{
  FullObjectRecord, ObjectSkin, OwnPropertyDescriptorSearchResult, OwnPropertySearchHint, PropertyDescriptorSearchResult, PropertyMode,
  PropertySearchHint,
};
use super::super::repr::Repr;
use super::property::{Property, PropertyName};
use super::receiver::JsReceiver;
use crate::context::ObjectRecordsInitializedContext;

pub use super::super::object_record::{PropertyDescriptorArray, PropertyDescriptorTable};
pub type OwnKeys = InternalArray<PropertyName>;

#[inline(always)]
pub fn define_own_property(
  object: Repr,
  context: impl ObjectRecordsInitializedContext,
  property: Property,
) -> OwnPropertyDescriptorSearchResult {
  let full_object_record = object.full_record().unwrap();
  return FullObjectRecord::define_own_property(full_object_record, context, JsReceiver::new(object), property, true);
}

#[inline(always)]
pub fn define_own_property_kv(
  object: Repr,
  context: impl ObjectRecordsInitializedContext,
  property_name: PropertyName,
  value: Repr,
) -> OwnPropertyDescriptorSearchResult {
  return define_own_property(object, context, new_property!(context, value: property_name, value));
}

#[inline(always)]
pub fn define_own_property_from_utf8(
  object: Repr,
  context: impl ObjectRecordsInitializedContext,
  property_name: &str,
  value: Repr,
) -> OwnPropertyDescriptorSearchResult {
  return define_own_property(object, context, new_property!(context, str: property_name, value));
}

#[inline(always)]
pub fn define_own_properties(object: Repr, context: impl ObjectRecordsInitializedContext, properties: PropertyDescriptorArray) {
  let full_object_record = object.full_record().unwrap();
  FullObjectRecord::define_own_properties(full_object_record, context, JsReceiver::new(object), properties);
}

#[inline(always)]
pub fn has_property(object: Repr, hint: PropertySearchHint) -> Option<PropertyDescriptorSearchResult> {
  let full_object_record = object.full_record().unwrap();
  return FullObjectRecord::has_property(full_object_record, JsReceiver::new(object), hint);
}

#[inline(always)]
pub fn get_own_property(object: Repr, hint: OwnPropertySearchHint) -> Option<OwnPropertyDescriptorSearchResult> {
  let full_object_record = object.full_record().unwrap();
  return FullObjectRecord::get_own_property(full_object_record, JsReceiver::new(object), hint);
}

#[inline(always)]
pub fn get_own_property_raw_with(
  object: Repr,
  name: PropertyName,
  index: isize,
  mode: PropertyMode,
) -> Option<OwnPropertyDescriptorSearchResult> {
  let full_object_record = object.full_record().unwrap();
  return FullObjectRecord::get_own_property(
    full_object_record,
    JsReceiver::new(object),
    OwnPropertySearchHint::new_with(name, index, mode),
  );
}

#[inline(always)]
pub fn get_own_property_raw(object: Repr, name: PropertyName) -> Option<OwnPropertyDescriptorSearchResult> {
  let full_object_record = object.full_record().unwrap();
  return FullObjectRecord::get_own_property(full_object_record, JsReceiver::new(object), OwnPropertySearchHint::new(name));
}

#[inline(always)]
pub fn own_property_keys(object: Repr, context: impl ObjectRecordsInitializedContext) -> OwnKeys {
  let full_object_record = object.full_record().unwrap();
  return FullObjectRecord::own_property_keys(full_object_record, context, JsReceiver::new(object));
}
