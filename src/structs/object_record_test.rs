#[cfg(test)]
mod object_record_test {
  use super::super::object_record::*;
  use super::super::shape::{Shape, ShapeTag};
  use crate::context::LuxContext;
  use crate::def::*;
  use crate::utility::*;

  #[test]
  fn init_test() {
    let context = LuxContext::new_only_allocator();
    let o = ObjectRecord::new(context, 12, Shape::null());
    assert_eq!(o.size(), 12);
    assert_eq!(o.size_contains_object_record() as usize, 12 + PTR_SIZE);
    assert_eq!(o.shape_tag(), ShapeTag::Null);
    assert!(o.is_light_layout());
  }

  #[test]
  fn store_bit_flag_test() {
    let context = LuxContext::new_only_allocator();
    let mut o = ObjectRecord::new(context, 12, Shape::null());
    o.data_field().unset(1);
    assert!(o.is_light_layout());
  }
}

#[cfg(test)]
mod full_object_record_test {
  use super::super::internal_array::InternalArray;
  use super::super::object::{JsObject, PropertyName};
  use super::super::object_record::*;
  use super::super::repr::Repr;
  use super::super::shape::{Shape, ShapeTag};
  use crate::context::LuxContext;
  use crate::def::*;
  use crate::utility::*;

  #[test]
  fn init_test() {
    let context = LuxContext::new_only_allocator();
    let full_object_record = FullObjectRecord::new(context, 12, Shape::null(), 0);
    let object_record = ObjectRecord::from(full_object_record);
    assert_eq!(object_record.size(), 12);
    assert_eq!(object_record.size_contains_object_record() as usize, 12 + PTR_SIZE);
    assert_eq!(object_record.shape_tag(), ShapeTag::Null);
    assert!(!object_record.is_light_layout());
    assert!(full_object_record.is_fast_mode());
    assert!(!full_object_record.is_external_field_mode());
    assert!(!full_object_record.is_element_mode());
    assert!(!full_object_record.is_slow_mode());
    assert_eq!(full_object_record.own_properties_len(), 0);
    assert!(!full_object_record.is_transited());
    assert!(full_object_record.is_extensible());
  }

  #[test]
  fn define_fast_own_property_test() {
    let context = LuxContext::new_until_js_object_records();
    let mut full_object_record = FullObjectRecord::new(context, 8, Shape::object(), 10);
    let object_record = ObjectRecord::from(full_object_record);
    assert_eq!(object_record.size(), 88);
    assert_eq!(object_record.size_contains_object_record() as usize, 88 + PTR_SIZE);
    assert_eq!(object_record.shape_tag(), ShapeTag::Object);
    assert!(!object_record.is_light_layout());
    assert!(full_object_record.is_fast_mode());
    assert!(!full_object_record.is_external_field_mode());
    assert!(!full_object_record.is_element_mode());
    assert!(!full_object_record.is_slow_mode());
    assert!(!full_object_record.is_transited());
    assert!(full_object_record.is_extensible());

    let mut object = JsObject::new(context, full_object_record);
    for i in 0..10 {
      let pname = format!("test_property_{}", i);
      JsObject::define_own_property(
        context,
        object.into(),
        new_property!(
          context,
          value: PropertyName::from_utf8_string(context, &pname),
          Repr::from(i as f64)
        ),
      );
    }

    let record = object.full_record_unchecked();
    assert_eq!(full_object_record.fast_properties_capacity(), 10);
    assert_eq!(full_object_record.fast_properties_len(), 0);
    assert!(record.is_fast_mode());
    assert_eq!(record.fast_properties_capacity(), 10);
    assert_eq!(record.fast_properties_len(), 10);
  }

  #[test]
  fn get_own_property_test() {
    let context = LuxContext::new_until_js_object_records();
    let mut full_object_record = FullObjectRecord::new(context, 8, Shape::object(), 10);
    let object_record = ObjectRecord::from(full_object_record);
    assert_eq!(object_record.size(), 88);
    assert_eq!(object_record.size_contains_object_record() as usize, 88 + PTR_SIZE);
    assert_eq!(object_record.shape_tag(), ShapeTag::Object);
    assert!(!object_record.is_light_layout());
    assert!(full_object_record.is_fast_mode());
    assert!(!full_object_record.is_external_field_mode());
    assert!(!full_object_record.is_element_mode());
    assert!(!full_object_record.is_slow_mode());
    assert!(!full_object_record.is_transited());
    assert!(full_object_record.is_extensible());

    let mut object = JsObject::new(context, full_object_record);
    for i in 0..10 {
      let pname = format!("test_property_{}", i);
      JsObject::define_own_property(
        context,
        object.into(),
        new_property!(
          context,
          value: PropertyName::from_utf8_string(context, &pname),
          Repr::from(i as f64)
        ),
      );
    }
    for i in 0..10 {
      let pname = format!("test_property_{}", i);
      let found = JsObject::get_own_property_by_name(object.into(), PropertyName::from_utf8_string(context, &pname));
      assert!(found.is_some());
      assert_eq!(found.unwrap().descriptor().value().to_number_unchecked(), i as f64);
    }

    let record = object.full_record_unchecked();
    assert_eq!(full_object_record.fast_properties_capacity(), 10);
    assert_eq!(full_object_record.fast_properties_len(), 0);
    assert!(record.is_fast_mode());
    assert_eq!(record.fast_properties_capacity(), 10);
    assert_eq!(record.fast_properties_len(), 10);
  }

  #[test]
  fn get_own_property_by_hint_test() {
    let context = LuxContext::new_until_js_object_records();
    let mut full_object_record = FullObjectRecord::new(context, 8, Shape::object(), 10);
    let object_record = ObjectRecord::from(full_object_record);
    assert_eq!(object_record.size(), 88);
    assert_eq!(object_record.size_contains_object_record() as usize, 88 + PTR_SIZE);
    assert_eq!(object_record.shape_tag(), ShapeTag::Object);
    assert!(!object_record.is_light_layout());
    assert!(full_object_record.is_fast_mode());
    assert!(!full_object_record.is_external_field_mode());
    assert!(!full_object_record.is_element_mode());
    assert!(!full_object_record.is_slow_mode());
    assert!(!full_object_record.is_transited());
    assert!(full_object_record.is_extensible());

    let mut vec = Vec::<OwnPropertyDescriptorSearchResult>::with_capacity(10);
    let mut object = JsObject::new(context, full_object_record);
    for i in 0..10 {
      let pname = format!("test_property_{}", i);
      vec.push(JsObject::define_own_property(
        context,
        object.into(),
        new_property!(
          context,
          value: PropertyName::from_utf8_string(context, &pname),
          Repr::from(i as f64)
        ),
      ));
    }
    for i in 0..10 {
      let pname = format!("test_property_{}", i);
      let hint = OwnPropertySearchHint::from(vec[i]);
      let found = JsObject::get_own_property(object.into(), hint);
      assert!(found.is_some());
      assert_eq!(found.unwrap().descriptor().value().to_number_unchecked(), i as f64);
    }

    let record = object.full_record_unchecked();
    assert_eq!(full_object_record.fast_properties_capacity(), 10);
    assert_eq!(full_object_record.fast_properties_len(), 0);
    assert!(record.is_fast_mode());
    assert_eq!(record.fast_properties_capacity(), 10);
    assert_eq!(record.fast_properties_len(), 10);
  }

  #[test]
  fn define_external_own_property_test() {
    let context = LuxContext::new_until_js_object_records();
    let mut full_object_record = FullObjectRecord::new(context, 8, Shape::object(), 10);
    let object_record = ObjectRecord::from(full_object_record);
    assert_eq!(object_record.size(), 88);
    assert_eq!(object_record.size_contains_object_record() as usize, 88 + PTR_SIZE);
    assert_eq!(object_record.shape_tag(), ShapeTag::Object);
    assert!(!object_record.is_light_layout());
    assert!(full_object_record.is_fast_mode());
    assert!(!full_object_record.is_external_field_mode());
    assert!(!full_object_record.is_element_mode());
    assert!(!full_object_record.is_slow_mode());
    assert!(!full_object_record.is_transited());
    assert!(full_object_record.is_extensible());

    let mut object = JsObject::new(context, full_object_record);
    for i in 0..13 {
      let pname = format!("test_property_{}", i);
      JsObject::define_own_property(
        context,
        object.into(),
        new_property!(
          context,
          value: PropertyName::from_utf8_string(context, &pname),
          Repr::from(i as f64)
        ),
      );
    }

    let record = object.full_record_unchecked();
    assert_eq!(full_object_record.fast_properties_capacity(), 10);
    assert_eq!(full_object_record.fast_properties_len(), 0);
    assert!(record.is_external_field_mode());
    assert_eq!(record.fast_properties_capacity(), 10);
    assert_eq!(record.fast_properties_len(), 10);
    assert_eq!(record.own_properties_len(), 13);
  }
}
