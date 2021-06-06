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
  use super::super::object::{Property, PropertyDescriptor};
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
  fn define_own_property_test() {
    let context = LuxContext::new_until_js_object_records();
    let mut full_object_record = FullObjectRecord::new(context, 8, Shape::null(), 1);
    let object_record = ObjectRecord::from(full_object_record);
    assert_eq!(object_record.size(), 16);
    assert_eq!(object_record.size_contains_object_record() as usize, 16 + PTR_SIZE);
    assert_eq!(object_record.shape_tag(), ShapeTag::Null);
    assert!(!object_record.is_light_layout());
    assert!(full_object_record.is_fast_mode());
    assert!(!full_object_record.is_external_field_mode());
    assert!(!full_object_record.is_element_mode());
    assert!(!full_object_record.is_slow_mode());
    assert!(!full_object_record.is_transited());
    assert!(full_object_record.is_extensible());
    assert_eq!(full_object_record.fast_properties_capacity(), 1);
    assert_eq!(full_object_record.fast_properties_len(), 0);
  }
}
