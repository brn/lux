#[cfg(test)]
mod object_record_test {
  use super::super::object_record::*;
  use super::super::shape::Shape;
  use crate::context::LuxContext;

  #[test]
  fn test_init_object_record() {
    let context = LuxContext::new_only_allocator();
    let o = ObjectRecord::new(context, 12, Shape::null());
  }
}
