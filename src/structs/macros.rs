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
    crate::structs::object::Property::new($context, crate::structs::object::PropertyName::from_utf8_string($context, $name), a)
  }};
}
