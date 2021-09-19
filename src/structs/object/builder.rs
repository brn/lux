use super::super::internal_array::InternalArray;
use super::super::repr::Repr;
use super::super::string::FlatString;
use super::function::{BuiltinJsFunctionType, JsFunction};
use super::object::JsObject;
use super::property::{Property, PropertyName};
use crate::context::{Context, LuxContext};
use std::vec::Vec;

pub struct FunctionBuilder {
  context: Repr,
  properties: Vec<Property>,
  is_builtin: bool,
  prototype: JsObject,
  builtin_function_type: BuiltinJsFunctionType,
  builtin_to_string_str: FlatString,
  function: Repr,
}

impl FunctionBuilder {
  pub fn new(context: impl Context) -> Self {
    return FunctionBuilder {
      context: context.into(),
      properties: Vec::new(),
      is_builtin: false,
      prototype: JsObject::default(),
      builtin_function_type: BuiltinJsFunctionType::default(),
      builtin_to_string_str: FlatString::default(),
      function: Repr::invalid(),
    };
  }

  pub fn set_function_body(&mut self, function: Repr) -> &mut Self {
    self.function = function;
    return self;
  }

  pub fn as_builtin(&mut self, func_type: BuiltinJsFunctionType, to_string_str: FlatString) -> &mut Self {
    self.is_builtin = true;
    self.builtin_function_type = func_type;
    self.builtin_to_string_str = to_string_str;
    return self;
  }

  pub fn define_property_from_utf8(&mut self, str: &str, value: Repr) -> &mut Self {
    self.properties.push(new_property!(LuxContext::from(self.context), str: str, value));
    return self;
  }

  pub fn define_property(&mut self, name: PropertyName, value: Repr) -> &mut Self {
    self
      .properties
      .push(new_property!(LuxContext::from(self.context), value: name, value));
    return self;
  }

  pub fn set_prototype(&mut self, prototype: JsObject) -> &mut Self {
    self.prototype = prototype;
    return self;
  }

  pub fn build(&self) -> JsFunction {
    let c = LuxContext::from(self.context);
    let mut properties = c.globals().empty_internal_array::<Property>();
    if self.properties.len() > 0 {
      properties = InternalArray::<Property>::new(c, self.properties.len());
      for p in &self.properties {
        properties.push(*p);
      }
    }
    if self.is_builtin {
      return JsFunction::new_builtin(c, properties, self.function, self.builtin_function_type, self.builtin_to_string_str);
    }
    return JsFunction::new(c, properties);
  }
}
