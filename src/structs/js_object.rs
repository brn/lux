use super::cell::*;
use super::natives::{FunctionProtoToString, NativeFunction, NativeFunctionCall};
use super::object::{BuiltinJsFunctionType, JsFunction, JsObject, Property, WellKnownSymbolType};
use super::object_record::ObjectSkin;
use super::shape::Shape;
use crate::context::*;

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct BuiltinsLayout {
  symbol_constructor: JsFunction,
  function_prototype: JsObject,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct Builtins(HeapLayout<BuiltinsLayout>);
impl_object!(Builtins, HeapLayout<BuiltinsLayout>);

impl Builtins {
  pub const SIZE: usize = std::mem::size_of::<BuiltinsLayout>();
  pub fn new(context: impl Context) -> Builtins {
    let mut layout = HeapLayout::<BuiltinsLayout>::new(context, context.object_records().builtins_record());
    let builtins = Builtins(layout);
    layout.function_prototype = Builtins::init_function_prototype(context);
    layout.symbol_constructor = Builtins::init_symbol_constructor(context);
    return builtins;
  }

  pub fn symbol_constructor(&self) -> JsFunction {
    return self.symbol_constructor;
  }

  fn init_function_prototype(context: impl Context) -> JsObject {
    // let function_proto_to_string = JsFunction::new_builtin(
    //   context,
    //   context.empty_internal_array::<Property>(),
    //   NativeFunction::new(context, FunctionProtoToString::default()).into(),
    //   BuiltinJsFunctionType::FunctionProtoToString,
    //   context.to_string_tag_symbol_str(),
    // );
    // let props = fixed_array!(
    //   type: Property,
    //   context: context,
    //   capacity: 13,
    //   new_property!(
    //     context,
    //     str: "toString",
    //     function_proto_to_string.into()
    //   )
    // );
    let object = JsObject::new(context, context.object_records().function_prototype_record());
    return object;
  }

  //  fn init_object_prototype(context: impl Context) -> JsObject {}

  fn init_symbol_constructor(context: impl Context) -> JsFunction {
    let symbol_constructor_props = fixed_array!(
      type: Property,
      context: context,
      capacity: 13,
      new_property!(
        context,
        str: "asyncIterator",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::AsyncIterator)
          .into()
      ),
      new_property!(
        context,
        str: "hasInstance",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::HasInstance)
          .into()
      ),
      new_property!(
        context,
        str: "isConcatSpreadable",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::IsConcatSpreadable)
          .into()
      ),
      new_property!(
        context,
        str: "iterator",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::Iterator)
          .into()
      ),
      new_property!(
        context,
        str: "match",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::Match)
          .into()
      ),
      new_property!(
        context,
        str: "matchAll",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::MatchAll)
          .into()
      ),
      new_property!(
        context,
        str: "replace",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::Replace)
          .into()
      ),
      new_property!(
        context,
        str: "search",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::Search)
          .into()
      ),
      new_property!(
        context,
        str: "species",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::Species)
          .into()
      ),
      new_property!(
        context,
        str: "split",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::Split)
          .into()
      ),
      new_property!(
        context,
        str: "toPrimitive",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::ToPrimitive)
          .into()
      ),
      new_property!(
        context,
        str: "toStringTag",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::ToStringTag)
          .into()
      ),
      new_property!(
        context,
        str: "unscopables",
        context
          .symbol_registry()
          .get_wellknown_symbol(WellKnownSymbolType::Unscopables)
          .into()
      )
    );
    return JsFunction::new(context, symbol_constructor_props);
  }
}

#[cfg(test)]
mod builtins_test {
  use super::super::shape::ShapeTag;
  use super::*;
  use crate::context::LuxContext;

  #[test]
  fn builtins_symbol_constructor_test() {
    let context = LuxContext::new();
    let builtins = Builtins::new(context);
    let symbol_constructor = builtins.symbol_constructor();
    assert_eq!(symbol_constructor.shape().tag(), ShapeTag::Function);
  }
}
