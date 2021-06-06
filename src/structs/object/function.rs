use super::super::cell::*;
use super::super::internal_array::InternalArray;
use super::super::natives::{NativeFunction, NativeFunctionCall};
use super::super::object_record::{FullObjectRecord, ObjectSkin};
use super::super::repr::Repr;
use super::super::shape::Shape;
use super::super::string::FlatString;
use super::property::Property;
use super::receiver::JsReceiver;
use crate::context::Context;
use crate::utility::*;
use num_derive::FromPrimitive;
use std::mem::size_of;

#[derive(Copy, Clone, FromPrimitive, PartialEq)]
pub enum BuiltinJsFunctionType {
  None = 0,
  Array = 2,
  ArrayProtoConcat,
  ArrayProtoCopyWithin,
  ArrayProtoEntries,
  ArrayProtoEvery,
  ArrayProtoFill,
  ArrayProtoFilter,
  ArrayProtoFind,
  ArrayProtoFindIndex,
  ArrayProtoFlat,
  ArrayProtoFlatMap,
  ArrayProtoForEach,
  ArrayProtoIncludes,
  ArrayProtoIndexOf,
  ArrayProtoJoin,
  ArrayProtoKeys,
  ArrayProtoLastIndexOf,
  ArrayProtoMap,
  ArrayProtoPop,
  ArrayProtoPush,
  ArrayProtoReduce,
  ArrayProtoReduceRight,
  ArrayProtoReserve,
  ArrayProtoShift,
  ArrayProtoSlice,
  ArrayProtoSome,
  ArrayProtoSort,
  ArrayProtoToLocaleString,
  ArrayProtoToString,
  ArrayProtoUnshift,
  ArrayProtoValues,
  ArrayProto,
  BigInt,
  BigInt64Array,
  BigUint64Array,
  Boolean,
  DataView,
  Date,
  DecodeURI,
  DecodeURIComponent,
  EncodeURI,
  EncodeURIComponent,
  Error,
  Eval,
  EvalError,
  Float32Array,
  Float64Array,
  Function,
  FunctionProtoToString,
  Int8Array,
  Int16Array,
  Int32Array,
  IsFinite,
  IsNaN,
  JsonParse,
  JsonStringify,
  Map,
  Number,
  ObjectProtoToString,
  ObjectProtoValueOf,
  ParseFloat,
  ParseInt,
  Promise,
  PromiseProtoThen,
  PromiseAll,
  PromiseReject,
  PromiseResolve,
  Proxy,
  RangeError,
  ReferenceError,
  RegExp,
  Set,
  SharedArrayBuffer,
  String,
  Symbol,
  TypeError,
  Uint8Array,
  Uint8ClampedArray,
  Uint16Array,
  Uint32Array,
  URIError,
  WeakMap,
  WeakSet,
}
impl Default for BuiltinJsFunctionType {
  fn default() -> BuiltinJsFunctionType {
    return BuiltinJsFunctionType::None;
  }
}

const FUNCTION_TYPE_MASK: u32 = 0xFFFFFFFE;

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct JsFunctionLayout;

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct BuiltinJsFunctionLayout {
  builtin_type: BuiltinJsFunctionType,
  to_string_result: FlatString,
  function: Repr,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct JsFunction(HeapLayout<JsFunctionLayout>);
impl_object!(JsFunction, HeapLayout<JsFunctionLayout>);

impl JsFunction {
  pub const SIZE: usize = size_of::<JsFunctionLayout>();
  pub const BUILTIN_BIT: usize = 1;
  pub fn new(context: impl Context, properties: InternalArray<Property>) -> JsFunction {
    let layout = HeapLayout::<JsFunctionLayout>::new_object(context, context.object_records().function_record());
    let func = JsFunction(layout);
    FullObjectRecord::define_own_properties(
      func.full_record_unchecked(),
      context,
      JsReceiver::new(func.into()),
      properties,
    );
    return func;
  }

  pub fn new_builtin(
    context: impl Context,
    properties: InternalArray<Property>,
    function: Repr,
    func_type: BuiltinJsFunctionType,
    to_string_result: FlatString,
  ) -> JsFunction {
    let mut layout =
      HeapLayout::<BuiltinJsFunctionLayout>::new_object(context, context.object_records().builtin_function_record());
    let func = JsFunction(HeapLayout::<JsFunctionLayout>::wrap(layout.as_addr()));
    layout.builtin_type = func_type;
    layout.to_string_result = to_string_result;
    layout.function = function;
    return func;
  }

  pub fn get_native_function<NativeFunctionType: NativeFunctionCall>(
    &self,
  ) -> Option<NativeFunction<NativeFunctionType>> {
    match JsFunction::get_builtin_layout(self) {
      Some(layout) => return Some(NativeFunction::<NativeFunctionType>::from(layout.function)),
      _ => return None,
    }
  }

  pub fn is_builtin_function(&self) -> bool {
    return HeapObject::get_data_field(self, JsFunction::BUILTIN_BIT);
  }

  pub fn is_builtin_function_type_of(&self, func_type: BuiltinJsFunctionType) -> bool {
    match JsFunction::get_builtin_layout(self) {
      Some(layout) => {
        return layout.builtin_type == func_type;
      }
      _ => return false,
    };
  }

  pub fn get_builtin_function_type(&self) -> Option<BuiltinJsFunctionType> {
    match JsFunction::get_builtin_layout(self) {
      Some(layout) => {
        return Some(layout.builtin_type);
      }
      _ => return None,
    };
  }

  pub fn get_native_to_string_result(&self, context: impl Context) -> Option<FlatString> {
    match JsFunction::get_builtin_layout(self) {
      Some(layout) => {
        return Some(layout.to_string_result);
      }
      _ => return None,
    };
  }

  pub fn is_symbol_constructor(&self) -> bool {
    return self.is_builtin_function_type_of(BuiltinJsFunctionType::Symbol);
  }

  fn get_builtin_layout(this: &JsFunction) -> Option<HeapLayout<BuiltinJsFunctionLayout>> {
    if !this.is_builtin_function() {
      return None;
    }
    return Some(HeapLayout::<BuiltinJsFunctionLayout>::wrap(this.0.as_addr()));
  }
}
