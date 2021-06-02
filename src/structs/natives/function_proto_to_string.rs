use super::super::internal_array::InternalArray;
use super::super::object::JsReceiver;
use super::super::repr::Repr;
use super::native_function::NativeFunctionCall;
use crate::context::Context;

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct FunctionProtoToString;

impl NativeFunctionCall for FunctionProtoToString {
  // https://262.ecma-international.org/11.0/#sec-function.prototype.tostring
  // arguments must be
  //
  // +-+---------------+
  // |1| function name |
  // +-+---------------+
  //
  fn call(context: impl Context, receiver: JsReceiver, arguments: InternalArray<Repr>) -> Option<Repr> {
    debug_assert!(arguments.length() == 1);
    debug_assert!(arguments[0].is_string());
    if !receiver.is_function() {
      todo!("Need to implements type error");
    }
    let func = receiver.to_function();
    if func.is_builtin_function() {
      return Some(func.get_native_to_string_result(context).unwrap().into());
    }
    todo!("Need to implements user defined function toString");
  }
}
