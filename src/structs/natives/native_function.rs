use super::super::cell::*;
use super::super::internal_array::InternalArray;
use super::super::object::JsReceiver;
use super::super::repr::Repr;
use super::super::shape::Shape;
use crate::context::Context;
use crate::def::*;

pub trait NativeFunctionCall: Copy + Default {
  fn call(context: impl Context, receiver: JsReceiver, arguments: InternalArray<Repr>) -> Option<Repr>;
}

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct NativeFunctionLayout<F: NativeFunctionCall> {
  callable: F,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct NativeFunction<F: NativeFunctionCall>(HeapLayout<NativeFunctionLayout<F>>);
impl_object!(
  NativeFunction<F: NativeFunctionCall>,
  HeapLayout<NativeFunctionLayout<F>>
);

pub const NATIVE_FUNCTION_SIZE: usize = PTR_SIZE;

impl<F: NativeFunctionCall> NativeFunction<F> {
  pub fn new(context: impl Context, func: F) -> NativeFunction<F> {
    let mut layout =
      HeapLayout::<NativeFunctionLayout<F>>::new(context, context.object_records().native_function_record());
    layout.callable = func;
    return NativeFunction(layout);
  }
}
