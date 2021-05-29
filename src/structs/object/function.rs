use super::super::cell::*;
use super::super::internal_array::InternalArray;
use super::super::shadow_class::ShadowInstance;
use super::super::shape::Shape;
use super::property::Property;
use crate::context::Context;
use std::mem::size_of;

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct JsFunctionLayout;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct JsFunction(HeapLayout<JsFunctionLayout>);
impl_object!(JsFunction, HeapLayout<JsFunctionLayout>);

impl JsFunction {
  const SIZE: usize = size_of::<JsFunctionLayout>();
  pub fn new(context: impl Context, properties: InternalArray<Property>) -> JsFunction {
    let layout = HeapLayout::<JsFunctionLayout>::new_object(context, JsFunction::SIZE, Shape::function());
    let func = JsFunction(layout);
    func.cell().class().define_own_properties(context, properties);
    return func;
  }
}
