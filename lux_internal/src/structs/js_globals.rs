// use super::cell::*;
// use super::shape::*;
// use crate::context::Context;
// use crate::utility::*;
// use std::mem::size_of;

// #[repr(C)]
// #[derive(Copy, Clone)]
// pub struct JsBoolean(HeapLayout<VoidHeapBody>);
// impl_object!(JsBoolean, HeapLayout<VoidHeapBody>);

// impl JsBoolean {
//   pub const TYPE: Shape = Shape::boolean();
//   pub const SIZE: usize = size_of::<VoidHeapBody>();
//   pub const VALUE_BIT: usize = 1;

//   pub fn persist<'a>(context: impl Context, val: bool) -> JsBoolean {
//     let layout = HeapLayout::<VoidHeapBody>::persist(context, JsBoolean::SIZE, Shape::boolean());
//     return JsBoolean::init(layout, val);
//   }

//   pub fn is_true(&self) -> bool {
//     return HeapObject::get_data_field(self).get(JsBoolean::VALUE_BIT);
//   }

//   #[inline]
//   fn init(mut layout: HeapLayout<VoidHeapBody>, val: bool) -> JsBoolean {
//     let mut boolean = JsBoolean(layout);
//     if val {
//       HeapObject::get_data_field(&boolean).set(JsBoolean::VALUE_BIT);
//     }
//     return boolean;
//   }
// }

// #[cfg(test)]
// mod js_global_test {
//   use super::*;

//   use crate::context::LuxContext;

//   #[test]
//   fn js_boolean_init_test() {
//     let mut mc = LuxContext::new();
//     let js_true = JsBoolean::persist(mc, true);
//     assert_eq!(js_true.size(), JsBoolean::SIZE);
//     assert_eq!(js_true.shape(), Shape::boolean());
//   }

//   #[test]
//   fn js_boolean_true_test() {
//     let mut mc = LuxContext::new();
//     let js_true = JsBoolean::persist(mc, true);
//     assert_eq!(js_true.is_true(), true);
//   }

//   #[test]
//   fn js_boolean_false_test() {
//     let mut mc = LuxContext::new();
//     let js_true = JsBoolean::persist(mc, false);
//     assert_eq!(js_true.is_true(), false);
//   }
// }
