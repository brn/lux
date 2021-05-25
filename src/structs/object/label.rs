use super::super::cell::*;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct JsLabelLayout {
  label_id: u64,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct JsLabel(HeapLayout<JsLabelLayout>);
impl_object!(JsLabel, HeapLayout<JsLabelLayout>);
