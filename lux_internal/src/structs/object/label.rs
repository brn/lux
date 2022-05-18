use super::super::cell::*;

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct JsLabelLayout {
  label_id: u64,
}

#[repr(C)]
pub struct JsLabel(HeapLayout<JsLabelLayout>);
impl_object!(JsLabel, HeapLayout<JsLabelLayout>);
