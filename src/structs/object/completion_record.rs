use super::super::cell::*;
use super::super::shape::Shape;
use super::label::JsLabel;
use crate::context::AllocationOnlyContext;
use std::mem::size_of;

pub enum CompletionType {
  Normal = 1, // 01
  Return,     // 10
  Continue,   // 11
  Break,      // 100
  Throw,      // 101
}

const COMPLETION_TYPE_USED_BITS: usize = 3;

#[repr(C)]
#[drive(Copy, Clone)]
pub struct CompletionRecord {
  type_and_value: Bitset<u64>,
  target: Option<JsLabel>,
}

impl CompletionRecord {
  const BASE_SIZE: usize = Cell::SIZE;
  pub fn new(
    context: &mut impl AllocationOnlyContext,
    completion_type: CompletionType,
    value: Repr,
    target: Option<JsLabel>,
  ) -> CompletionRecord {
    let mut completion_record = CompletionRecord {
      type_and_value: Bitset::<u64>::new(),
      target,
    };
    let mask = completion_record.type_and_value.mask_lower(COMPLETION_TYPE_USED_BITS);
    if value.is_boxed() {
      mask.assign(value.unbox_unchecked() as usize);
    } else {
      mask.assign(value.to_number().to_bits());
    }
    completion_record.type_and_value = mask.into();
    return completion_record;
  }
}
