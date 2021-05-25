use super::super::hash_map::{ContextHash, ContextHashMut};
use super::super::internal_array::InternalArray;
use crate::context::{AllocationOnlyContext, Context};
use std::hash::Hash;
use std::hash::Hasher;

pub type FixedU16CodePointArray = InternalArray<u16>;

pub fn from_utf8(context: &mut impl AllocationOnlyContext, str: &str) -> FixedU16CodePointArray {
  let u16_vec = str.encode_utf16().collect::<Vec<_>>();
  let mut array = FixedU16CodePointArray::new(context, u16_vec.len());
  for c in u16_vec {
    array.push(c);
  }
  return array;
}

impl FixedU16CodePointArray {
  pub fn from_utf8(context: &mut impl AllocationOnlyContext, str: &str) -> FixedU16CodePointArray {
    return from_utf8(context, str);
  }
}

impl std::cmp::PartialEq for FixedU16CodePointArray {
  fn eq(&self, a: &Self) -> bool {
    if self.length() != a.length() {
      return false;
    }
    for i in 0..self.length() {
      //      println!("{} {}", self[i], a[i]);
      if self[i] != a[i] {
        return false;
      }
    }
    return true;
  }
}
impl std::cmp::Eq for FixedU16CodePointArray {}
impl Hash for FixedU16CodePointArray {
  fn hash<H: Hasher>(&self, state: &mut H) {
    for u in *self {
      u.hash(state);
    }
  }
}

impl ContextHash for FixedU16CodePointArray {
  fn context_hash<H: Hasher>(&self, _: &mut impl Context, state: &mut H) -> u64 {
    for u in *self {
      u.hash(state);
    }
    return state.finish();
  }
}

impl ContextHashMut for FixedU16CodePointArray {
  fn context_hash_mut<H: Hasher>(&mut self, _: &mut impl Context, state: &mut H) -> u64 {
    for u in *self {
      u.hash(state);
    }
    return state.finish();
  }
}
