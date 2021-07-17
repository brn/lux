use super::super::hash_map::{DefaultHasher, Hasher, PredefinedHash};
use super::super::internal_array::{InternalArray, InternalArrayIterator, RefInternalArrayIterator};
use super::super::repr::Repr;
use crate::context::{AllocationOnlyContext, ObjectRecordsInitializedContext};
use std::char::decode_utf16;
use std::hash::Hash;

pub type FixedU16CodePointArray = InternalArray<u16>;
pub type FixedU16CodePointArrayIterator = InternalArrayIterator<u16>;
pub type RefFixedU16CodePointArrayIterator<'a> = RefInternalArrayIterator<'a, u16>;

pub fn from_utf8(context: impl ObjectRecordsInitializedContext, str: &str) -> FixedU16CodePointArray {
  let u16_vec = str.encode_utf16().collect::<Vec<_>>();
  let mut array = FixedU16CodePointArray::new(context, u16_vec.len());
  for c in u16_vec {
    array.push(c);
  }
  return array;
}

impl FixedU16CodePointArray {
  pub fn null() -> Self {
    return FixedU16CodePointArray::from(Repr::invalid());
  }

  pub fn from_u16_vec(context: impl ObjectRecordsInitializedContext, vec: &Vec<u16>) -> Self {
    return FixedU16CodePointArray::copy_construct(context, vec.len(), vec.len(), vec.as_ptr());
  }

  pub fn from_utf8(context: impl ObjectRecordsInitializedContext, str: &str) -> FixedU16CodePointArray {
    return from_utf8(context, str);
  }

  pub fn to_utf8(&self) -> String {
    return decode_utf16(self.into_iter())
      .map(|r| r.unwrap_or('#'))
      .collect::<String>();
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

impl PredefinedHash for FixedU16CodePointArray {
  fn prepare_hash(&mut self, _: impl AllocationOnlyContext) {}

  fn predefined_hash(&self) -> u64 {
    let mut h = DefaultHasher::default();
    for u in *self {
      h.write_u16(u);
    }
    return h.finish();
  }
}
