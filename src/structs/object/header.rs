use super::super::shape::Shape;
use crate::def::*;
use crate::utility::*;

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct Header {
  ///
  /// Use as object size information and used as forwarded pointer and mark bit.
  ///
  /// |---data----|--own properties count--|--size---|--object_record bit--|--size tag--|--type tag--|--mark bit--|
  /// |---17bit---|----------6bit-----------|--32bit--|--------1bit--------|----1bit----|----6bit----|----1bit----|
  ///
  field: Bitset<u64>,
}

const MARK_BIT_START: usize = 1;
const MARK_BIT_SIZE: usize = 1;
const TYPE_TAG_START: usize = MARK_BIT_START + MARK_BIT_SIZE;
const TYPE_TAG_SIZE: usize = 6;
const SIZE_TAG_START: usize = TYPE_TAG_START + TYPE_TAG_SIZE;
const SIZE_TAG_SIZE: usize = 1;
const OBJECT_RECORD_BIT_START: usize = SIZE_TAG_START + SIZE_TAG_SIZE;
const OBJECT_RECORD_BIT_SIZE: usize = 1;
const SIZE_FIELD_START: usize = OBJECT_RECORD_BIT_START + OBJECT_RECORD_BIT_SIZE;
const SIZE_FIELD_SIZE: usize = 32;
const OWN_PROPERTIES_COUNT_START: usize = SIZE_FIELD_START + SIZE_FIELD_SIZE;
const OWN_PROPERTIES_COUNT_SIZE: usize = 6;
const DATA_FIELD_START: usize = OWN_PROPERTIES_COUNT_START + OWN_PROPERTIES_COUNT_SIZE;
const _DATA_FIELD_SIZE: usize = 23;

impl Header {
  #[inline]
  pub fn init(&mut self) {
    self.field.assign(0);
  }

  #[inline]
  pub fn set_size(&mut self, size: u32) {
    debug_assert!(size <= 0xffffffff_u32, "Size must be less than 55bit integer");
    self.field.set(SIZE_TAG_START);
    let mut range = self.field.mask_lower_mut(SIZE_FIELD_START);
    range.assign(size as u64);
  }

  #[inline]
  pub fn size(&self) -> u32 {
    if self.is_size_used_as_size() {
      let mask = self
        .field
        .mask_range(SIZE_FIELD_START, SIZE_FIELD_START + SIZE_FIELD_SIZE);
      return mask.bits() as u32;
    }
    return 0;
  }

  #[inline]
  pub fn set_own_properties_len(&mut self, offset: u8) {
    return self
      .field
      .mask_range_mut(
        OWN_PROPERTIES_COUNT_START,
        OWN_PROPERTIES_COUNT_START + OWN_PROPERTIES_COUNT_SIZE,
      )
      .set(offset as usize);
  }

  #[inline]
  pub fn own_properties_len(&self) -> u8 {
    return self
      .field
      .mask_range(
        OWN_PROPERTIES_COUNT_START,
        OWN_PROPERTIES_COUNT_START + OWN_PROPERTIES_COUNT_SIZE,
      )
      .bits() as u8;
  }

  #[inline]
  pub fn data(&self) -> MaskedBitset<u64> {
    return self.field.mask_lower(DATA_FIELD_START);
  }

  #[inline]
  pub fn data_mut(&mut self) -> MaskedBitsetMut<u64> {
    return self.field.mask_lower_mut(DATA_FIELD_START);
  }

  #[inline]
  pub fn data_offset(&mut self, offset: usize) -> MaskedBitsetMut<u64> {
    return self.field.mask_lower_mut(DATA_FIELD_START + offset);
  }

  #[inline]
  pub fn set_forwarded_pointer(&mut self, addr: *mut Byte) {
    self.field.unset(SIZE_TAG_START);
    let mut mask = self.field.mask_lower_mut(SIZE_FIELD_START);
    mask.assign(addr as u64);
  }

  #[inline]
  pub fn forwarded_pointer(&self) -> *mut Byte {
    if !self.is_size_used_as_size() {
      return self.field.mask_lower(SIZE_FIELD_START).bits() as *mut Byte;
    }
    return 0xdeadbeef as *mut Byte;
  }

  #[inline]
  pub fn set_object_record(&mut self) {
    self.field.set(OBJECT_RECORD_BIT_START);
  }

  #[inline]
  pub fn has_object_record(&self) -> bool {
    return self.field.get(OBJECT_RECORD_BIT_START);
  }

  #[inline]
  pub fn mark(&mut self) {
    self.field.set(MARK_BIT_START);
  }

  #[inline]
  pub fn unmark(&mut self) {
    self.field.unset(MARK_BIT_START);
  }

  #[inline]
  pub fn is_marked(&self) -> bool {
    return self.field.get(MARK_BIT_START);
  }

  #[inline]
  pub fn set_shape(&mut self, shape: Shape) {
    let mut mask = self.field.mask_range_mut(TYPE_TAG_START, SIZE_TAG_START);
    mask.assign(shape.into());
  }

  #[inline]
  pub fn shape(&self) -> Shape {
    let mask = self.field.mask_range(TYPE_TAG_START, SIZE_TAG_START);
    return Shape::from_tag(mask.bits() as u8);
  }

  #[inline]
  fn is_size_used_as_size(&self) -> bool {
    return self.field.get(SIZE_TAG_START);
  }
}

impl std::fmt::Debug for Header {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return write!(
      f,
      "
{:?} {{
  size: {:?},
  shape: {:?},
  mark: {:?},
  is_size_used_as_size: {:?},
  has_object_record: {:?}
}}",
      std::any::type_name::<Header>(),
      self.size(),
      self.shape(),
      self.is_marked(),
      self.is_size_used_as_size(),
      self.has_object_record()
    );
  }
}

#[cfg(test)]
mod header_tests {
  use super::*;
  #[test]
  fn header_set_size_test() {
    let mut h = Header {
      field: Bitset::<u64>::new(),
    };
    h.set_size(4294967295);
    assert_eq!(h.size(), 4294967295);
  }

  #[test]
  fn header_set_forwarded_pointer() {
    let p = 0xdeadbeef as *mut Byte;
    let mut h = Header {
      field: Bitset::<u64>::new(),
    };
    h.set_forwarded_pointer(p);
    assert_eq!(h.forwarded_pointer() as usize, p as usize);
  }

  #[test]
  fn header_mark() {
    let mut h = Header {
      field: Bitset::<u64>::new(),
    };
    h.mark();
    assert_eq!(h.is_marked(), true);
  }

  #[test]
  fn header_unmark() {
    let mut h = Header {
      field: Bitset::<u64>::new(),
    };
    h.mark();
    assert_eq!(h.is_marked(), true);
    h.unmark();
    assert_eq!(h.is_marked(), false);
  }

  #[test]
  fn header_set_shape() {
    let mut h = Header {
      field: Bitset::<u64>::new(),
    };
    h.set_shape(Shape::undefined());
    assert_eq!(h.shape(), Shape::undefined());
  }
}
