use super::bitutil::*;
use num::Unsigned;
use num_traits::{NumAssign, PrimInt, ToPrimitive};
use std::cell::Cell;
use std::mem::size_of;
use std::ops::*;

pub trait BitNum: PrimInt + NumAssign + std::fmt::Display + std::fmt::Binary + Default {}

impl BitNum for u8 {}
impl BitNum for u16 {}
impl BitNum for u32 {}
impl BitNum for u64 {}

pub trait BitOperator<BitNum> {
  fn bits_ref(&self) -> &BitNum;

  fn bits_ref_mut(&mut self) -> &mut BitNum;

  fn bits(&self) -> BitNum;

  fn assign(&mut self, bit_value: BitNum);

  fn set(&mut self, index: usize);

  fn set_raw(&mut self, value: BitNum);

  fn unset(&mut self, index: usize);

  fn assign_and(&mut self, value: BitNum);

  fn assign_or(&mut self, value: BitNum);

  fn get(&self, index: usize) -> bool;

  fn is_full(&self) -> bool;

  fn mask(&self, mask: BitNum) -> BitNum;

  fn right_most_empty_slot(&self) -> u32;
}

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct Bitset<BitType: BitNum> {
  bit_field: BitType,
}

macro_rules! impl_bit_operator_for_usize {
  ($T:ty, $mod:ident) => {
    impl BitOperator<$T> for $mod<$T> {
      _priv_impl_bit_operator_for_usize_body!($T, $mod);

      #[inline]
      fn assign(&mut self, bit_value: $T) {
        self.bit_field = bit_value;
      }
    }
  };
  (mask; $T:ty, $mod:ident) => {
    impl<'a> BitOperator<$T> for $mod<'a, $T> {
      _priv_impl_bit_operator_for_usize_body!($T, $mod);

      #[inline]
      fn assign(&mut self, bit_value: $T) {
        unimplemented!();
      }
    }
  };
  (mask_mut; $T:ty, $mod:ident) => {
    impl<'a> BitOperator<$T> for $mod<'a, $T> {
      _priv_impl_bit_operator_for_usize_body!($T, $mod);

      #[inline]
      fn assign(&mut self, bit_value: $T) {
        self.bit_field = bit_value;
        *self.base = (self.bit_field << self.shift) | self.masked_bits;
      }
    }
  };
}

macro_rules! _priv_impl_bit_operator_for_usize_body {
  ($T:ty, $mod:ident) => {
    #[inline]
    fn bits_ref(&self) -> &$T {
      return &self.bit_field;
    }

    fn bits_ref_mut(&mut self) -> &mut $T {
      return &mut self.bit_field;
    }

    #[inline]
    fn bits(&self) -> $T {
      return self.bit_field;
    }

    #[inline]
    fn set(&mut self, index: usize) {
      debug_assert!(
        index - 1 <= size_of::<$T>() * 8,
        "Bitset::set index must be lower than BitType size"
      );
      self.assign(self.bits() | (1 << (index - 1)));
    }

    #[inline]
    fn set_raw(&mut self, value: $T) {
      self.assign(self.bits() | value);
    }

    #[inline]
    fn unset(&mut self, index: usize) {
      debug_assert!(
        index - 1 <= size_of::<$T>() * 8,
        "Bitset::set index must be lower than BitType size"
      );
      self.assign(self.bits() & !(1 << (index - 1)));
    }

    #[inline]
    fn assign_and(&mut self, value: $T) {
      self.assign(self.bits() & value);
    }

    #[inline]
    fn assign_or(&mut self, value: $T) {
      self.assign(self.bits() | value);
    }

    #[inline]
    fn get(&self, index: usize) -> bool {
      debug_assert!(
        index - 1 <= size_of::<$T>() * 8,
        "Bitset::set index must be lower than BitType size"
      );
      let i = 1 << (index - 1);
      return self.bits() & i == i;
    }

    #[inline]
    fn is_full(&self) -> bool {
      return self.bits() == !0;
    }

    #[inline]
    fn mask(&self, mask: $T) -> $T {
      return self.bits() & mask;
    }

    #[inline]
    fn right_most_empty_slot(&self) -> u32 {
      return Bitutil::msb(self.bit_field ^ (self.bit_field + 1));
    }
  };
}

#[repr(C)]
pub struct MaskedBitset<'a, BitType: BitNum> {
  bit_field: BitType,
  shift: usize,
  masked_bits: BitType,
  base: &'a BitType,
}

#[repr(C)]
pub struct MaskedBitsetMut<'a, BitType: BitNum> {
  bit_field: BitType,
  shift: usize,
  masked_bits: BitType,
  base: &'a mut BitType,
}

impl<'a, BitType: BitNum> MaskedBitset<'a, BitType> {
  #[inline]
  pub fn new(shift: usize, masked_bits: BitType, bit_field: BitType, base: &'a BitType) -> MaskedBitset<BitType> {
    return MaskedBitset {
      shift,
      masked_bits,
      bit_field,
      base,
    };
  }
}

impl<'a, BitType: BitNum> MaskedBitsetMut<'a, BitType> {
  #[inline]
  pub fn new(
    shift: usize,
    masked_bits: BitType,
    bit_field: BitType,
    base: &'a mut BitType,
  ) -> MaskedBitsetMut<BitType> {
    return MaskedBitsetMut {
      shift,
      masked_bits,
      bit_field,
      base,
    };
  }
}

impl<'a, BitType: BitNum> Into<Bitset<BitType>> for MaskedBitset<'a, BitType> {
  #[inline]
  fn into(self) -> Bitset<BitType> {
    return Bitset {
      bit_field: (self.bit_field << self.shift) | self.masked_bits,
    };
  }
}

impl<BitType: BitNum> Bitset<BitType> {
  #[inline]
  pub fn new() -> Bitset<BitType> {
    Bitset {
      bit_field: BitType::from(0).unwrap(),
    }
  }

  #[inline]
  pub fn mask_lower(&self, lower_index: usize) -> MaskedBitset<BitType> {
    debug_assert!(lower_index > 0, "lower_index must be greater than 1");
    let lower_mask = (BitType::one() << lower_index - 1) - BitType::one();
    let shift = lower_index - 1;
    return MaskedBitset::new(
      shift,
      self.bit_field & lower_mask,
      self.bit_field >> shift,
      &self.bit_field,
    );
  }

  #[inline]
  pub fn mask_lower_mut(&mut self, lower_index: usize) -> MaskedBitsetMut<BitType> {
    debug_assert!(lower_index > 0, "lower_index must be greater than 1");
    let lower_mask = (BitType::one() << lower_index - 1) - BitType::one();
    let shift = lower_index - 1;
    return MaskedBitsetMut::new(
      shift,
      self.bit_field & lower_mask,
      self.bit_field >> shift,
      &mut self.bit_field,
    );
  }

  #[inline]
  pub fn mask_range(&self, lower_index: usize, upper_index: usize) -> MaskedBitset<BitType> {
    debug_assert!(lower_index > 0, "lower_index must be greater than 1");
    debug_assert!(lower_index < upper_index, "Range must be lower_index < upper_index");
    debug_assert!(upper_index - lower_index > 1, "Range size must be at least 1");
    let lower_mask = if lower_index > 0 {
      (BitType::one() << lower_index - 1) - BitType::one()
    } else {
      BitType::zero()
    };
    let upper_mask = if upper_index > 0 {
      !((BitType::one() << upper_index - 1) - BitType::one())
    } else {
      BitType::zero()
    };
    let shift = lower_index - 1;
    return MaskedBitset::new(
      shift,
      self.bit_field & (lower_mask | upper_mask),
      (self.bit_field & !(upper_mask)) >> shift,
      &self.bit_field,
    );
  }

  #[inline]
  pub fn mask_range_mut(&mut self, lower_index: usize, upper_index: usize) -> MaskedBitsetMut<BitType> {
    debug_assert!(lower_index > 0, "lower_index must be greater than 1");
    debug_assert!(lower_index < upper_index, "Range must be lower_index < upper_index");
    debug_assert!(upper_index - lower_index > 1, "Range size must be at least 1");
    let lower_mask = if lower_index > 0 {
      (BitType::one() << lower_index - 1) - BitType::one()
    } else {
      BitType::zero()
    };
    let upper_mask = if upper_index > 0 {
      !((BitType::one() << upper_index - 1) - BitType::one())
    } else {
      BitType::zero()
    };
    let shift = lower_index - 1;
    return MaskedBitsetMut::new(
      shift,
      self.bit_field & (lower_mask | upper_mask),
      (self.bit_field & !(upper_mask)) >> shift,
      &mut self.bit_field,
    );
  }
}

macro_rules! impl_from_for_usize {
  ($name:tt) => {
    impl From<$name> for Bitset<$name> {
      fn from(a: $name) -> Bitset<$name> {
        let mut bs = Bitset::<$name>::new();
        bs.bit_field = a;
        return bs;
      }
    }
  };
}

impl_from_for_usize!(u8);
impl_from_for_usize!(u16);
impl_from_for_usize!(u32);
impl_from_for_usize!(u64);

impl_bit_operator_for_usize!(u8, Bitset);
impl_bit_operator_for_usize!(u16, Bitset);
impl_bit_operator_for_usize!(u32, Bitset);
impl_bit_operator_for_usize!(u64, Bitset);

impl_bit_operator_for_usize!(mask; u8, MaskedBitset);
impl_bit_operator_for_usize!(mask; u16, MaskedBitset);
impl_bit_operator_for_usize!(mask; u32, MaskedBitset);
impl_bit_operator_for_usize!(mask; u64, MaskedBitset);

impl_bit_operator_for_usize!(mask_mut; u8, MaskedBitsetMut);
impl_bit_operator_for_usize!(mask_mut; u16, MaskedBitsetMut);
impl_bit_operator_for_usize!(mask_mut; u32, MaskedBitsetMut);
impl_bit_operator_for_usize!(mask_mut; u64, MaskedBitsetMut);

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn bitset_set_test() {
    let mut bs = Bitset::<u32>::new();
    for i in 1..32 {
      if i % 2 != 0 {
        bs.set(i);
      }
    }
    assert_eq!(bs.bits(), 1431655765);
  }

  #[test]
  fn bitset_unset_test() {
    let mut bs = Bitset::<u32>::new();
    for i in 1..32 {
      if i % 2 != 0 {
        bs.set(i);
      }
    }
    assert_eq!(bs.bits(), 1431655765);
    let state = [
      1431655764, 0, 1431655760, 0, 1431655744, 0, 1431655680, 0, 1431655424, 0, 1431654400, 0, 1431650304, 0,
      1431633920, 0, 1431568384, 0, 1431306240, 0, 1430257664, 0, 1426063360, 0, 1409286144, 0, 1342177280, 0,
      1073741824, 0, 0, 0,
    ];
    for i in 1..32 {
      if i % 2 != 0 {
        bs.unset(i);
        assert_eq!(bs.bits(), state[i - 1]);
      }
    }
    assert_eq!(bs.bits(), 0);
  }

  #[test]
  fn bitset_assign_test() {
    let mut bs = Bitset::<u32>::new();
    bs.assign(120);
    assert_eq!(bs.bits(), 120);
  }

  #[test]
  fn bitset_assign_and_test() {
    let mut bs = Bitset::<u32>::new();
    bs.assign(120);
    bs.assign_and(119);
    assert_eq!(bs.bits(), 112);
  }

  #[test]
  fn bitset_assign_or_test() {
    let mut bs = Bitset::<u32>::new();
    bs.assign(120);
    bs.assign_or(119);
    assert_eq!(bs.bits(), 127);
  }

  #[test]
  fn bitset_get_test() {
    let mut bs = Bitset::<u32>::new();
    for i in 1..32 {
      if i % 2 != 0 {
        bs.set(i);
      }
    }
    for i in 1..32 {
      if i % 2 != 0 {
        assert_eq!(bs.get(i), true);
      } else {
        assert_eq!(bs.get(i), false);
      }
    }
  }

  #[test]
  fn bitset_is_full_test() {
    let mut bs = Bitset::<u8>::new();
    bs.set(1);
    assert_eq!(bs.is_full(), false);
    bs.assign(0xFF);
    assert_eq!(bs.is_full(), true);
  }

  #[test]
  fn bitset_mask_test() {
    let mut bs = Bitset::<u32>::new();
    bs.assign(120);
    assert_eq!(bs.mask(119), 112);
  }

  #[test]
  fn bitset_mask_range_test() {
    let mut bs = Bitset::<u16>::new();
    bs.assign(65535);
    let range = bs.mask_range(5, 13);
    assert_eq!(range.bits(), 255);
  }

  #[test]
  fn bitset_mask_range_get_test() {
    let mut bs = Bitset::<u16>::new();
    bs.assign(65535);
    let range = bs.mask_range(5, 13);
    for i in 1..8 {
      assert_eq!(range.get(i), true);
    }
  }

  #[test]
  fn bitset_mask_range_unset_test() {
    let mut bs = Bitset::<u16>::new();
    bs.assign(65535);
    let mut range = bs.mask_range(5, 13);
    range.unset(1);
    range.unset(2);
    range.unset(3);
    assert_eq!(range.bits(), 248);
  }

  #[test]
  fn masked_bitset_into_test() {
    let mut bs = Bitset::<u16>::new();
    bs.assign(65535);
    let mut range = bs.mask_range(5, 13);
    range.unset(1);
    range.unset(2);
    range.unset(3);
    bs = range.into();
    assert_eq!(bs.bits(), 65423);
  }

  #[test]
  fn bitset_mask_lower_test() {
    let mut bs = Bitset::<u16>::new();
    bs.assign(65535);
    let mut range = bs.mask_lower(5);
    range.unset(1);
    range.unset(2);
    range.unset(3);
    assert_eq!(range.bits(), 4088);
  }

  #[test]
  fn lower_masked_bitset_into_test() {
    let mut bs = Bitset::<u16>::new();
    bs.assign(65535);
    let mut range = bs.mask_lower(5);
    range.unset(1);
    range.unset(2);
    range.unset(3);
    bs = range.into();
    assert_eq!(bs.bits(), 65423);
    range = bs.mask_lower(5);
    assert_eq!(range.bits(), 4088);
  }
}
