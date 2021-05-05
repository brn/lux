use super::bitutil::*;
use num::Unsigned;
use num_traits::PrimInt;
use std::ops::*;

struct Bitset<BitType: PrimInt + BitOrAssign + BitAndAssign + Unsigned> {
  bit_field: BitType,
}

fn flag_right_most_empty_slot_index<BitType: PrimInt + BitOrAssign + BitAndAssign + Unsigned>(
  bit_field: BitType,
) -> BitType {
  return bit_field ^ (bit_field + BitType::from(1).unwrap());
}

impl<BitType: PrimInt + BitOrAssign + BitAndAssign + Unsigned> Bitset<BitType> {
  pub fn new() -> Bitset<BitType> {
    Bitset {
      bit_field: BitType::from(0).unwrap(),
    }
  }

  pub fn apply_lower(&mut self, value: BitType, start: usize) {
    let lower_bits = self.bit_field & BitType::from((1 << start) - 1).unwrap();
    self.bit_field = value << start;
    self.bit_field |= lower_bits;
  }

  pub fn apply_range(&mut self, value: BitType, upper: usize, lower: usize) {
    let lower_bits = self.bit_field & BitType::from((1 << lower) - 1).unwrap();
    let upper_bits = self.bit_field & BitType::from(!((1 << upper) - 1)).unwrap();
    self.bit_field = value << lower;
    self.bit_field |= lower_bits | upper_bits;
  }

  pub fn bits(&self) -> BitType {
    return self.bit_field;
  }

  pub fn set(&mut self, index: usize) {
    self.bit_field |= BitType::from(1).unwrap() << index;
  }

  pub fn set_raw(&mut self, value: BitType) {
    self.bit_field |= value;
  }

  pub fn unset(&mut self, index: usize) {
    self.bit_field &= !(BitType::from(1).unwrap() << index);
  }

  pub fn unset_raw(&mut self, value: BitType) {
    self.bit_field &= !value;
  }

  pub fn assign(&mut self, bit_value: BitType) {
    self.bit_field = bit_value;
  }

  pub fn get(&self, index: usize) -> bool {
    let i = BitType::from(0x1 << index).unwrap();
    return self.bit_field & i == i;
  }

  pub fn get_raw(&self, value: usize) -> bool {
    let i = BitType::from(value).unwrap();
    return (self.bit_field & i) == i;
  }

  pub fn is_full(&self) -> bool {
    return self.bit_field == BitType::from(!0).unwrap();
  }

  pub fn mask<T: PrimInt>(&self, mask: T) -> BitType {
    return self.bit_field & BitType::from(mask).unwrap();
  }
}

impl Bitset<u8> {
  pub fn right_most_empty_slot(&self) -> u32 {
    return Bitutil::msb(flag_right_most_empty_slot_index(self.bit_field));
  }
}

impl Bitset<u16> {
  pub fn right_most_empty_slot(&self) -> u32 {
    return Bitutil::msb(flag_right_most_empty_slot_index(self.bit_field));
  }
}

impl Bitset<u32> {
  pub fn right_most_empty_slot(&self) -> u32 {
    return Bitutil::msb(flag_right_most_empty_slot_index(self.bit_field));
  }
}

impl Bitset<u64> {
  pub fn right_most_empty_slot(&self) -> u32 {
    return Bitutil::msb(flag_right_most_empty_slot_index(self.bit_field));
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn bitset_set_test() {
    let mut bs = Bitset::<u32>::new();
    for i in 0..31 {
      if i % 2 != 0 {
        bs.set(i);
      }
    }
    assert_eq!(bs.bits(), 715827882);
  }

  #[test]
  fn bitset_unset_test() {
    let mut bs = Bitset::<u32>::new();
    for i in 0..31 {
      if i % 2 != 0 {
        bs.set(i);
      }
    }
    let state = [
      0, 715827880, 0, 715827872, 0, 715827840, 0, 715827712, 0, 715827200, 0, 715825152, 0, 715816960, 0, 715784192,
      0, 715653120, 0, 715128832, 0, 713031680, 0, 704643072, 0, 671088640, 0, 536870912, 0, 0, 0, 0,
    ];
    for i in 0..31 {
      if i % 2 != 0 {
        bs.unset(i);
        assert_eq!(bs.bits(), state[i]);
      }
    }
    assert_eq!(bs.bits(), 0);
  }
}
