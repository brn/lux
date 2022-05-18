use num::Unsigned;
use num_traits::PrimInt;
use std::ops::*;

pub trait BitOp<BitType: PrimInt + BitOrAssign + BitAndAssign + Unsigned> {
  fn count(v: BitType) -> u32;
  fn msb(v: BitType) -> u32;
}

pub struct Bitutil;

impl BitOp<u8> for Bitutil {
  fn count(v: u8) -> u32 {
    let mut count: u32 = ((v & 0x55) + ((v >> 1) & 0x55)).into();
    count = (count & 0x33) + ((count >> 2) & 0x33);
    return (count & 0x0f) + ((count >> 4) & 0x0f);
  }

  fn msb(v: u8) -> u32 {
    if v == 0 {
      return 0;
    }
    let mut ret = v;
    ret |= ret >> 1;
    ret |= ret >> 2;
    ret |= ret >> 4;
    return Bitutil::count(ret) - 1;
  }
}

impl BitOp<u16> for Bitutil {
  fn count(v: u16) -> u32 {
    let mut count: u32 = ((v & 0x5555) + ((v >> 1) & 0x5555)).into();
    count = (count & 0x3333) + ((count >> 2) & 0x3333);
    count = (count & 0x0f0f) + ((count >> 4) & 0x0f0f);
    return (count & 0x00ff) + ((count >> 8) & 0x00ff);
  }

  fn msb(v: u16) -> u32 {
    if v == 0 {
      return 0;
    }
    let mut ret = v;
    ret |= ret >> 1;
    ret |= ret >> 2;
    ret |= ret >> 4;
    ret |= ret >> 8;
    return Bitutil::count(ret) - 1;
  }
}

impl BitOp<u32> for Bitutil {
  fn count(v: u32) -> u32 {
    let mut count: u32 = (v & 0x55555555) + ((v >> 1) & 0x55555555);
    count = (count & 0x33333333) + ((count >> 2) & 0x33333333);
    count = (count & 0x0f0f0f0f) + ((count >> 4) & 0x0f0f0f0f);
    count = (count & 0x00ff00ff) + ((count >> 8) & 0x00ff00ff);
    return (count & 0x0000ffff) + ((count >> 16) & 0x0000ffff);
  }

  fn msb(v: u32) -> u32 {
    if v == 0 {
      return 0;
    }
    let mut ret = v;
    ret |= ret >> 1;
    ret |= ret >> 2;
    ret |= ret >> 4;
    ret |= ret >> 8;
    ret |= ret >> 1;
    return Bitutil::count(ret) - 1;
  }
}

impl BitOp<u64> for Bitutil {
  fn count(v: u64) -> u32 {
    let mut count: u64 = (v & 0x5555555555555555) + ((v >> 1) & 0x5555555555555555);
    count = (count & 0x3333333333333333) + ((count >> 2) & 0x3333333333333333);
    count = (count & 0x0f0f0f0f0f0f0f0f) + ((count >> 4) & 0x0f0f0f0f0f0f0f0f);
    count = (count & 0x00ff00ff00ff00ff) + ((count >> 8) & 0x00ff00ff00ff00ff);
    count = (count & 0x0000ffff0000ffff) + ((count >> 16) & 0x0000ffff0000ffff);
    return ((count & 0x00000000ffffffff) + ((count >> 32) & 0x00000000ffffffff)) as u32;
  }

  fn msb(v: u64) -> u32 {
    if v == 0 {
      return 0;
    }
    let mut ret = v;
    ret |= ret >> 1;
    ret |= ret >> 2;
    ret |= ret >> 4;
    ret |= ret >> 8;
    ret |= ret >> 16;
    ret |= ret >> 32;
    return Bitutil::count(ret) - 1;
  }
}
