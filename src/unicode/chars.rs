use super::data::{ID_CONTINUE_DIFF, ID_CONTINUE_HASH_MOD, ID_START, ID_START_HASH_MOD};

pub const LF: u16 = 0x00A;
pub const CR: u16 = 0x00D;
pub const LF_CHAR: char = ch(LF);
pub const CR_CHAR: char = ch(CR);
pub const LT: u16 = 0x2028;
pub const PS: u16 = 0x2029;
pub const LT_CHAR: char = ch(LT);
pub const PS_CHAR: char = ch(PS);

#[inline(always)]
pub fn is_decimal_digits(u: u16) -> bool {
  return u >= 0x30 && u <= 0x39;
}

#[inline(always)]
pub fn is_octal_digits(u: u16) -> bool {
  return u >= 0x30 && u <= 0x37;
}

#[inline(always)]
pub fn is_binary_digits(u: u16) -> bool {
  return u >= 0x30 && u <= 0x31;
}

#[inline(always)]
pub fn is_hex_digits(u: u16) -> bool {
  return (u >= 0x41 && u <= 0x46) || (u >= 0x61 && u <= 0x66) || (u >= 0x30 && u <= 0x31);
}

#[inline(always)]
pub fn to_int(u: u16) -> u32 {
  assert!(u >= 0x30 && u <= 0x39);
  return (u - 0x30 as u16) as u32;
}

#[inline(always)]
pub fn to_hex(uchar: u16) -> i32 {
  let mut ret: i32 = 0;
  if uchar >= '0' as u16 && uchar <= '9' as u16 {
    ret = (uchar - '0' as u16) as i32;
  } else if uchar >= 'a' as u16 && uchar <= 'f' as u16 {
    ret = (uchar - 'a' as u16) as i32 + 10;
  } else if uchar >= 'A' as u16 && uchar <= 'F' as u16 {
    ret = ((uchar - 'A' as u16) + 10_u16) as i32;
  } else {
    return -1;
  }
  return ret;
}

#[inline(always)]
pub fn is_cr(u: u16) -> bool {
  return u == CR;
}

#[inline(always)]
pub fn is_lf(u: u16) -> bool {
  return u == LF;
}

#[inline(always)]
pub fn is_cr_or_lf(u: u16) -> bool {
  return is_cr(u) || is_lf(u);
}

#[inline(always)]
pub fn is_high_surrogate(u: u16) -> bool {
  return u >= 0xD800 && u < 0xDC00;
}

#[inline(always)]
pub fn is_low_surrogate(u: u16) -> bool {
  return u >= 0xDC00 && u < 0xE000;
}

#[inline(always)]
pub fn join_surrogate_pair(high: u16, low: u16) -> u32 {
  return 0x10000_u32 + (high as u32 - 0xD800_u32) * 0x400_u32 + (low as u32 - 0xDC00_u32);
}

#[inline(always)]
pub fn is_start_unicode_escape_sequence(u: u16) -> bool {
  return ch(u) == 'u';
}

#[inline(always)]
pub fn is_start_ascii_escape_sequence(u: u16) -> bool {
  return ch(u) == 'x';
}

#[inline(always)]
pub fn is_start_escape_sequence(u: u16) -> bool {
  return is_start_ascii_escape_sequence(u) || is_start_unicode_escape_sequence(u);
}

// http://www.ecma-international.org/ecma-262/9.0/index.html#sec-runtime-semantics-wordcharacters-abstract-operation
pub fn is_word_char(u: u16) -> bool {
  let c = ch(u);
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_';
}

// http://www.ecma-international.org/ecma-262/9.0/index.html#sec-white-space
#[inline(always)]
pub fn is_whitespace(c: u16) -> bool {
  return c == 0x0009
    || c == 0x000B
    || c == 0x000C
    || c == 0x00A0
    || c == 0xFEFF
    || c == 0x0020
    || c == 0x3000
    || c == 0x1680
    || (c >= 0x2000 && c <= 0x2006)
    || (c >= 0x2008 && c <= 0x200A)
    || c == 0x205F
    || c == 0x00A0
    || c == 0x2007
    || c == 0x202F;
}

#[inline(always)]
pub const fn ch(u: u16) -> char {
  return u as u8 as char;
}

// http://www.ecma-international.org/ecma-262/9.0/index.html#sec-names-and-keywords
pub fn is_identifier_start(value: u32) -> bool {
  let index = (value % ID_START_HASH_MOD) as usize;
  for ch in ID_START[index].iter() {
    if *ch == value {
      return true;
    }
  }
  return false;
}

// http://www.ecma-international.org/ecma-262/9.0/index.html#sec-names-and-keywords
pub fn is_identifier_continue(value: u32, is_unicode_escape_seq: bool) -> bool {
  if is_unicode_escape_seq {
    return is_hex_digits(value as u16);
  }

  if is_identifier_start(value) {
    return true;
  }

  let index = (value % ID_CONTINUE_HASH_MOD) as usize;

  for ch in ID_CONTINUE_DIFF[index].iter() {
    if *ch == value {
      return true;
    }
  }
  return false;
}

#[cfg(test)]
mod chars_test {
  use super::*;
  use std::fs;

  #[test]
  fn is_identifier_start_test() {
    let v = fs::read_to_string(format!("{}/test/data/unicode/id_start.txt", env!("CARGO_MANIFEST_DIR"))).unwrap();
    let u16_list = v.encode_utf16().collect::<Vec<_>>();
    let mut iter = u16_list.iter();
    let len = u16_list.len();
    let mut count = 0;
    loop {
      if let Some(c) = iter.next() {
        if is_high_surrogate(*c) {
          if let Some(low) = iter.next() {
            count += 1;
            let uc = join_surrogate_pair(*c, *low);
            assert!(is_identifier_start(uc));
          } else {
            unreachable!();
          }
        } else {
          assert!(is_identifier_start((*c) as u32));
        }
        count += 1;
      } else {
        break;
      }
    }
    assert_eq!(len, count);
  }

  #[test]
  fn is_identifier_continue_test() {
    let v = fs::read_to_string(format!(
      "{}/test/data/unicode/id_continue.txt",
      env!("CARGO_MANIFEST_DIR")
    ))
    .unwrap();
    let u16_list = v.encode_utf16().collect::<Vec<_>>();
    let mut iter = u16_list.iter();
    let len = u16_list.len();
    let mut count = 0;
    loop {
      if let Some(c) = iter.next() {
        if is_high_surrogate(*c) {
          if let Some(low) = iter.next() {
            count += 1;
            let uc = join_surrogate_pair(*c, *low);
            assert!(is_identifier_continue(uc, false));
          } else {
            unreachable!();
          }
        } else {
          assert!(is_identifier_continue((*c) as u32, false));
        }
        count += 1;
      } else {
        break;
      }
    }
    assert_eq!(len, count);
  }
}
