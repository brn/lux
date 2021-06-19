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
  return (u >= 0x41 && u <= 0x46) || (u >= 0x61 && u <= 0x66) || (u >= 0x30 && u <= 0x39);
}

#[inline(always)]
pub fn to_int(u: u16) -> Result<u32, ()> {
  if u >= 0x30 && u <= 0x39 {
    return Ok((u - 0x30 as u16) as u32);
  }
  return Err(());
}

#[inline(always)]
pub fn to_int_from_octal(u: u16) -> Result<u32, ()> {
  if u >= '0' as u16 && u <= '7' as u16 {
    return to_int(u);
  }
  return Err(());
}

#[inline(always)]
pub fn to_int_from_bin(u: u16) -> Result<u32, ()> {
  if u >= '0' as u16 && u <= '1' as u16 {
    return to_int(u);
  }
  return Err(());
}

#[inline(always)]
pub fn to_hex(uchar: u16) -> Result<u32, ()> {
  let mut ret: u32 = 0;
  if uchar >= '0' as u16 && uchar <= '9' as u16 {
    ret = (uchar - '0' as u16) as u32;
  } else if uchar >= 'a' as u16 && uchar <= 'f' as u16 {
    ret = (uchar - 'a' as u16) as u32 + 10;
  } else if uchar >= 'A' as u16 && uchar <= 'F' as u16 {
    ret = ((uchar - 'A' as u16) + 10_u16) as u32;
  } else {
    return Err(());
  }
  return Ok(ret);
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
pub fn is_surrogate_pair(u: u16) -> bool {
  return is_high_surrogate(u) || is_low_surrogate(u);
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
pub fn uc32_to_uc16(u: u32) -> Result<(u16, u16), ()> {
  if u > 0x10FFFF {
    return Err(());
  }
  if u < 0x1000 {
    return Ok((u as u16, 0));
  }
  return Ok((
    ((u - 0x10000) / 0x400 + 0xD800) as u16,
    ((u - 0x10000) % 0x400 + 0xDC00) as u16,
  ));
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
  // [:ID_Start:] + $ + _ + \\ + <ZWNJ> + <ZWJ>
  if value == 36 || value == 95 || value == 92 || value == 0x200C || value == 0x200D {
    return true;
  }
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

pub fn to_utf8(chars: &std::vec::Vec<u16>) -> String {
  return std::char::decode_utf16(chars.iter().cloned())
    .map(|r| r.unwrap_or('#'))
    .collect::<String>();
}

pub fn parse_hex<'a>(mut iter: impl Iterator<Item = u16>) -> u64 {
  const DIGITS: u64 = 16;
  let mut value = 0_u64;
  for u in iter {
    if let Ok(hex) = to_hex(u) {
      value *= DIGITS;
      value += hex as u64;
    } else {
      return value;
    }
  }
  return value;
}

pub fn parse_binary<'a>(iter: &'a mut impl Iterator<Item = u16>) -> u64 {
  const DIGITS: u64 = 2;
  let mut value: u64 = 0;
  for u in iter {
    if let Ok(bin) = to_int_from_bin(u) {
      value *= DIGITS;
      value += bin as u64;
    } else {
      return value;
    }
  }
  return value;
}

pub fn parse_octal<'a>(iter: &'a mut impl Iterator<Item = u16>) -> u64 {
  const DIGITS: u64 = 8;
  let mut value: u64 = 0;
  for u in iter {
    if let Ok(octal) = to_int_from_octal(u) {
      value *= DIGITS;
      value += octal as u64;
    } else {
      return value;
    }
  }
  return value;
}

const PARTIAL_MOD_LIMIT: u64 = 10000000000000000;
pub fn parse_decimal<'a>(iter: &'a mut std::iter::Peekable<impl Iterator<Item = u16>>) -> f64 {
  const DIGITS: f64 = 10.0;

  while let Some(peek) = iter.peek().cloned() {
    if ch(peek) == '0' {
      iter.next();
    } else {
      break;
    }
  }
  let mut is_floating_point = false;
  let mut floating_digits = 1;
  let mut value = 0.0_f64;
  let mut exponents = 0.0_f64;
  let mut is_negative_exponents = false;
  let mut start_exponents = false;
  for u in iter.by_ref() {
    if start_exponents {
      if ch(u) == '-' {
        is_negative_exponents = true;
      } else if ch(u) == '+' {
        continue;
      }
      exponents *= DIGITS;
      exponents += to_int(u).unwrap() as f64;
    } else if ch(u) == '.' {
      is_floating_point = true;
    } else if is_decimal_digits(u) {
      if ch(u) == 'e' || ch(u) == 'E' {
        start_exponents = true;
        continue;
      }
      if !is_floating_point {
        value = value.mul_add(DIGITS, to_int(u).unwrap() as f64);
      } else {
        value = value.mul_add(DIGITS, to_int(u).unwrap() as f64);
        if floating_digits >= PARTIAL_MOD_LIMIT {
          value /= floating_digits as f64;
          floating_digits = 1;
        } else {
          floating_digits *= 10;
        }
      }
    } else {
      break;
    }
  }
  if start_exponents {
    value *= if is_negative_exponents {
      1.0 / (u64::pow(10, exponents as u32) as f64)
    } else {
      u64::pow(10, exponents as u32) as f64
    };
  }
  return value / (floating_digits as f64);
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum NumericValueKind {
  Decimal = 1,
  DecimalLeadingZero,
  ImplicitOctal,
  Octal,
  Hex,
  Binary,
}

pub fn parse_numeric_value<'a>(
  iter: &'a mut std::iter::Peekable<impl Iterator<Item = u16>>,
  is_double: bool,
) -> Result<(f64, NumericValueKind), &'static str> {
  use NumericValueKind::*;
  let mut kind = Decimal;
  let mut is_period_seen = is_double;
  let mut is_leading_zeros = false;
  let mut buffer = Vec::<u16>::new();

  if let Some(uc) = iter.peek() {
    if ch(*uc) == '.' {
      is_period_seen = true;
      iter.next();
      buffer.push('.' as u16);
    }
  }

  if is_period_seen {
    if let Some(next) = iter.peek() {
      if !is_decimal_digits(*next) {
        return Err("Number expected");
      }
    }
  }

  if let Some(next) = iter.peek().cloned() {
    if ch(next) == '0' {
      is_leading_zeros = true;
      iter.next();
      kind = DecimalLeadingZero;
      if let Some(next) = iter.peek() {
        if ch(*next) == 'x' {
          iter.next();
          return Ok((parse_hex(iter) as f64, NumericValueKind::Hex));
        } else if ch(*next) == 'b' {
          iter.next();
          return Ok((parse_binary(iter) as f64, NumericValueKind::Binary));
        } else if ch(*next) == 'o' {
          iter.next();
          return Ok((parse_octal(iter) as f64, NumericValueKind::Octal));
        } else if ch(*next) == '.' {
          iter.next();
          is_leading_zeros = false;
          is_period_seen = true;
          kind = Decimal;
          buffer.push('.' as u16);
        }
      }
    }

    if let Some(next) = iter.peek().cloned() {
      if is_leading_zeros && ch(next) >= '0' && ch(next) <= '7' {
        kind = ImplicitOctal;
        while let Some(u) = iter.peek().cloned() {
          if !is_octal_digits(u) {
            break;
          }
          buffer.push(u);
          iter.next();
        }
      } else if ch(next) == '8' || ch(next) == '9' {
        kind = if is_leading_zeros { DecimalLeadingZero } else { Decimal };
        for u in iter.by_ref() {
          if !is_decimal_digits(u) {
            break;
          }
          buffer.push(u);
        }
      }

      if kind == ImplicitOctal {
        if let Some(next) = iter.peek().cloned() {
          if is_decimal_digits(next) && ch(next) > '7' {
            for u in iter.by_ref() {
              if !is_decimal_digits(u) {
                break;
              }
              buffer.push(u);
            }
            kind = DecimalLeadingZero;
          }
        }
      }

      if !is_period_seen && ch(next) == '.' && !is_period_seen {
        if kind == Decimal {
          is_period_seen = true;
          iter.next();
          buffer.push('.' as u16);
        }
      }

      for u in iter.by_ref() {
        if !is_period_seen && ch(u) == '.' && kind == Decimal {
          is_period_seen = true;
          buffer.push('.' as u16);
          continue;
        }
        if !is_decimal_digits(u) {
          break;
        }
        buffer.push(u);
      }

      if let Some(next) = iter.peek() {
        if ch(*next) == 'e' || ch(*next) == 'E' {
          if kind != Decimal && kind != DecimalLeadingZero {
            return Err("Unexpected token.");
          }
          buffer.push(*next);
          if let Some(next) = iter.peek() {
            if ch(*next) == '+' || ch(*next) == '-' {
              buffer.push(*next);
              iter.next();
            }
            if let Some(next) = iter.peek() {
              if !is_decimal_digits(*next) {
                return Err("Expected exponent digit.");
              }
              buffer.push(*next);
              iter.next();
            } else {
              return Err("Expected exponent digit.");
            }
            for u in iter.by_ref() {
              if !is_decimal_digits(u) {
                break;
              }
              buffer.push(u);
            }
          }
        }
      }

      let mut buffer_iter = buffer.into_iter();
      return match kind {
        Decimal | DecimalLeadingZero => Ok((parse_decimal(buffer_iter.peekable().by_ref()) as f64, kind)),
        _ => Ok((parse_octal(buffer_iter.by_ref()) as f64, kind)),
      };
    }
  }

  return Err("Unexpected end of input");
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

  #[test]
  fn parse_numeric_value_int_test() {
    let buf = "120304374".encode_utf16().collect::<Vec<_>>();
    let result = parse_numeric_value(buf.into_iter().peekable().by_ref(), false);
    assert!(result.is_ok());
    let (value, kind) = result.unwrap();
    assert_eq!(value, 120304374.0);
    assert_eq!(kind, NumericValueKind::Decimal);
  }

  #[test]
  fn parse_numeric_value_hex_test() {
    let buf = "0xFFFF".encode_utf16().collect::<Vec<_>>();
    let result = parse_numeric_value(buf.into_iter().peekable().by_ref(), false);
    assert!(result.is_ok());
    let (value, kind) = result.unwrap();
    assert_eq!(value, 0xFFFF as f64);
    assert_eq!(kind, NumericValueKind::Hex);
  }

  #[test]
  fn parse_numeric_value_binary_test() {
    let buf = "0b010101".encode_utf16().collect::<Vec<_>>();
    let result = parse_numeric_value(buf.into_iter().peekable().by_ref(), false);
    assert!(result.is_ok());
    let (value, kind) = result.unwrap();
    assert_eq!(value, 0b010101 as f64);
    assert_eq!(kind, NumericValueKind::Binary);
  }

  #[test]
  fn parse_numeric_value_octal_test() {
    let buf = "0o7766".encode_utf16().collect::<Vec<_>>();
    let result = parse_numeric_value(buf.into_iter().peekable().by_ref(), false);
    assert!(result.is_ok());
    let (value, kind) = result.unwrap();
    assert_eq!(value, 0o7766 as f64);
    assert_eq!(kind, NumericValueKind::Octal);
  }

  #[test]
  fn parse_numeric_value_implicit_octal_test() {
    let buf = "07766".encode_utf16().collect::<Vec<_>>();
    let result = parse_numeric_value(buf.into_iter().peekable().by_ref(), false);
    assert!(result.is_ok());
    let (value, kind) = result.unwrap();
    assert_eq!(value, 0o7766 as f64);
    assert_eq!(kind, NumericValueKind::ImplicitOctal);
  }

  #[test]
  fn parse_numeric_value_int_leading_zeros_octal_test() {
    let buf = "07769837".encode_utf16().collect::<Vec<_>>();
    let result = parse_numeric_value(buf.into_iter().peekable().by_ref(), false);
    assert!(result.is_ok());
    let (value, kind) = result.unwrap();
    assert_eq!(value, 7769837 as f64);
    assert_eq!(kind, NumericValueKind::DecimalLeadingZero);
  }

  #[test]
  fn parse_numeric_value_floating_point_test() {
    let buf = "0.7769837".encode_utf16().collect::<Vec<_>>();
    let result = parse_numeric_value(buf.into_iter().peekable().by_ref(), false);
    assert!(result.is_ok());
    let (value, kind) = result.unwrap();
    assert_eq!(value, 0.7769837);
    assert_eq!(kind, NumericValueKind::Decimal);
  }

  #[test]
  fn parse_numeric_value_floating_point_not_start_zeros_test() {
    let buf = ".7769837".encode_utf16().collect::<Vec<_>>();
    let result = parse_numeric_value(buf.into_iter().peekable().by_ref(), false);
    assert!(result.is_ok());
    let (value, kind) = result.unwrap();
    assert_eq!(value, 0.7769837);
    assert_eq!(kind, NumericValueKind::Decimal);
  }

  #[test]
  fn parse_numeric_value_floating_point_intermeditate_test() {
    let buf = "776.9837".encode_utf16().collect::<Vec<_>>();
    let result = parse_numeric_value(buf.into_iter().peekable().by_ref(), false);
    assert!(result.is_ok());
    let (value, kind) = result.unwrap();
    assert_eq!(value, 776.9837);
    assert_eq!(kind, NumericValueKind::Decimal);
  }
}
