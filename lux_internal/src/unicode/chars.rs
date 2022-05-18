use super::ucd::*;
use super::ucd_type::*;
use std::iter::{Iterator, Peekable};

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
pub fn is_non_octal_digits(u: u16) -> bool {
  return u >= 0x38 && u <= 0x39;
}

#[inline(always)]
pub fn octal_hex_followable_count(u: u16) -> u8 {
  return if u >= 0x30 && u <= 0x33 { 2 } else { 1 };
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
pub fn is_line_terminator(u: u16) -> bool {
  return is_cr(u) || is_lf(u) || is_paragraph_separator(u);
}

#[inline(always)]
pub fn is_paragraph_separator(u: u16) -> bool {
  return u == 0x2028 || u == 0x2029;
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
  if u < 0x10000 {
    return Ok((u as u16, 0));
  }
  return Ok((((u - 0x10000) / 0x400 + 0xD800) as u16, ((u - 0x10000) % 0x400 + 0xDC00) as u16));
}

#[inline(always)]
pub fn is_start_escape_sequence(u: u16) -> bool {
  return is_start_ascii_escape_sequence(u) || is_start_unicode_escape_sequence(u);
}

pub fn is_alpha(u: u16) -> bool {
  let c = ch(u);
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
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

#[inline(always)]
pub const fn ch_code(u: char) -> u16 {
  return u as u16;
}

#[inline(always)]
pub const fn char_safe(u: u16) -> Result<char, u16> {
  if u > (u8::MAX as u16) {
    return Err(u);
  }
  return Ok(u as u8 as char);
}

// http://www.ecma-international.org/ecma-262/9.0/index.html#sec-names-and-keywords
#[inline]
pub fn is_identifier_start(value: u32, should_includes_reverse_solidus: bool) -> bool {
  // [:ID_Start:] + $ + _ or \\
  if value == 36 || value == 95 || (should_includes_reverse_solidus && value == 92) {
    return true;
  }
  return Ucd::get_id_property(value) == UcdIdProperty::IdStart;
}

// http://www.ecma-international.org/ecma-262/9.0/index.html#sec-names-and-keywords
#[inline]
pub fn is_identifier_continue(value: u32, should_includes_reverse_solidus: bool) -> bool {
  // in https://262.ecma-international.org/12.0/ 12.1 Unicode Format-Control Characaters
  // <ZWNJ> + <ZWJ>
  if value == 0x200C || value == 0x200D {
    return true;
  }

  if is_identifier_start(value, should_includes_reverse_solidus) {
    return true;
  }
  return Ucd::get_id_property(value) == UcdIdProperty::IdContinue;
}

#[inline]
pub fn to_utf8(chars: &std::vec::Vec<u16>) -> String {
  return std::char::decode_utf16(chars.iter().cloned())
    .map(|r| r.unwrap_or('#'))
    .collect::<String>();
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum NumericConvertionError {
  UnexpectedTokenFound,
  NotANumber,
  ExponentsExpectedNumber,
  UnexpectedEndOfInput,
  LeadingZeroesNotAllowed,
}

#[inline]
pub fn parse_hex<'a>(
  start: &'a mut impl Iterator<Item = u16>,
  should_stop_if_invalid_token_found: bool,
) -> Result<u64, NumericConvertionError> {
  const DIGITS: u64 = 16;
  let mut iter = start.by_ref().peekable();
  let mut value = 0_u64;
  let mut digits = 0;
  while let Some(next) = iter.peek().cloned() {
    if let Ok(hex) = to_hex(next) {
      value = value * DIGITS + (hex as u64);
      iter.next();
    } else {
      if digits == 0 && should_stop_if_invalid_token_found {
        return Err(NumericConvertionError::UnexpectedTokenFound);
      }
      break;
    }
    digits += 1;
  }
  if digits == 0 {
    return Err(NumericConvertionError::NotANumber);
  }
  return Ok(value);
}

#[inline]
pub fn parse_binary<'a>(
  start: &'a mut impl Iterator<Item = u16>,
  should_stop_if_invalid_token_found: bool,
) -> Result<u64, NumericConvertionError> {
  const DIGITS: u64 = 2;
  let mut iter = start.by_ref().peekable();
  let mut value: u64 = 0;
  let mut digits = 0;
  while let Some(next) = iter.peek().cloned() {
    if let Ok(bin) = to_int_from_bin(next) {
      value = value * DIGITS + (bin as u64);
      iter.next();
    } else {
      if digits == 0 && should_stop_if_invalid_token_found {
        return Err(NumericConvertionError::UnexpectedTokenFound);
      }
      return Ok(value);
    }
    digits += 1;
  }
  if digits == 0 {
    return Err(NumericConvertionError::NotANumber);
  }
  return Ok(value);
}

#[inline]
pub fn parse_octal<'a>(
  start: &'a mut impl Iterator<Item = u16>,
  should_stop_if_invalid_token_found: bool,
) -> Result<u64, NumericConvertionError> {
  const DIGITS: u64 = 8;
  let mut iter = start.by_ref().peekable();
  let mut value: u64 = 0;
  let mut digits = 0;
  while let Some(next) = iter.peek().cloned() {
    if let Ok(octal) = to_int_from_octal(next) {
      value = value * DIGITS + (octal as u64);
      iter.next();
    } else {
      if digits == 0 && should_stop_if_invalid_token_found {
        return Err(NumericConvertionError::UnexpectedTokenFound);
      }
      break;
    }
    digits += 1;
  }
  if digits == 0 {
    return Err(NumericConvertionError::NotANumber);
  }
  return Ok(value);
}

#[inline]
pub fn parse_uint32_without_exponents<'a>(
  start: &'a mut impl Iterator<Item = u16>,
  should_stop_if_invalid_token_found: bool,
) -> Result<u32, NumericConvertionError> {
  const DIGITS: u32 = 10;
  let mut iter = start.by_ref().peekable();
  let mut value: u32 = 0;
  let mut digits = 0;
  while let Some(next) = iter.peek().cloned() {
    if is_decimal_digits(next) {
      value = value * DIGITS + to_int(next).unwrap();
      iter.next();
    } else {
      if digits == 0 && should_stop_if_invalid_token_found {
        return Err(NumericConvertionError::UnexpectedTokenFound);
      }
      break;
    }
    digits += 1;
  }
  if digits == 0 {
    return Err(NumericConvertionError::NotANumber);
  }
  return Ok(value);
}

const PARTIAL_MOD_LIMIT: u64 = 10000000000000000;
const POW_LIMIT: u32 = 19;

fn parse_exponents<'a>(
  mut value: f64,
  iter: &'a mut std::iter::Peekable<impl Iterator<Item = u16>>,
  should_stop_if_invalid_token_found: bool,
) -> Result<f64, NumericConvertionError> {
  const DIGITS: u32 = 10;
  let mut is_negative = false;
  let mut exponents_value: u32 = 0;
  let mut digits = 0;
  let mut last_ch: char = '0';
  let mut is_overflowed = false;
  while let Some(next) = iter.peek().cloned() {
    last_ch = ch(next);
    if ch(next) == '-' {
      is_negative = true;
      iter.next();
    } else if ch(next) == '+' {
      iter.next();
      continue;
    } else if is_decimal_digits(next) {
      if !is_overflowed {
        exponents_value = exponents_value * DIGITS + (to_int(next).unwrap() as u32);
        // If exponents_value exceeds 309, exponent_value is overflowed.
        is_overflowed = value > 0.0 && exponents_value >= 309;
      }
      iter.next();
    } else {
      if digits == 0 && should_stop_if_invalid_token_found {
        return Err(NumericConvertionError::UnexpectedTokenFound);
      }
      break;
    }
    digits += 1;
  }

  if is_overflowed {
    return Ok(f64::INFINITY);
  }

  if digits == 0 || last_ch == '-' || last_ch == '+' {
    return Err(NumericConvertionError::ExponentsExpectedNumber);
  }

  if exponents_value >= POW_LIMIT {
    while exponents_value > 0 {
      let exp_val = if exponents_value < POW_LIMIT { exponents_value } else { POW_LIMIT };
      exponents_value -= exp_val;
      let p = u64::pow(10, exp_val) as f64;
      if !is_negative {
        value *= p;
      } else {
        value /= p;
      }
    }
    return Ok(value);
  } else {
    let p = u64::pow(10, exponents_value) as f64;
    return Ok(if !is_negative { value * p } else { value / p });
  }
}

pub fn parse_decimal<'a>(
  start: &'a mut impl Iterator<Item = u16>,
  should_stop_if_invalid_token_found: bool,
) -> Result<f64, NumericConvertionError> {
  const DIGITS: f64 = 10.0;
  let mut iter = start.by_ref().peekable();

  let mut is_floating_point = false;
  let mut floating_digits = 1;
  let mut value = 0.0_f64;
  let mut is_start_exponents = false;
  let mut digits = 0;
  while let Some(u) = iter.peek().cloned() {
    if ch(u) == '.' {
      if is_floating_point {
        break;
      }
      is_floating_point = true;
      iter.next();
    } else if ch(u) == 'e' || ch(u) == 'E' {
      is_start_exponents = true;
      iter.next();
      break;
    } else if is_decimal_digits(u) {
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
      iter.next();
    } else {
      if digits == 0 && should_stop_if_invalid_token_found {
        return Err(NumericConvertionError::UnexpectedTokenFound);
      }
      break;
    }
    digits += 1;
  }

  if digits == 0 {
    return Err(NumericConvertionError::NotANumber);
  }

  if is_start_exponents {
    match parse_exponents(value, &mut iter, should_stop_if_invalid_token_found) {
      Ok(val) => {
        value = val;
      }
      Err(e) => {
        return Err(e);
      }
    };
  }
  return Ok(value / (floating_digits as f64));
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
  origin: &'a mut impl Iterator<Item = u16>,
  start: &'a mut impl Iterator<Item = u16>,
  is_double: bool,
  should_stop_if_invalid_token_found: bool,
  is_allow_leading_zeroes: bool,
) -> Result<(f64, NumericValueKind), NumericConvertionError> {
  use NumericValueKind::*;
  let mut kind = Decimal;
  let mut is_period_seen = is_double;
  let mut is_leading_zeros = false;
  let mut has_exponents_part = false;
  let mut is_int32 = false;
  let mut iter = start.peekable();
  let mut digits_len = 0;
  let mut char_count = 0;

  if let Some(uc) = iter.peek() {
    if ch(*uc) == '.' {
      is_period_seen = true;
      iter.next();
      char_count += 1;
    }
  }

  if let Some(next) = iter.peek().cloned() {
    if ch(next) == '0' {
      is_leading_zeros = true;
      iter.next();
      kind = DecimalLeadingZero;
      if let Some(next) = iter.peek() {
        if ch(*next).to_lowercase().last().unwrap() == 'x' {
          origin.next();
          origin.next();
          return match parse_hex(origin, should_stop_if_invalid_token_found) {
            Ok(val) => Ok((val as f64, NumericValueKind::Hex)),
            Err(e) => Err(e),
          };
        } else if ch(*next).to_lowercase().last().unwrap() == 'b' {
          origin.next();
          origin.next();
          return match parse_binary(origin, should_stop_if_invalid_token_found) {
            Ok(val) => Ok((val as f64, NumericValueKind::Binary)),
            Err(e) => Err(e),
          };
        } else if ch(*next).to_lowercase().last().unwrap() == 'o' {
          origin.next();
          origin.next();
          return match parse_octal(origin, should_stop_if_invalid_token_found) {
            Ok(val) => Ok((val as f64, NumericValueKind::Octal)),
            Err(e) => Err(e),
          };
        }
      }

      while let Some(next) = iter.peek().cloned() {
        if ch(next) == '0' {
          digits_len += 1;
          iter.next();
        } else {
          break;
        }
      }

      if let Some(next) = iter.peek() {
        if ch(*next) == '.' {
          iter.next();
          is_leading_zeros = false;
          is_period_seen = true;
          kind = Decimal;
          char_count += 1;
        }
      }
    }

    if let Some(next) = iter.peek().cloned() {
      if is_decimal_digits(next) {
        if is_leading_zeros && is_octal_digits(next) {
          while let Some(u) = iter.peek().cloned() {
            if !is_decimal_digits(u) {
              break;
            } else if !is_octal_digits(u) {
              kind = DecimalLeadingZero;
              break;
            } else {
              kind = ImplicitOctal;
            }
            iter.next();
            char_count += 1;
            digits_len += 1;
          }
        } else if ch(next) == '8' || ch(next) == '9' {
          kind = if is_leading_zeros { DecimalLeadingZero } else { Decimal };
          while let Some(u) = iter.peek().cloned() {
            if !is_decimal_digits(u) {
              break;
            }
            iter.next();
            char_count += 1;
            digits_len += 1;
          }
        }
        if kind == ImplicitOctal {
          if let Some(next) = iter.peek().cloned() {
            if is_decimal_digits(next) && ch(next) > '7' {
              while let Some(u) = iter.peek().cloned() {
                if !is_decimal_digits(u) {
                  break;
                }
                iter.next();
                char_count += 1;
                digits_len += 1;
              }
              kind = DecimalLeadingZero;
            }
          }
        }
      }

      if let Some(next) = iter.peek().cloned() {
        if ch(next) == '.' && !is_period_seen {
          if match kind {
            Decimal | DecimalLeadingZero => true,
            _ => false,
          } {
            kind = if is_leading_zeros { DecimalLeadingZero } else { Decimal };
            is_period_seen = true;
            iter.next();
            char_count += 1;
          }
        }
      }

      while let Some(u) = iter.peek().cloned() {
        if !is_period_seen && ch(u) == '.' && kind == Decimal {
          is_period_seen = true;
          iter.next();
          continue;
        }
        if !is_decimal_digits(u) {
          break;
        }
        iter.next();
        char_count += 1;
        digits_len += 1;
      }

      if let Some(next) = iter.peek().cloned() {
        if ch(next) == 'e' || ch(next) == 'E' {
          has_exponents_part = true;
          if kind != Decimal && kind != DecimalLeadingZero {
            for _ in 0..(char_count + 2) {
              origin.next();
            }
            return Err(NumericConvertionError::UnexpectedTokenFound);
          }
          iter.next();
          if let Some(next) = iter.peek().cloned() {
            if ch(next) == '+' || ch(next) == '-' {
              iter.next();
              char_count += 1;
            }
            if let Some(next) = iter.peek().cloned() {
              if !is_decimal_digits(next) {
                for _ in 0..(char_count + 2) {
                  origin.next();
                }
                return Err(NumericConvertionError::ExponentsExpectedNumber);
              }
              iter.next();
              char_count += 1;
            } else {
              for _ in 0..(char_count + 2) {
                origin.next();
              }
              return Err(NumericConvertionError::ExponentsExpectedNumber);
            }
            while let Some(u) = iter.peek().cloned() {
              if !is_decimal_digits(u) {
                break;
              }
              iter.next();
              char_count += 1;
            }
          } else {
            for _ in 0..(char_count + 2) {
              origin.next();
            }
            return Err(NumericConvertionError::ExponentsExpectedNumber);
          }
        }
      }
    }

    return match kind {
      Decimal | DecimalLeadingZero => {
        if kind == DecimalLeadingZero && digits_len >= 1 && !has_exponents_part && !is_allow_leading_zeroes {
          return Err(NumericConvertionError::LeadingZeroesNotAllowed);
        }
        if digits_len <= 9 && !has_exponents_part && !is_period_seen {
          match parse_uint32_without_exponents(origin, should_stop_if_invalid_token_found) {
            Ok(val) => Ok((val as f64, kind)),
            Err(e) => Err(e),
          }
        } else {
          match parse_decimal(origin, should_stop_if_invalid_token_found) {
            Ok(val) => Ok((val as f64, kind)),
            Err(e) => Err(e),
          }
        }
      }
      _ => match parse_octal(origin, should_stop_if_invalid_token_found) {
        Ok(val) => Ok((val as f64, kind)),
        Err(e) => Err(e),
      },
    };
  }

  return Err(NumericConvertionError::UnexpectedEndOfInput);
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
            assert!(is_identifier_start(uc, false));
          } else {
            unreachable!();
          }
        } else {
          assert!(is_identifier_start((*c) as u32, false));
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
    let v = fs::read_to_string(format!("{}/test/data/unicode/id_continue.txt", env!("CARGO_MANIFEST_DIR"))).unwrap();
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

  fn run_parse_numeric_value_test(value: &str, expected_value: f64, expected_kind: NumericValueKind) {
    let buf = value.encode_utf16().collect::<Vec<_>>();
    let mut iter = buf.into_iter().peekable();
    let mut clone = iter.clone();
    let result = parse_numeric_value(iter.by_ref(), &mut clone, false, true, true);
    assert!(result.is_ok());
    let (value, kind) = result.unwrap();
    assert_eq!(value, expected_value);
    assert_eq!(kind, expected_kind);
  }

  #[test]
  fn parse_numeric_value_int_test() {
    run_parse_numeric_value_test("120304374", 120304374.0, NumericValueKind::Decimal);
  }

  #[test]
  fn parse_numeric_value_hex_test() {
    run_parse_numeric_value_test("0xFFFF", 0xFFFF as f64, NumericValueKind::Hex);
  }

  #[test]
  fn parse_numeric_value_binary_test() {
    run_parse_numeric_value_test("0b010101", 0b010101 as f64, NumericValueKind::Binary);
  }

  #[test]
  fn parse_numeric_value_octal_test() {
    run_parse_numeric_value_test("0o7766", 0o7766 as f64, NumericValueKind::Octal);
  }

  #[test]
  fn parse_numeric_value_implicit_octal_test() {
    run_parse_numeric_value_test("07766", 0o7766 as f64, NumericValueKind::ImplicitOctal);
  }

  #[test]
  fn parse_numeric_value_int_leading_zeros_octal_test() {
    run_parse_numeric_value_test("07769837", 7769837.0, NumericValueKind::DecimalLeadingZero);
  }

  #[test]
  fn parse_numeric_value_floating_point_test() {
    run_parse_numeric_value_test("0.7769837", 0.7769837, NumericValueKind::Decimal);
  }

  #[test]
  fn parse_numeric_value_floating_point_not_start_zeros_test() {
    run_parse_numeric_value_test(".7769837", 0.7769837, NumericValueKind::Decimal);
  }

  #[test]
  fn parse_numeric_value_floating_point_intermeditate_test() {
    run_parse_numeric_value_test("776.9837", 776.9837, NumericValueKind::Decimal);
  }

  #[test]
  fn parse_numeric_value_exponent() {
    run_parse_numeric_value_test("120e+10", 120e+10, NumericValueKind::Decimal);
  }

  #[test]
  fn parse_numeric_value_exponent2() {
    run_parse_numeric_value_test("120e-200", 120e-200, NumericValueKind::Decimal);
  }
}
