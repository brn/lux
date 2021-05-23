use super::u16_str::FixedU16CodePointArray;
use crate::context::Context;
use crate::structs::Repr;
use crate::unicode::chars;
use crate::unicode::{is_surrogate_pair, is_white_space};
use crate::utility::{BitOperator, Bitset};

#[derive(Copy, Clone)]
enum ToIntegerState {
  Start,
  StrDecimalLiteral,
  NonDecimalIntegerLiteral,
}

enum ToIntegerFlag {
  Octal,
  Binary,
  Hex,
  Negate,
}

static INFINITY_WORD: &str = "Infinity";

fn parse_exponent(array: FixedU16CodePointArray, start: usize, acc: f64, radix: u32) -> Result<Repr, Repr> {
  let mut next = start + 1;
  if array.length() > next {
    let is_positive = array[next] == '+' as u16;
    next += 1;
    if next < array.length() {
      let mut p: u32 = 1;
      let mut digit = 1;
      for i in next..array.length() {
        if is_surrogate_pair(array[i]) {
          return Err(Repr::nan());
        }
        if chars::is_decimal_digits(array[i]) {
          p *= digit;
          p += chars::to_int(array[i]);
          digit *= 10;
        } else {
          return Err(Repr::nan());
        }
      }
      let exp = radix.pow(p);
      if is_positive {
        return Ok(Repr::from(acc * (exp as f64)));
      }
      return Ok(Repr::from(acc / exp as f64));
    }
    return Err(Repr::nan());
  }

  return Err(Repr::nan());
}

fn parse_octal(array: FixedU16CodePointArray, start: usize, end: usize) -> Result<Repr, Repr> {
  let mut digit = 1;
  let mut result = 0.0_f64;
  for j in start..end {
    let n = array[j];
    if is_surrogate_pair(n) {
      return Err(Repr::nan());
    }
    if n == 'e' as u16 {
      return parse_exponent(array, j, result, 10);
    };

    if !chars::is_octal_digits(n) {
      return Err(Repr::nan());
    }
    result *= digit as f64;
    result += chars::to_int(n) as f64;
    digit *= 8;
  }
  return Ok(Repr::from(result));
}

fn parse_binary(array: FixedU16CodePointArray, start: usize, end: usize) -> Result<Repr, Repr> {
  let mut digit = 1;
  let mut result = 0.0_f64;
  for j in start..end {
    let n = array[j];
    if is_surrogate_pair(n) {
      return Err(Repr::nan());
    }
    if n == 'e' as u16 {
      return parse_exponent(array, j, result.into(), 10);
    };

    if !chars::is_binary_digits(n) {
      return Err(Repr::nan());
    }
    result *= digit as f64;
    result += chars::to_int(n) as f64;
    digit *= 2;
  }
  return Ok(Repr::from(result));
}

fn parse_hex(array: FixedU16CodePointArray, start: usize, end: usize) -> Result<Repr, Repr> {
  let mut digit = 1;
  let mut result = 0.0_f64;

  for j in start..end {
    let n = array[j];
    if is_surrogate_pair(n) {
      return Err(Repr::nan());
    }
    if n == 'e' as u16 {
      return parse_exponent(array, j, result, 10);
    };
    if !chars::is_hex_digits(n) {
      return Err(Repr::nan());
    }
    result *= digit as f64;
    result += chars::to_int(n) as f64;
    digit *= 16;
  }
  return Ok(Repr::from(result));
}

fn parse_deciaml(array: FixedU16CodePointArray, start: usize, end: usize) -> Result<Repr, Repr> {
  let mut digit = 1;
  let mut result = 0.0_f64;
  let mut is_point = false;
  let mut start_mut = start;
  if array[start] == '.' as u16 {
    is_point = true;
    start_mut += 1;
  }
  for j in start_mut..end {
    let n = array[j];
    if is_surrogate_pair(n) {
      return Err(Repr::nan());
    }
    if n == 'e' as u16 {
      return parse_exponent(array, j, result, 10);
    };
    if !chars::is_decimal_digits(n) {
      return Err(Repr::nan());
    }
    if !is_point {
      result *= digit as f64;
    } else {
      result /= digit as f64;
    }
    result += chars::to_int(n) as f64;
    digit *= 10;
  }

  return Ok(Repr::from(result));
}

pub fn to_number(array: FixedU16CodePointArray) -> Result<Repr, Repr> {
  if array.length() == 0 || (array.length() == 1 && is_white_space(array[0])) {
    return Ok(Repr::from(0.0_f64));
  }

  let mut start = 0;
  let mut end = array.length();

  for i in 0..array.length() {
    if !is_white_space(array[i]) {
      start = i;
      break;
    }
  }

  for i in (array.length() - 1)..0 {
    if !is_white_space(array[i]) {
      end = i;
      break;
    }
  }

  let mut next = ToIntegerState::Start;
  let mut bitset = Bitset::<u8>::new();
  let c = array[start];
  if is_surrogate_pair(c) {
    return Err(Repr::nan());
  }

  if c == '+' as u16 || c == '-' as u16 {
    if c == '-' as u16 {
      bitset.set(ToIntegerFlag::Negate as usize);
    }
    start += 1;
    next = ToIntegerState::StrDecimalLiteral;
  } else if c == '0' as u16 {
    let peak = if start + 1 < array.length() {
      array[start + 1]
    } else {
      0
    };
    if is_surrogate_pair(peak) {
      return Err(Repr::nan());
    }
    if peak == 'b' as u16 || peak == 'B' as u16 {
      bitset.set(ToIntegerFlag::Binary as usize);
    } else if peak == 'o' as u16 || peak == 'O' as u16 {
      bitset.set(ToIntegerFlag::Octal as usize);
    } else if peak == 'x' as u16 || peak == 'X' as u16 {
      bitset.set(ToIntegerFlag::Hex as usize);
    }
    next = ToIntegerState::NonDecimalIntegerLiteral;
  } else if chars::is_decimal_digits(c) {
    next = ToIntegerState::StrDecimalLiteral;
  } else if c == 'I' as u16 {
    // Infinity
    for j in start..(start + 8) {
      if is_surrogate_pair(array[j]) {
        return Err(Repr::nan());
      }
      if end <= j || (INFINITY_WORD.as_bytes()[j] as u16) != array[j] {
        return Err(Repr::nan());
      }
    }
    return Ok(Repr::infinity());
  } else {
    return Err(Repr::nan());
  }

  match next {
    ToIntegerState::Start => {
      return Err(Repr::nan());
    }
    ToIntegerState::StrDecimalLiteral => {
      match parse_deciaml(array, start, end) {
        Ok(r) => {
          if bitset.get(ToIntegerFlag::Negate as usize) {
            return Ok(Repr::from(-1.0_f64 * (r.to_number_unchecked())));
          }
          return Ok(r);
        }
        Err(e) => {
          return Err(e);
        }
      };
    }
    ToIntegerState::NonDecimalIntegerLiteral => {
      if bitset.get(ToIntegerFlag::Octal as usize) {
        return parse_octal(array, start, end);
      } else if bitset.get(ToIntegerFlag::Binary as usize) {
        return parse_binary(array, start, end);
      }
      debug_assert!(bitset.get(ToIntegerFlag::Hex as usize));
      return parse_hex(array, start, end);
    }
  }
}
