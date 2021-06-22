use super::u16_str::FixedU16CodePointArray;
use crate::structs::Repr;
use crate::unicode::chars;
use crate::unicode::is_white_space;

pub fn to_number(array: FixedU16CodePointArray) -> Result<Repr, Repr> {
  if array.length() == 0 || (array.length() == 1 && is_white_space(array[0])) {
    return Ok(Repr::from(0.0_f64));
  }

  let mut iter = array.into_iter();
  let mut clone = iter.clone();
  return match chars::parse_numeric_value(iter.by_ref(), &mut clone, false) {
    Ok((value, _)) => Ok(Repr::from(value)),
    _ => Err(Repr::nan()),
  };
}
