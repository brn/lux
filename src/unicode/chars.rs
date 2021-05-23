pub fn is_decimal_digits(u: u16) -> bool {
  return u >= 0x30 && u <= 0x39;
}

pub fn is_octal_digits(u: u16) -> bool {
  return u >= 0x30 && u <= 0x37;
}

pub fn is_binary_digits(u: u16) -> bool {
  return u >= 0x30 && u <= 0x31;
}

pub fn is_hex_digits(u: u16) -> bool {
  return (u >= 0x41 && u <= 0x46) || (u >= 0x61 && u <= 0x66) || (u >= 0x30 && u <= 0x31);
}

pub fn to_int(u: u16) -> u32 {
  assert!(u >= 0x30 && u <= 0x39);
  return (u - 0x30 as u16) as u32;
}
