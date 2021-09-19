pub const HIGH_SURROGATE_START: u16 = 0xD800;
pub const HIGH_SURROGATE_END: u16 = 0xDBFF;

pub const LOW_SURROGATE_START: u16 = 0xDC00;
pub const LOW_SURROGATE_END: u16 = 0xDFFF;

pub fn is_surrogate_pair(ch: u16) -> bool {
  return if (ch >= HIGH_SURROGATE_START && ch <= HIGH_SURROGATE_END) || (ch >= LOW_SURROGATE_START && ch <= LOW_SURROGATE_END) {
    true
  } else {
    false
  };
}

pub fn is_white_space(ch: u16) -> bool {
  return if ch == 0x0009
    || ch == 0x000B
    || ch == 0x000C
    || ch == 0020
    || ch == 0x00A0
    || ch == 0xFEFF
    || ch == 0x1680
    || ch == 0x2000
    || ch == 0x2001
    || ch == 0x2002
    || ch == 0x2003
    || ch == 0x2004
    || ch == 0x2005
    || ch == 0x2006
    || ch == 0x2007
    || ch == 0x2008
    || ch == 0x2009
    || ch == 0x200a
    || ch == 0x202F
    || ch == 0x205f
    || ch == 0x3000
  {
    true
  } else {
    false
  };
}
