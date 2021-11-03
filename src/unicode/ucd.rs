use super::data::*;
use super::ucd_type::*;

pub struct Ucd(&'static RawUcd);

impl Ucd {
  pub const fn new(raw_ucd: &'static RawUcd) -> Self {
    return Ucd(raw_ucd);
  }

  pub const fn codepoint(&self) -> u32 {
    return self.0 .0;
  }

  pub const fn is_id_start(&self) -> bool {
    return (self.0 .8 & UcdProps::ID_START) == UcdProps::ID_START;
  }

  pub const fn is_id_continue(&self) -> bool {
    return (self.0 .8 & UcdProps::ID_CONTINUE) == UcdProps::ID_CONTINUE;
  }

  pub fn get_id_property(code: u32) -> UcdIdProperty {
    let l2i = ID_PROPERTY_LEVEL1_INDICES[(code >> 9) as usize];
    let vi = ID_PROPERTY_LEVEL2_INDICES[((l2i << 6) + ((code >> 3) & 0b111110)) as usize];
    return ID_PROPERTY_VALUES[((vi << 4) + (code & 0x000f)) as usize];
  }
}
