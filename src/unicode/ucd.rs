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

  // pub fn get_id_property(code: u32) -> UcdIdProperty {
  //   let l2i = ID_PROPERTY_LEVEL1_INDICES[(code >> 9) as usize];
  //   let vi = ID_PROPERTY_LEVEL2_INDICES[((l2i << 6) + ((code >> 3) & 0b111110)) as usize];
  //   return ID_PROPERTY_VALUES[((vi << 4) + (code & 0x000f)) as usize];
  // }

  pub fn get_id_property(ch: u32) -> UcdIdProperty {
    let level_1_index = (ch >> 8) as usize;
    let level_2_base_index = ID_PROPERTY_LEVEL1_INDICES[level_1_index];
    let level_2_index = ((level_2_base_index << 4) + ((ch >> 4) & 0x000F)) as usize * 2;
    let level_3_base_index =
      unsafe { std::ptr::read_unaligned((&ID_PROPERTY_LEVEL2_INDICES as *const u8).offset(level_2_index as isize) as *const u16) };
    let swapped_l3_base_index = level_3_base_index.to_le() as u32;
    let level_3_index = ((swapped_l3_base_index << 4) + (ch & 0x000f)) as usize;
    // println!(
    //   "leve1[{}] level2[base: {}, real: {}], level3[base: {} real: {}] {:?} {:?}",
    //   level_1_index,
    //   level_2_base_index,
    //   level_2_index,
    //   swapped_l3_base_index,
    //   level_3_index,
    //   char::from_u32(ch),
    //   ID_PROPERTY_VALUES[level_3_index]
    // );
    return ID_PROPERTY_VALUES[level_3_index];
  }
}
