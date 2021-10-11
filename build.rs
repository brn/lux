use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;
use std::{env, fs};

struct UnicodeDataLayout {
  general_category: String,
  canonical_combining_class: String,
  bidi_class: String,
  decomposition_type: Option<String>,
  decomposition_mapping: String,
  numeric_type: String,
  numeric_value: String,
  bidi_mirrored: String,
  unicode_1_name: String,
  iso_comment: String,
  upper_case: String,
  lower_case: String,
  title_case: String,
}
struct UnicodeData(Rc<UnicodeDataLayout>);

impl std::fmt::Debug for UnicodeData {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return writeln!(
      f,
      "{{ gc: {}, ccc: {}, bc: {}, dt: {:?}, dm: {}, nt: {}, nv: {}, bm: {}, u1n: {}, ic: {}, uc: {}, lc: {}, tc: {} }}",
      self.0.general_category,
      self.0.canonical_combining_class,
      self.0.bidi_class,
      self.0.decomposition_type,
      self.0.decomposition_mapping,
      self.0.numeric_type,
      self.0.numeric_value,
      self.0.bidi_mirrored,
      self.0.unicode_1_name,
      self.0.iso_comment,
      self.0.upper_case,
      self.0.lower_case,
      self.0.title_case
    );
  }
}

impl UnicodeData {
  fn new(raw_unicode_data: &str) -> Vec<UnicodeData> {
    let mut ret = Vec::new();
    let lines = raw_unicode_data.split('\n').collect::<Vec<_>>();
    for line in lines.iter() {
      if line.len() == 0 {
        continue;
      }
      let raw_unicode_char_info = line.split(';').collect::<Vec<_>>();
      println!("{:?}", raw_unicode_char_info);
      ret.push(UnicodeData(Rc::new(UnicodeDataLayout {
        general_category: raw_unicode_char_info[0].to_owned(),
        canonical_combining_class: raw_unicode_char_info[1].to_owned(),
        bidi_class: raw_unicode_char_info[2].to_owned(),
        decomposition_type: Some(raw_unicode_char_info[3].to_owned()),
        decomposition_mapping: raw_unicode_char_info[3].to_owned(),
        numeric_type: raw_unicode_char_info[4].to_owned(),
        numeric_value: raw_unicode_char_info[4].to_owned(),
        bidi_mirrored: raw_unicode_char_info[5].to_owned(),
        unicode_1_name: raw_unicode_char_info[6].to_owned(),
        iso_comment: raw_unicode_char_info[7].to_owned(),
        upper_case: raw_unicode_char_info[8].to_owned(),
        lower_case: raw_unicode_char_info[9].to_owned(),
        title_case: raw_unicode_char_info[10].to_owned(),
      })));
    }

    return ret;
  }
}

fn main() {
  let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  d.push("tools/UnicodeData.txt");
  let raw_unicode_data = fs::read_to_string(d).unwrap();
  let data = UnicodeData::new(&raw_unicode_data);
  println!("{:?}", data);
}
