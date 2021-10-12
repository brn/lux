use std::cell::{Ref, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::rc::Rc;
use std::{env, fs};

macro_rules! gc_enum {
  ($name:ident { $($item:ident($id_start:expr, $id_continue:expr),)* }) => {
    #[derive(Copy, Clone, Debug)]
    enum $name {
      $(
        $item,
      )*
    }
    impl $name {
      pub fn is_id_start(&self) -> bool {
        return match self {
          $(
            &$name::$item => $id_start,
          )*
        }
      }

      pub fn is_id_continue(&self) -> bool {
        return match self {
          $(
            &$name::$item => $id_continue,
          )*
        }
      }
    }

    fn gc_from_str(s: &str) -> Result<GeneralCategory, String> {
        return match s {
          $(
            stringify!($item) => Ok($name::$item),
          )*
          _ => Err(format!("{} is not a valid GeneralCategory", s))
        };
      }
  };
}

gc_enum! {
  GeneralCategory {
    Lu(true, true),
    Ll(true, true),
    Lt(true, true),
    Lm(true, true),
    Lo(true, true),
    Mn(false, true),
    Mc(false, true),
    Me(false, false),
    Nd(false, true),
    Nl(true, false),
    No(false, false),
    Pc(false, true),
    Pd(false, false),
    Ps(false, false),
    Pe(false, false),
    Pi(false, false),
    Pf(false, false),
    Po(false, false),
    Sm(false, false),
    Sc(false, false),
    Sk(false, false),
    So(false, false),
    Zs(false, false),
    Zl(false, false),
    Zp(false, false),
    Cc(false, false),
    Cf(false, false),
    Cs(false, false),
    Co(false, false),
    Cn(false, false),
  }
}

macro_rules! decmp_type_enum {
  ($name:ident { $($item:ident($repr:tt),)* }) => {
    #[derive(Copy, Clone, Debug)]
    enum $name {
      $(
        $item,
      )*
    }

    fn dt_from_str(s: &str) -> Result<$name, String> {
        return match s {
          $(
            stringify!($repr) => Ok($name::$item),
          )*
            _ => Err(format!("{} is not a valid DecompositionType", s))
        };
      }
  };
}
decmp_type_enum! {
  DecompositionType {
    Font(font),
    NoBreak(noBreak),
    Initial(initial),
    Medial(medial),
    Final(final),
    Isolated(isolated),
    Circle(circle),
    Super(super),
    Sub(sub),
    Vertical(vertical),
    Wide(wide),
    Narrow(narrow),
    Small(small),
    Square(square),
    Fraction(fraction),
    Compat(compat),
  }
}

struct UnicodeDataLayout {
  codepoint: u32,
  name: String,
  general_category: GeneralCategory,
  canonical_combining_class: u32,
  bidi_class: String,
  decomposition_type: Option<DecompositionType>,
  decomposition_mapping: Vec<u32>,
  decimal_digit: u32,
  digit_value: u32,
  numeric_value: String,
  bidi_mirrored: String,
  unicode_1_name: String,
  iso_comment: String,
  upper_case: String,
  lower_case: String,
  title_case: String,
}
#[derive(Clone)]
struct UnicodeData(Rc<UnicodeDataLayout>);

impl std::fmt::Debug for UnicodeData {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return writeln!(
      f,
      "{{ code: \"U+{:x}\", name: \"{}\", generic_category: \"{:?}\", ccc: \"{}\", bidi_class: \"{}\", decmp_type: \"{:?}\", decmp_mapping: \"{:?}\", dedcimal_digit: \"{}\", digit_value: \"{}\", numeric_value: \"{}\", bidi_mirrored: \"{}\", u1n: \"{}\", iso_comment: \"{}\", upper_case: \"{}\", lower_case: \"{}\", title_case: \"{}\" }}",
      self.0.codepoint,
      self.0.name,
      self.0.general_category,
      self.0.canonical_combining_class,
      self.0.bidi_class,
      self.0.decomposition_type,
      self.0.decomposition_mapping,
      self.0.decimal_digit,
      self.0.digit_value,
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

fn trim_comments(line: &str) -> String {
  let mut ret = String::new();
  for ch in line.chars() {
    if ch == '#' {
      break;
    }
    ret.push(ch);
  }
  return ret;
}

fn extract_decomposition_content(item: &str) -> String {
  let mut ret = String::new();
  for i in item.chars() {
    if i == '<' {
      continue;
    } else if i == '>' {
      break;
    }
    ret.push(i);
  }
  return ret;
}

fn extract_decomposition(item: &str) -> (Option<DecompositionType>, Vec<u32>) {
  let trimmed = item.trim_start();
  let mut d_type = String::new();
  let mut d_mapping = vec![String::new()];
  let mut is_d_type_start = false;
  let mut last = '\0';
  for i in item.chars() {
    if i == '<' {
      is_d_type_start = true;
      last = i;
      continue;
    } else if i == '>' {
      is_d_type_start = false;
      last = i;
      continue;
    } else if i == ' ' {
      if last == '>' {
        continue;
      } else {
        d_mapping.push(String::new());
        continue;
      }
      last = i;
    }
    if is_d_type_start {
      d_type.push(i);
    } else {
      d_mapping.last_mut().unwrap().push(i);
    }
    last = i;
  }
  return (
    if d_type.len() > 0 {
      Some(dt_from_str(&d_type).unwrap())
    } else {
      None
    },
    d_mapping
      .iter()
      .filter(|mapping| mapping.len() > 0)
      .map(|mapping| u32::from_str_radix(mapping, 16).unwrap())
      .collect::<Vec<u32>>(),
  );
}

impl UnicodeData {
  fn new(codepoint: u32, raw_unicode_char_info: &Vec<&str>) -> Self {
    let (decomposition_type, decomposition_mapping) = extract_decomposition(raw_unicode_char_info[5]);

    let data = UnicodeData(Rc::new(UnicodeDataLayout {
      codepoint,
      name: raw_unicode_char_info[1].to_owned(),
      general_category: gc_from_str(raw_unicode_char_info[2]).unwrap(),
      canonical_combining_class: if raw_unicode_char_info[3].len() > 0 {
        u32::from_str_radix(raw_unicode_char_info[3], 10).unwrap()
      } else {
        0
      },
      bidi_class: raw_unicode_char_info[4].to_owned(),
      decomposition_type,
      decomposition_mapping,
      decimal_digit: if raw_unicode_char_info[6].len() > 0 {
        u32::from_str_radix(raw_unicode_char_info[6], 10).unwrap()
      } else {
        0
      },
      digit_value: if raw_unicode_char_info[7].len() > 0 {
        u32::from_str_radix(raw_unicode_char_info[7], 10).unwrap()
      } else {
        0
      },
      numeric_value: raw_unicode_char_info[8].to_owned(),
      bidi_mirrored: raw_unicode_char_info[9].to_owned(),
      unicode_1_name: raw_unicode_char_info[10].to_owned(),
      iso_comment: raw_unicode_char_info[11].to_owned(),
      upper_case: raw_unicode_char_info[12].to_owned(),
      lower_case: raw_unicode_char_info[13].to_owned(),
      title_case: raw_unicode_char_info[14].to_owned(),
    }));

    return data;
  }

  fn normalize(&self, table: &UnicodeDataTable) -> Vec<UnicodeData> {
    const SBASE: i32 = 0xAC00;
    const LBASE: i32 = 0x1100;
    const VBASE: i32 = 0x1161;
    const TBASE: i32 = 0x11A7;
    const LCOUNT: i32 = 19;
    const VCOUNT: i32 = 21;
    const TCOUNT: i32 = 28;
    const NCOUNT: i32 = VCOUNT * TCOUNT;
    const SCOUNT: i32 = LCOUNT * NCOUNT;
    let sindex = (self.0.codepoint as i32) - SBASE;
    if sindex < 0 || sindex > SCOUNT {
      if let Some(codepoints) = table.get_canonical_decomposition(self.0.codepoint) {
        let mut ret = Vec::new();
        for p in codepoints.iter() {
          if let Some(data) = table.get_unicode(*p) {
            ret.push(data.clone());
          }
        }
        UnicodeData::sort_ccc(&mut ret);
        return ret;
      }
      return vec![self.clone()];
    }

    let mut ret = Vec::new();
    let l = LBASE + sindex / NCOUNT;
    let v = VBASE + (sindex % NCOUNT) / TCOUNT;
    let t = TBASE + sindex % TCOUNT;
    ret.push(table.get_unicode(l as u32).unwrap());
    ret.push(table.get_unicode(v as u32).unwrap());
    if t != TBASE {
      ret.push(table.get_unicode(t as u32).unwrap())
    };
    UnicodeData::sort_ccc(&mut ret);
    return ret;
  }

  fn sort_ccc(list: &mut Vec<UnicodeData>) {
    for i in (0..list.len()) {
      let mut min_i = i;
      for prev_index in (i..0) {
        let prev_data = &list[prev_index];
        if list[i].canonical_combining_class < prev_data.canonical_combining_class {
          min_i = prev_index;
        }
      }
      let tmp = list[min_i].clone();
      list[min_i] = list[i].clone();
      list[i] = tmp;
    }
  }
}

impl std::ops::Deref for UnicodeData {
  type Target = UnicodeDataLayout;
  fn deref(&self) -> &Self::Target {
    return &(*self.0);
  }
}

#[derive(Clone)]
struct UnicodeDataTable {
  decomposition_table: HashMap<u32, (Option<DecompositionType>, Vec<u32>)>,
  composition_table: HashMap<Vec<u32>, u32>,
  composition_exclusion_table: HashSet<u32>,
  canonical_combining_class_table: HashMap<u32, u32>,
  unicode_table: HashMap<u32, UnicodeData>,
  unicode_data: Vec<UnicodeData>,
  prop_table: HashMap<u32, String>,
}
impl UnicodeDataTable {
  fn new() -> Self {
    return UnicodeDataTable {
      decomposition_table: HashMap::new(),
      composition_table: HashMap::new(),
      composition_exclusion_table: HashSet::new(),
      canonical_combining_class_table: HashMap::new(),
      unicode_table: HashMap::new(),
      unicode_data: Vec::new(),
      prop_table: HashMap::new(),
    };
  }

  fn read_all_data(&mut self, raw_unicode_data: &str, raw_composition_exclusion_data: &str, raw_prop_list_data: &str) {
    {
      let lines = raw_composition_exclusion_data.split('\n').collect::<Vec<_>>();
      for line in lines.iter() {
        if line.len() == 0 {
          continue;
        }
        if (line.as_bytes()[0] as char) == '#' {
          continue;
        }
        let trimmed = trim_comments(line);
        let splited = trimmed.split(';').collect::<Vec<_>>();
        if splited[1].trim() == "Full_Composition_Exclusion" {
          if splited[0].contains("..") {
            let range = splited[0].trim().split("..").collect::<Vec<_>>();
            for code in (u32::from_str_radix(range[0].trim(), 16).unwrap()..u32::from_str_radix(range[1].trim(), 16).unwrap() + 1) {
              self.composition_exclusion_table.insert(code);
            }
          } else {
            self
              .composition_exclusion_table
              .insert(u32::from_str_radix(splited[0].trim(), 16).unwrap());
          }
        }
      }
    }
    self.read_props(raw_prop_list_data);
    {
      let lines = raw_unicode_data.split('\n').collect::<Vec<_>>();
      let mut is_next_range_end = false;
      let mut last_codepoint = 0;
      for line in lines.iter() {
        if line.len() == 0 {
          continue;
        }
        let trimmed = trim_comments(line);
        let raw_unicode_char_info = trimmed.split(';').collect::<Vec<_>>();
        let codepoint = u32::from_str_radix(raw_unicode_char_info[0].trim(), 16).unwrap();
        if is_next_range_end {
          for code in (last_codepoint..codepoint + 1) {
            self.init_unicode_data(code, &raw_unicode_char_info);
          }
          is_next_range_end = false;
          continue;
        }
        if raw_unicode_char_info[1].ends_with("First>") {
          is_next_range_end = true;
          last_codepoint = codepoint;
        } else {
          self.init_unicode_data(codepoint, &raw_unicode_char_info);
        }
      }
    }
  }

  fn read_props(&mut self, raw_prop_list_data: &str) {
    let lines = raw_prop_list_data.split('\n').collect::<Vec<_>>();
    for line in lines.iter() {
      if line.starts_with('#') || line.len() == 0 {
        continue;
      }
      let trimmed = trim_comments(line);
      let splited = trimmed.split(';').map(|str| str.trim()).collect::<Vec<_>>();
      if splited[0].contains("..") {
        let range = splited[0].split("..").collect::<Vec<_>>();
        let start = u32::from_str_radix(range[0], 16).unwrap();
        let end = u32::from_str_radix(range[1], 16).unwrap();
        for code in start..(end + 1) {
          self.prop_table.insert(code, splited[1].to_owned());
        }
      } else {
        self
          .prop_table
          .insert(u32::from_str_radix(splited[0], 16).unwrap(), splited[1].to_owned());
      }
    }
  }

  fn init_unicode_data(&mut self, codepoint: u32, raw_unicode_char_info: &Vec<&str>) {
    let data = UnicodeData::new(codepoint, raw_unicode_char_info);
    if data.decomposition_type.is_some() || data.decomposition_mapping.len() > 0 {
      self
        .decomposition_table
        .insert(data.codepoint, (data.decomposition_type, data.decomposition_mapping.clone()));
    }
    if data.decomposition_type.is_none()
      && data.decomposition_mapping.len() > 1
      && !self.composition_exclusion_table.contains(&data.codepoint)
    {
      self.composition_table.insert(data.decomposition_mapping.clone(), data.codepoint);
    }
    if data.canonical_combining_class != 0 {
      self
        .canonical_combining_class_table
        .insert(data.codepoint, data.canonical_combining_class);
    }
    self.unicode_table.insert(data.codepoint, data.clone());
    self.unicode_data.push(data.clone());
  }

  fn get_unicode(&self, codepoint: u32) -> Option<UnicodeData> {
    if let Some(data) = self.unicode_table.get(&codepoint) {
      return Some(data.clone());
    }
    return None;
  }

  fn get_canonical_decomposition(&self, codepoint: u32) -> Option<Vec<u32>> {
    if let Some((opt_dt, ref points)) = self.decomposition_table.get(&codepoint) {
      if opt_dt.is_none() {
        return Some(points.clone());
      }
    }
    return None;
  }

  fn unicode_data(&self) -> &Vec<UnicodeData> {
    return &self.unicode_data;
  }
}

struct UnicodeProperty {
  id_start: Vec<UnicodeData>,
  id_start_table: HashMap<u32, UnicodeData>,
  id_continue: Vec<UnicodeData>,
}

impl UnicodeProperty {
  fn new() -> Self {
    UnicodeProperty {
      id_start: Vec::new(),
      id_start_table: HashMap::new(),
      id_continue: Vec::new(),
    }
  }

  fn collect(&mut self, table: &UnicodeDataTable) {
    for data in table.unicode_data().iter() {
      if let Some(val) = table.prop_table.get(&data.codepoint) {
        if val == "Pattern_Syntax" || val == "Pattern_White_Space" {
          continue;
        }
      }
      if data.general_category.is_id_start()
        || match data.codepoint {
          0x2118 | 0x212E | 0x309B | 0x309C => true,
          _ => false,
        }
      {
        let normalization = data.normalize(table);
        if normalization.len() > 1 || (normalization.len() == 1 && normalization[0].codepoint != data.codepoint) {
          self.id_start.push(data.clone());
          self.id_start_table.insert(data.codepoint, data.clone());
        }
        for d in normalization.iter() {
          self.id_start.push(d.clone());
          self.id_start_table.insert(d.codepoint, d.clone());
        }
      }
    }
    println!("{}", self.id_start.len());
  }
}

fn read_unicode_text(filename: &str) -> String {
  let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  d.push(&format!("tools/{}", filename));
  return fs::read_to_string(d).unwrap();
}

fn main() {
  let raw_unicode_data = read_unicode_text("UnicodeData.txt");
  let raw_composition_exclusion_data = read_unicode_text("DerivedNormalizationProps.txt");
  let raw_prop_list_data = read_unicode_text("PropList.txt");
  let mut table = UnicodeDataTable::new();
  table.read_all_data(&raw_unicode_data, &raw_composition_exclusion_data, &raw_prop_list_data);
  let mut prop = UnicodeProperty::new();
  prop.collect(&table);
}
