#[macro_use]
extern crate paste;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::io::{self, Write};
use std::path::PathBuf;
use std::rc::Rc;
use std::{env, fs};

include!("src/unicode/ucd_type.rs");

struct UnicodeDataLayout {
  codepoint: u32,
  name: String,
  general_category: GeneralCategory,
  canonical_combining_class: u32,
  bidi_class: BidiCategory,
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
  is_id_start: bool,
  is_id_continue: bool,
}
#[derive(Clone)]
struct UnicodeData(Rc<RefCell<UnicodeDataLayout>>);

impl std::fmt::Debug for UnicodeData {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let l = self.0.borrow();
    return writeln!(
      f,
      "{{ code: \"U+{:x}\", name: \"{}\", generic_category: \"{:?}\", ccc: \"{}\", bidi_class: \"{:?}\", decmp_type: \"{:?}\", decmp_mapping: \"{:?}\", dedcimal_digit: \"{}\", digit_value: \"{}\", numeric_value: \"{}\", bidi_mirrored: \"{}\", u1n: \"{}\", iso_comment: \"{}\", upper_case: \"{}\", lower_case: \"{}\", title_case: \"{}\", is_id_start: \"{}\", is_id_continue: \"{}\" }}",
      l.codepoint,
      l.name,
      l.general_category,
      l.canonical_combining_class,
      l.bidi_class,
      l.decomposition_type,
      l.decomposition_mapping,
      l.decimal_digit,
      l.digit_value,
      l.numeric_value,
      l.bidi_mirrored,
      l.unicode_1_name,
      l.iso_comment,
      l.upper_case,
      l.lower_case,
      l.title_case,
      l.is_id_start,
      l.is_id_continue
    );
  }
}

#[derive(Clone)]
struct UnicodeDataSerializer(usize, UnicodeData);
impl std::fmt::Debug for UnicodeDataSerializer {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let l = self.1 .0.borrow();
    return write!(
      f,
      "({}, GeneralCategory::{:?}, {}, {}, [{}], {} ,{}, {}, {})",
      l.codepoint,
      l.general_category,
      l.canonical_combining_class,
      if l.decomposition_type.is_some() {
        format!("Some(DecompositionType::{:?})", l.decomposition_type.unwrap())
      } else {
        "None".to_owned()
      },
      {
        let mut buf = String::new();
        for m in l.decomposition_mapping.iter() {
          buf = format!("{}{},", buf, m);
        }
        if self.0 > l.decomposition_mapping.len() {
          for _ in (l.decomposition_mapping.len()..self.0) {
            buf = format!("{}{},", buf, 0);
          }
        }
        buf
      },
      if l.upper_case.len() > 0 {
        u32::from_str_radix(&l.upper_case, 16).unwrap()
      } else {
        0
      },
      if l.lower_case.len() > 0 {
        u32::from_str_radix(&l.lower_case, 16).unwrap()
      } else {
        0
      },
      if l.title_case.len() > 0 {
        u32::from_str_radix(&l.title_case, 16).unwrap()
      } else {
        0
      },
      if l.is_id_start && l.is_id_continue {
        "UcdProps::ID_START | UcdProps::ID_CONTINUE"
      } else if l.is_id_start {
        "UcdProps::ID_START"
      } else if l.is_id_continue {
        "UcdProps::ID_CONTINUE"
      } else {
        "UcdProps::NONE"
      },
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

    let data = UnicodeData(Rc::new(RefCell::new(UnicodeDataLayout {
      codepoint,
      name: raw_unicode_char_info[1].to_owned(),
      general_category: gc_from_str(raw_unicode_char_info[2]).unwrap(),
      canonical_combining_class: if raw_unicode_char_info[3].len() > 0 {
        u32::from_str_radix(raw_unicode_char_info[3], 10).unwrap()
      } else {
        0
      },
      bidi_class: bidi_from_str(raw_unicode_char_info[4]).unwrap(),
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
      is_id_start: false,
      is_id_continue: false,
    })));

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
    let layout = self.0.borrow();
    let sindex = (layout.codepoint as i32) - SBASE;
    if sindex < 0 || sindex > SCOUNT {
      if let Some(codepoints) = table.get_canonical_decomposition(layout.codepoint) {
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
        if list[i].canonical_combining_class() < prev_data.canonical_combining_class() {
          min_i = prev_index;
        }
      }
      let tmp = list[min_i].clone();
      list[min_i] = list[i].clone();
      list[i] = tmp;
    }
  }

  fn mark_as_id_start(&mut self) {
    self.0.borrow_mut().is_id_start = true;
  }

  fn mark_as_id_continue(&mut self) {
    self.0.borrow_mut().is_id_continue = true;
  }

  fn decomposition_type(&self) -> Option<DecompositionType> {
    return self.0.borrow().decomposition_type;
  }

  fn decomposition_mapping<'a>(&self) -> &'a Vec<u32> {
    let v = &self.0.borrow().decomposition_mapping;
    return unsafe { &*(std::mem::transmute::<&Vec<u32>, *const Vec<u32>>(v)) };
  }

  fn general_category(&self) -> GeneralCategory {
    return self.0.borrow().general_category;
  }

  fn bidi_category(&self) -> BidiCategory {
    return self.0.borrow().bidi_class;
  }

  fn codepoint(&self) -> u32 {
    return self.0.borrow().codepoint;
  }

  fn canonical_combining_class(&self) -> u32 {
    return self.0.borrow().canonical_combining_class;
  }

  fn name(&self) -> &str {
    let v = &self.0.borrow().name;
    return unsafe { &*(std::mem::transmute::<&str, *const str>(v)) };
  }
}

trait UcdAttributeValue: Copy + Hash + Eq + std::fmt::Debug + Default {}
impl<T: Copy + Hash + Eq + std::fmt::Debug + Default> UcdAttributeValue for T {}

#[derive(Clone)]
struct UcdAttributesCmpTable<Attribute: UcdAttributeValue> {
  codepoint_mapping: HashMap<u32, Attribute>,
  level_2_has_bytes: bool,
  level_1_index: Vec<u16>,
  level_2_index: Vec<u16>,
  level_3_data: Vec<Attribute>,
}

impl<T: UcdAttributeValue> UcdAttributesCmpTable<T> {
  fn new() -> Self {
    return UcdAttributesCmpTable {
      codepoint_mapping: HashMap::new(),
      level_2_has_bytes: false,
      level_1_index: Vec::new(),
      level_2_index: Vec::new(),
      level_3_data: Vec::new(),
    };
  }

  fn add(&mut self, codepoint: u32, value: T) {
    self.codepoint_mapping.insert(codepoint, value);
  }

  fn serialize(&mut self, level_2_bit: u32, level_3_bit: u32) -> (&Vec<u16>, Vec<u8>, &Vec<T>) {
    self.serialize_to_cmp_table(level_2_bit, level_3_bit, false);
    let mut level2 = Vec::new();
    for i in self.level_2_index.iter() {
      if self.level_2_has_bytes {
        level2.push(*i as u8);
      } else {
        level2.extend_from_slice(&i.to_ne_bytes());
      }
    }
    return (&self.level_1_index, level2, &self.level_3_data);
  }

  fn serialize_to_cmp_table(&mut self, level_2_bit: u32, level_3_bit: u32, cut_off: bool) -> u32 {
    let mut level_2_hash = HashMap::<String, u16>::new();
    let mut level_3_hash = HashMap::<String, u16>::new();
    const PLANES: u32 = 17;
    let level_1_block = PLANES << (16 - level_2_bit - level_3_bit);
    let level_2_block = 1 << level_2_bit;
    let level_3_block = 1 << level_3_bit;

    let mut level_1_count = 0_u32;
    let mut level_2_count = 0_u32;
    let mut level_3_count = 0_u32;

    let mut level_3_row_data = vec![T::default(); level_3_block];
    let mut level_2_row_data = vec![0_u16; level_2_block];

    if cut_off {
      self.level_1_index = Vec::new();
    }
    let mut ch = 0_u32;
    for i in 0..level_1_block {
      for j in 0..level_2_row_data.len() {
        for k in 0..level_3_row_data.len() {
          if let Some(value) = self.codepoint_mapping.get(&ch) {
            level_3_row_data[k] = *value;
          } else {
            level_3_row_data[k] = T::default();
          }
          ch += 1;
        }

        let key = level_3_row_data.iter().map(|v| format!("{:?}", v)).collect::<Vec<_>>().join(";");
        let mut value_in_hash3 = level_3_hash.get(&key).copied();
        if value_in_hash3.is_none() {
          value_in_hash3 = Some(level_3_count as u16);
          level_3_hash.insert(key, value_in_hash3.unwrap());
          self.level_3_data.extend_from_slice(&level_3_row_data);
          level_3_count += 1;
        }

        level_2_row_data[j] = value_in_hash3.unwrap();
      }

      let key = level_2_row_data.iter().map(|v| format!("{:?}", v)).collect::<Vec<_>>().join(";");
      let mut value_in_hash = level_2_hash.get(&key).copied();
      if value_in_hash.is_none() {
        let value = level_2_count as u16;
        level_2_hash.insert(key, value);
        self.level_2_index.extend_from_slice(&level_2_row_data);
        level_2_count += 1;
        value_in_hash = Some(value);
      }

      self.level_1_index.push(value_in_hash.unwrap());
      level_1_count += 1;
    }

    if cut_off {
      level_3_row_data.fill(T::default());
      let l3_key = level_3_row_data.iter().map(|v| format!("{:?}", v)).collect::<Vec<_>>().join(";");
      if let Some(index) = level_3_hash.get(&l3_key) {
        level_2_row_data.fill(*index);

        let l2_key = level_2_row_data.iter().map(|v| format!("{:?}", v)).collect::<Vec<_>>().join(";");
        if let Some(index) = level_2_hash.get(&l2_key) {
          let cur_len = self.level_1_index.len() - 1;
          while self.level_1_index.len() > 0 && self.level_1_index[self.level_1_index.len() - 1] == *index {
            self.level_1_index.shrink_to(self.level_1_index.len() - 1);
            level_1_count -= 1;
          }
        }
      }
    }

    let level_1_uint = if level_2_hash.len() < 256 { 1 } else { 2 };
    let level_2_uint = if level_3_hash.len() < 256 { 1 } else { 2 };

    self.level_2_has_bytes = level_2_uint == 1;

    return level_1_count + level_2_count + level_3_count;
  }
}

macro_rules! write_to_array {
  ($name:expr, $type_name:expr, $table:expr, $iter:expr, $wrap:expr) => {{
    let mut buf = format!("pub const {}: [{};{}] = [\n  ", $name, $type_name, $table.len());
    let mut count = 0;
    for item in $iter {
      buf = format!("{}{}, ", buf, item);
      if count == $wrap {
        buf = format!("{}\n  ", buf);
        count = 0;
      }
      count += 1;
    }
    format!("{}\n];\n", buf)
  }};
}

#[derive(Clone)]
struct UnicodeDataTable {
  decomposition_table: HashMap<u32, (Option<DecompositionType>, Vec<u32>)>,
  composition_table: HashMap<Vec<u32>, u32>,
  composition_exclusion_table: HashSet<u32>,
  canonical_combining_class_table: HashMap<u32, u32>,
  unicode_table: HashMap<u32, UnicodeData>,
  unicode_data: Vec<UnicodeData>,
  general_category_table: UcdAttributesCmpTable<usize>,
  general_category_value_table: BTreeMap<usize, (GeneralCategory, BidiCategory)>,
  id_property_table: UcdAttributesCmpTable<UcdIdProperty>,
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
      general_category_table: UcdAttributesCmpTable::new(),
      general_category_value_table: BTreeMap::new(),
      id_property_table: UcdAttributesCmpTable::new(),
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
      let mut table = BTreeMap::new();
      let mut map = HashMap::new();
      map.insert((GeneralCategory::Cn, BidiCategory::L), 0);
      table.insert(0, (GeneralCategory::Cn, BidiCategory::L));
      let mut i = 0;
      for line in lines.iter() {
        if line.len() == 0 {
          continue;
        }
        let trimmed = trim_comments(line);
        let raw_unicode_char_info = trimmed.split(';').collect::<Vec<_>>();
        let codepoint = u32::from_str_radix(raw_unicode_char_info[0].trim(), 16).unwrap();
        if is_next_range_end {
          for code in (last_codepoint..codepoint + 1) {
            i += 1;
            self.init_unicode_data(code, &raw_unicode_char_info, &mut map);
          }
          is_next_range_end = false;
          continue;
        }
        if raw_unicode_char_info[1].ends_with("First>") {
          is_next_range_end = true;
          last_codepoint = codepoint;
        } else {
          self.init_unicode_data(codepoint, &raw_unicode_char_info, &mut map);
          i += 1;
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

  fn init_unicode_data(
    &mut self,
    codepoint: u32,
    raw_unicode_char_info: &Vec<&str>,
    map: &mut HashMap<(GeneralCategory, BidiCategory), usize>,
  ) {
    let data = UnicodeData::new(codepoint, raw_unicode_char_info);
    let key = (data.general_category(), data.bidi_category());

    let category_item = if let Some(i) = map.get(&key) {
      *i
    } else {
      let i = map.len();
      map.insert(key, i);
      self.general_category_value_table.insert(i, key);
      i
    };
    if data.decomposition_type().is_some() || data.decomposition_mapping().len() > 0 {
      self
        .decomposition_table
        .insert(data.codepoint(), (data.decomposition_type(), data.decomposition_mapping().clone()));
    }
    if data.decomposition_type().is_none()
      && data.decomposition_mapping().len() > 1
      && !self.composition_exclusion_table.contains(&data.codepoint())
    {
      self
        .composition_table
        .insert(data.decomposition_mapping().clone(), data.codepoint());
    }
    if data.canonical_combining_class() != 0 {
      self
        .canonical_combining_class_table
        .insert(data.codepoint(), data.canonical_combining_class());
    }
    self.general_category_table.add(data.codepoint(), category_item);
    self.unicode_table.insert(data.codepoint(), data.clone());
    self.unicode_data.push(data.clone());
  }

  fn collect_id_property(&mut self) {
    for i in (0..self.unicode_data.len()) {
      let data = self.unicode_data[i].clone();
      if let Some(val) = self.prop_table.get(&data.codepoint()) {
        if val == "Pattern_Syntax" || val == "Pattern_White_Space" {
          self.id_property_table.add(data.codepoint(), UcdIdProperty::None);
          continue;
        }
      }
      if data.general_category().is_id_start()
        || match data.codepoint() {
          0x2118 | 0x212E | 0x309B | 0x309C => true,
          _ => false,
        }
      {
        let normalization = data.normalize(self);
        if normalization.len() > 1 || (normalization.len() == 1 && normalization[0].codepoint() != data.codepoint()) {
          self.id_property_table.add(data.codepoint(), UcdIdProperty::IdStart);
        }
        for _ in normalization.iter().cloned() {
          self.id_property_table.add(data.codepoint(), UcdIdProperty::IdStart);
        }
        if data.codepoint() == 0x1ee0a {
          println!("{:?} {}", data, data.general_category().is_id_start());
        }
      } else if data.general_category().is_id_continue()
        || match data.codepoint() {
          0x1369 | 0x00B7 | 0x0387 | 0x19DA => true,
          _ => false,
        }
      {
        let normalization = data.normalize(self);
        if normalization.len() > 1 || (normalization.len() == 1 && normalization[0].codepoint() != data.codepoint()) {
          self.id_property_table.add(data.codepoint(), UcdIdProperty::IdContinue);
        }
        for _ in normalization.iter().cloned() {
          self.id_property_table.add(data.codepoint(), UcdIdProperty::IdContinue);
        }
      } else {
        self.id_property_table.add(data.codepoint(), UcdIdProperty::None);
      }
    }
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

  fn serialize_trie(&self) {
    // let trie = HashMap::<HashMap<u32, HashMap<u32, UnicodeData>>>::new();
    // for d in self.unicode_data.iter() {
    //   let cp = d.codepoint();
    //   let k0 = (cp >> 16) & 0xFF;
    //   let k1 = (cp >> 8) & 0xFF;
    //   let k2 = cp & 0xFF;
    //   if let None = trie.get(k0) {
    //     let l2 = HashMap::new();
    //     let l3 = HashMap::new();
    //     l3.insert(k2, cp);
    //     l2.insert(k1, l3.clone());
    //     trie.insert(k0, l2.clone());
    //   } else {
    //     if let Some(l2) = trie.get(k0) {
    //       if let None = l2.get(k1) {
    //         let l3 = HashMap::new();
    //         l3.insert(k2, cp);
    //         l2.insert(k1, l3.clone());
    //       } else {
    //         if let Some(l3) = l2.get(k1) {
    //           l3.insert(k2, cp);
    //         }
    //       }
    //     }
    //   }
    // }
  }

  fn serialize(&mut self) -> std::io::Result<()> {
    const PROLOGUE: &str = "use super::ucd::*;\nuse super::ucd_type::*;\n\n";
    let mut content = {
      let tables = self.general_category_table.serialize(5, 4);
      let table1 = write_to_array!("GENERAL_CATEGORIES_LEVEL1_INDICES", "u32", tables.0, tables.0.iter(), 16);
      let table2 = write_to_array!("GENERAL_CATEGORIES_LEVEL2_INDICES", "u32", tables.1, tables.1.iter(), 16);
      let table3 = write_to_array!("GENERAL_CATEGORIES_LEVEL3_INDICES", "u32", tables.2, tables.2.iter(), 16);
      let values = write_to_array!(
        "GENERAL_CATEGORIES_VALUES",
        "(GeneralCategory, BidiCategory)",
        self.general_category_value_table,
        self
          .general_category_value_table
          .values()
          .map(|(a, b)| format!("(GeneralCategory::{:?}, BidiCategory::{:?})", a, b)),
        6
      );
      format!("{}{}{}{}{}", PROLOGUE, table1, table2, table3, values)
    };
    content = {
      let tables = self.id_property_table.serialize(4, 4);
      let table1 = write_to_array!("ID_PROPERTY_LEVEL1_INDICES", "u32", tables.0, tables.0.iter(), 16);
      let table2 = write_to_array!("ID_PROPERTY_LEVEL2_INDICES", "u32", tables.1, tables.1.iter(), 16);
      let values = write_to_array!(
        "ID_PROPERTY_VALUES",
        "UcdIdProperty",
        tables.2,
        tables.2.iter().map(|p| format!("UcdIdProperty::{:?}", p)),
        6
      );
      format!("{}{}{}{}", content, table1, table2, values)
    };
    return self.write(&content);
  }

  fn write(&self, value: &str) -> std::io::Result<()> {
    let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push("src/unicode/data.rs");
    return fs::write(d, value);
  }
}

fn read_unicode_text(filename: &str) -> String {
  let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  d.push(&format!("tools/{}", filename));
  return fs::read_to_string(d).unwrap();
}

fn main() {
  println!("cargo:rerun-if-changed=build.rs");
  println!("cargo:rerun-if-changed=tools/UnicodeData.txt");
  let raw_unicode_data = read_unicode_text("UnicodeData.txt");
  let raw_composition_exclusion_data = read_unicode_text("DerivedNormalizationProps.txt");
  let raw_prop_list_data = read_unicode_text("PropList.txt");
  let mut table = UnicodeDataTable::new();
  table.read_all_data(&raw_unicode_data, &raw_composition_exclusion_data, &raw_prop_list_data);
  table.collect_id_property();
  table.serialize();
}
