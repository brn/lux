use super::super::cell::{Cell, HeapLayout, HeapObject};
use super::super::repr::*;
use super::super::shape::ShapeTag;
use super::backend::StringBackend;
use super::small_string::{OneByteChar, SmallString};
use super::string::JsString;
use super::u16_str::FixedU16CodePointArray;
use crate::context::{AllocationOnlyContext, Context, ObjectRecordsInitializedContext};
use crate::def::*;
use std::char::decode_utf16;
use std::mem::size_of;
use std::ops::Index;
use std::vec::Vec;

#[derive(Clone, Debug)]
pub struct StringPieceIndexOutOfBoundsError {
  bounds: usize,
  index: usize,
}

impl std::fmt::Display for StringPieceIndexOutOfBoundsError {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    return write!(f, "Index out of bound Bounds: {}, index: {}", self.bounds, self.index);
  }
}

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct StringPieceLayout {
  parent: StringPiece,
  length: usize,
  offset: usize,
  data: FixedU16CodePointArray,
  left: StringPiece,
  right: StringPiece,
}

#[repr(C)]
struct StringPiece(HeapLayout<StringPieceLayout>);
impl_object!(StringPiece, HeapLayout<StringPieceLayout>);

impl StringPiece {
  const SIZE: usize = size_of::<StringPieceLayout>();

  pub fn new(context: impl ObjectRecordsInitializedContext, str: FixedU16CodePointArray) -> StringPiece {
    let layout = StringPiece::init(context, str);
    return StringPiece(layout);
  }

  pub fn new_branch(context: impl Context) -> StringPiece {
    let layout = StringPiece::init_branch(context);
    return StringPiece(layout);
  }

  pub fn new_with_offset(context: impl Context, str: FixedU16CodePointArray, offset: usize, length: usize) -> StringPiece {
    let mut layout = StringPiece::init(context, str);
    let body = layout.as_ref_mut();
    body.offset = offset;
    body.length = length;
    return StringPiece(layout);
  }

  #[inline]
  pub fn wrap(heap: Addr) -> StringPiece {
    return StringPiece(HeapLayout::<StringPieceLayout>::wrap(heap));
  }

  #[inline]
  pub fn length(&self) -> usize {
    return self.length;
  }

  pub fn is_branch(&self) -> bool {
    return self.data.is_null();
  }

  pub fn index(&self, index: usize) -> Result<u16, StringPieceIndexOutOfBoundsError> {
    return match self.find(index) {
      Ok((node, index)) => {
        return Ok(node.at_unchecked(index));
      }
      Err(e) => Err(e),
    };
  }

  #[inline]
  pub fn at_unchecked(&self, index: usize) -> u16 {
    debug_assert!(!self.is_branch());
    debug_assert!(self.length() > index);
    return self.data[index];
  }

  #[inline]
  pub fn left(&self) -> Option<StringPiece> {
    return if self.left.is_null() { None } else { Some(self.left) };
  }

  #[inline]
  pub fn right(&self) -> Option<StringPiece> {
    return if self.right.is_null() { None } else { Some(self.right) };
  }

  #[inline]
  pub fn offset(&self) -> usize {
    return self.offset;
  }

  #[inline]
  pub fn str(&self) -> FixedU16CodePointArray {
    return self.data;
  }

  pub fn to_utf8(&self) -> String {
    return decode_utf16(self.data.into_iter()).map(|r| r.unwrap_or('#')).collect::<String>();
  }

  pub fn concat(&self, context: impl Context, piece: StringPiece) -> StringPiece {
    let mut branch = StringPiece::new_branch(context);
    branch.left = *self;
    branch.right = piece;
    branch.length = StringPiece::weight(self);
    return branch;
  }

  pub fn flatten(&self, buffer: &mut FixedU16CodePointArray, start_offset: usize, end_offset: usize) {
    let mut stack = vec![];
    stack.push(*self);
    let mut actual_start_offset = start_offset;
    let mut actual_end_offset = end_offset as isize;
    while stack.len() > 0 {
      match stack.pop() {
        Some(next) => {
          if !next.is_branch() {
            if next.length() > actual_start_offset {
              let str = next.str();
              let is_end_overflow = next.length() < (actual_end_offset as usize);
              if start_offset == 0 && is_end_overflow {
                buffer.append(str);
              } else {
                let end = if is_end_overflow {
                  next.length()
                } else {
                  actual_end_offset as usize
                };
                for i in actual_start_offset..end {
                  buffer.push(str[i]);
                }
              }
              actual_end_offset -= next.length() as isize;
              if actual_end_offset <= 0 {
                return;
              }
              actual_start_offset = 0;
            } else {
              actual_start_offset -= next.length();
              actual_end_offset -= next.length() as isize;
            }
            continue;
          }
          match next.right() {
            Some(right) => {
              stack.push(right);
              match next.left() {
                Some(left) => {
                  stack.push(left);
                }
                _ => {}
              }
            }
            _ => {}
          }
        }
        _ => {
          break;
        }
      }
    }
  }

  #[inline]
  fn str_ptr(&self) -> *mut u16 {
    return self.data.data_mut();
  }

  #[inline]
  fn set_parent(&mut self, parent: StringPiece) {
    self.parent = parent;
  }

  #[inline]
  fn parent(&self) -> StringPiece {
    return self.parent;
  }

  #[inline]
  fn update_length_rec(&mut self, diff: isize) {
    self.update_length(diff);
    if !self.parent.is_null() {
      let mut stack = vec![self.0.parent];
      while stack.len() > 0 {
        let next = stack.pop();
        match next {
          Some(mut parent) => {
            parent.update_length(diff);
          }
          _ => {
            break;
          }
        };
      }
    }
  }

  #[inline]
  fn update_length(&mut self, s: isize) {
    self.length = self.length.wrapping_add(s as usize);
  }

  fn find(&self, index: usize) -> Result<(StringPiece, usize), StringPieceIndexOutOfBoundsError> {
    if !self.is_branch() && self.length() > index {
      return Ok((*self, index));
    }
    let mut stack: Vec<StringPiece> = Vec::new();
    let mut cur_index = index;
    stack.push(*self);
    while stack.len() > 0 {
      let next = stack.pop();
      match next {
        Some(next) => {
          if !next.is_branch() {
            return Ok((next, cur_index));
          }
          match next.left() {
            Some(left) => {
              if next.length() > cur_index {
                stack.push(left);
              } else {
                match next.right() {
                  Some(right) => {
                    stack.push(right);
                    cur_index -= next.length();
                  }
                  _ => {
                    unreachable!();
                  }
                }
              }
            }
            _ => {
              unreachable!();
            }
          };
        }
        None => {
          break;
        }
      };
    }

    return Err(StringPieceIndexOutOfBoundsError {
      bounds: StringPiece::weight(self) + self.length(),
      index,
    });
  }

  fn init(context: impl ObjectRecordsInitializedContext, str: FixedU16CodePointArray) -> HeapLayout<StringPieceLayout> {
    let mut layout = HeapLayout::<StringPieceLayout>::new(context, context.object_records().string_piece_record());
    layout.parent = StringPiece::default();
    layout.length = str.length();
    layout.data = str;
    layout.left = StringPiece::default();
    layout.right = StringPiece::default();
    return layout;
  }

  fn init_branch(context: impl ObjectRecordsInitializedContext) -> HeapLayout<StringPieceLayout> {
    let mut layout = HeapLayout::<StringPieceLayout>::new(context, context.object_records().string_piece_record());
    layout.parent = StringPiece::default();
    layout.length = 0;
    layout.data = FixedU16CodePointArray::default();
    layout.left = StringPiece::default();
    layout.right = StringPiece::default();
    return layout;
  }

  fn weight(this: &StringPiece) -> usize {
    if !this.is_branch() {
      return this.length();
    }
    let mut stack = Vec::new();
    match this.right() {
      Some(right) => stack.push(right),
      _ => {
        unreachable!()
      }
    };
    let mut len = this.length();
    while stack.len() > 0 {
      let next = stack.pop();
      match next {
        Some(next) => {
          if next.is_branch() {
            match next.left() {
              Some(left) => {
                stack.push(left);
              }
              _ => {}
            };
            match next.right() {
              Some(right) => {
                stack.push(right);
              }
              _ => {}
            };
          } else {
            len += next.length();
          }
        }
        _ => {
          break;
        }
      }
    }

    return len;
  }
}

impl Index<usize> for StringPiece {
  type Output = u16;
  fn index(&self, index: usize) -> &Self::Output {
    return unsafe { &*self.str_ptr().offset((index + self.offset()) as isize) };
  }
}

pub struct StringPieceIterator {
  piece: StringPiece,
  index: usize,
}

impl Iterator for StringPieceIterator {
  type Item = u16;
  fn next(&mut self) -> Option<Self::Item> {
    if self.index >= self.piece.length() {
      return None;
    }
    let result = self.piece.str()[self.index + self.piece.offset()];
    self.index += 1;
    return Some(result);
  }
}

impl IntoIterator for StringPiece {
  type Item = u16;
  type IntoIter = StringPieceIterator;

  fn into_iter(self) -> Self::IntoIter {
    return StringPieceIterator { piece: self, index: 0 };
  }
}

#[cfg(test)]
mod string_piece_test {
  use super::*;

  use crate::context::Context;
  use crate::context::LuxContext;

  fn new_string_piece(context: impl Context, s: &str) -> (StringPiece, Vec<u16>) {
    let u16_vec = s.encode_utf16().collect::<Vec<_>>();
    let mut array = FixedU16CodePointArray::new(context, u16_vec.len() * 2);
    for w in &u16_vec {
      array.push(*w);
    }
    return (StringPiece::new(context, array), u16_vec);
  }

  #[test]
  fn string_piece_concat_test() {
    let mut mc = LuxContext::new();
    let a = "test string value";
    let b = "テスト　文字列";
    let c = "lorem ipsum";
    let d = "quick brown fox";
    let (piece_1, a_vec) = new_string_piece(mc, a);
    let (piece_2, b_vec) = new_string_piece(mc, b);
    let (piece_3, c_vec) = new_string_piece(mc, c);
    let (piece_4, _) = new_string_piece(mc, d);

    let new_piece_1 = piece_1.concat(mc, piece_2);
    let new_piece_2 = new_piece_1.concat(mc, piece_3);
    let new_piece_3 = new_piece_2.concat(mc, piece_4);
    assert_eq!(new_piece_1.is_branch(), true);
    assert_eq!(new_piece_1.length(), a_vec.len());
    assert_eq!(new_piece_2.is_branch(), true);
    assert_eq!(new_piece_2.length(), a_vec.len() + b_vec.len());
    assert_eq!(new_piece_3.is_branch(), true);
    assert_eq!(new_piece_3.length(), a_vec.len() + b_vec.len() + c_vec.len());
  }

  #[test]
  fn string_piece_at_test() {
    let mut mc = LuxContext::new();
    let a = "test string value";
    let b = "テスト　文字列";
    let c = "lorem ipsum";
    let d = "quick brown fox";
    let (piece_1, a_vec) = new_string_piece(mc, a);
    let (piece_2, b_vec) = new_string_piece(mc, b);
    let (piece_3, c_vec) = new_string_piece(mc, c);
    let (piece_4, d_vec) = new_string_piece(mc, d);

    //                []
    //        []
    //   []       [c]     [d]
    //[a]  [b]
    //
    let new_piece = piece_1.concat(mc, piece_2).concat(mc, piece_3).concat(mc, piece_4);
    assert_eq!(new_piece.is_branch(), true);
    for i in 0..(a_vec.len()) {
      assert_eq!(new_piece.index(i).unwrap(), a_vec[i]);
    }
    let mut start = a_vec.len();
    for i in start..(a_vec.len() + b_vec.len()) {
      assert_eq!(new_piece.index(i).unwrap(), b_vec[i - start]);
    }
    start = a_vec.len() + b_vec.len();
    for i in start..(a_vec.len() + b_vec.len() + c_vec.len()) {
      assert_eq!(new_piece.index(i).unwrap(), c_vec[i - start]);
    }
    start = a_vec.len() + b_vec.len() + c_vec.len();
    for i in start..(a_vec.len() + b_vec.len() + c_vec.len() + d_vec.len()) {
      assert_eq!(new_piece.index(i).unwrap(), d_vec[i - start]);
    }
  }

  #[test]
  fn string_flatten_test() {
    let mut mc = LuxContext::new();
    let a = "test string value";
    let b = "テスト　文字列";
    let c = "lorem ipsum";
    let d = "quick brown fox";
    let (piece_1, mut a_vec) = new_string_piece(mc, a);
    let (piece_2, mut b_vec) = new_string_piece(mc, b);
    let (piece_3, mut c_vec) = new_string_piece(mc, c);
    let (piece_4, mut d_vec) = new_string_piece(mc, d);
    let size = a_vec.len() + b_vec.len() + c_vec.len() + d_vec.len();
    let mut u16_vec = Vec::with_capacity(size);
    u16_vec.append(&mut a_vec);
    u16_vec.append(&mut b_vec);
    u16_vec.append(&mut c_vec);
    u16_vec.append(&mut d_vec);

    //                []
    //        []
    //   []       [c]     [d]
    //[a]  [b]
    //
    let new_piece = piece_1.concat(mc, piece_2).concat(mc, piece_3).concat(mc, piece_4);
    let mut buffer = FixedU16CodePointArray::new(mc, size);
    new_piece.flatten(&mut buffer, 0, size);
    for (i, w) in buffer.into_iter().enumerate() {
      assert_eq!(w, u16_vec[i]);
    }
  }

  #[test]
  fn string_flatten_slice_test() {
    let mut mc = LuxContext::new();
    let a = "test string value";
    let b = "テスト　文字列";
    let c = "lorem ipsum";
    let d = "quick brown fox";
    let (piece_1, mut a_vec) = new_string_piece(mc, a);
    let (piece_2, mut b_vec) = new_string_piece(mc, b);
    let (piece_3, mut c_vec) = new_string_piece(mc, c);
    let (piece_4, mut d_vec) = new_string_piece(mc, d);
    let size = a_vec.len() + b_vec.len() + c_vec.len() + d_vec.len();
    let mut u16_vec = Vec::with_capacity(size);
    u16_vec.append(&mut a_vec);
    u16_vec.append(&mut b_vec);
    u16_vec.append(&mut c_vec);
    u16_vec.append(&mut d_vec);

    //                []
    //        []
    //   []       [c]     [d]
    //[a]  [b]
    //
    let new_piece = piece_1.concat(mc, piece_2).concat(mc, piece_3).concat(mc, piece_4);
    let start = 18;
    let end = size - 14;
    let mut buffer = FixedU16CodePointArray::new(mc, end - start);
    new_piece.flatten(&mut buffer, start, end);
    for (i, w) in buffer.into_iter().enumerate() {
      assert_eq!(w, u16_vec[i + start]);
    }
  }
}

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct FlattenStringLayout {
  str: FixedU16CodePointArray,
}

#[repr(C)]
pub struct FlattenString(HeapLayout<FlattenStringLayout>);
impl_object!(FlattenString, HeapLayout<FlattenStringLayout>);

impl FlattenString {
  pub const SIZE: usize = size_of::<FlattenStringLayout>();

  pub fn new(context: impl ObjectRecordsInitializedContext, str: FixedU16CodePointArray) -> FlattenString {
    let mut layout = HeapLayout::<FlattenStringLayout>::new(context, context.object_records().flatten_string_record());
    layout.str = str;
    return FlattenString(layout);
  }

  pub fn str(&self) -> FixedU16CodePointArray {
    return self.str;
  }
}

impl StringBackend for FlattenString {
  fn at(&self, index: usize) -> Option<u16> {
    if index < self.str().length() {
      return Some(self.str[index]);
    }
    return None;
  }

  fn concat(&self, context: impl Context, repr: Repr) -> JsString {
    let left = match Cell::from(repr).shape().tag() {
      ShapeTag::SmallString => StringPiece::new(context, SmallString::from(repr).str()),
      ShapeTag::OneByteChar => {
        let array = fixed_array!(type: u16, context: context, capacity: 1, OneByteChar::from(repr).at(0).unwrap());
        StringPiece::new(context, array)
      }
      ShapeTag::FlattenString => StringPiece::new(context, FlattenString::from(repr).str()),
      ShapeTag::StringRope => StringRope::from(repr).piece(),
      _ => unreachable!(),
    };
    let right = StringPiece::new(context, self.str());
    let new_piece = left.concat(context, right);
    return JsString::from(StringRope::from_piece(context, new_piece));
  }

  fn slice(&self, context: impl Context, start_index: usize, end_index: usize) -> FixedU16CodePointArray {
    return self.str.slice(context, start_index, end_index);
  }

  fn flatten(&mut self, context: impl AllocationOnlyContext) -> FixedU16CodePointArray {
    return self.str;
  }

  fn len(&self) -> usize {
    return self.str.length();
  }
}

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct StringRopeLayout {
  len: usize,
  piece: StringPiece,
}

#[repr(C)]
pub struct StringRope(HeapLayout<StringRopeLayout>);
impl_object!(StringRope, HeapLayout<StringRopeLayout>);

impl StringRope {
  pub const SIZE: usize = size_of::<StringRopeLayout>();
  pub const PIECE_SIZE: usize = StringPiece::SIZE;

  pub fn new(context: impl ObjectRecordsInitializedContext, str: FixedU16CodePointArray) -> StringRope {
    let mut layout = HeapLayout::<StringRopeLayout>::new(context, context.object_records().string_rope_record());
    let piece = StringPiece::new(context, str);
    layout.len = str.length();
    layout.piece = piece;
    return StringRope(layout);
  }

  fn from_piece(context: impl Context, str: StringPiece) -> StringRope {
    let mut layout = HeapLayout::<StringRopeLayout>::new(context, context.object_records().string_rope_record());
    layout.piece = str;
    return StringRope(layout);
  }

  fn piece(&self) -> StringPiece {
    return self.piece;
  }
}

impl StringBackend for StringRope {
  fn at(&self, index: usize) -> Option<u16> {
    return match self.piece.index(index) {
      Ok(w) => Some(w),
      _ => None,
    };
  }

  fn concat(&self, context: impl Context, repr: Repr) -> JsString {
    let new_piece = match Cell::from(repr).shape().tag() {
      ShapeTag::SmallString => StringPiece::new(context, SmallString::from(repr).str()),
      ShapeTag::OneByteChar => {
        let array = fixed_array!(type: u16, context: context, capacity: 1, OneByteChar::from(repr).at(0).unwrap());
        StringPiece::new(context, array)
      }
      ShapeTag::FlattenString => StringPiece::new(context, FlattenString::from(repr).str()),
      ShapeTag::StringRope => StringRope::from(repr).piece(),
      _ => unreachable!(),
    };
    return JsString::from(StringRope::from_piece(context, new_piece));
  }

  fn slice(&self, context: impl Context, start_index: usize, end_index: usize) -> FixedU16CodePointArray {
    let len = end_index - start_index;
    let mut fixed_array = FixedU16CodePointArray::new(context, if self.len > len { len } else { self.len });
    self.piece.flatten(&mut fixed_array, start_index, end_index);
    return fixed_array;
  }

  fn flatten(&mut self, context: impl ObjectRecordsInitializedContext) -> FixedU16CodePointArray {
    let mut fixed_array = FixedU16CodePointArray::new(context, self.len);
    self.piece.flatten(&mut fixed_array, 0, self.len);
    return fixed_array;
  }

  fn len(&self) -> usize {
    return self.len;
  }
}
