use super::super::cell::{BareHeapLayout, Cell, HeapLayout, HeapObject};
use super::super::repr::*;
use super::super::shape::Shape;
use super::u16_str::FixedU16CodePointArray;
use crate::context::Context;
use crate::def::*;
use std::mem::size_of;
use std::ops::{Index, IndexMut};

#[repr(C)]
#[derive(Copy, Clone)]
struct StringPieceBody {
  length: usize,
  offset: usize,
  data: Addr,
  left: Addr,
  right: Addr,
}

#[repr(transparent)]
#[derive(Copy, Clone)]
struct StringPiece(HeapLayout<StringPieceBody>);
impl_heap_object!(StringPiece);
impl_repr_convertion!(StringPiece);

impl StringPiece {
  const SIZE: usize = Cell::SIZE + size_of::<StringPieceBody>();

  pub fn new(context: &mut impl Context, str: FixedU16CodePointArray) -> StringPiece {
    let layout = StringPiece::init(context, str);
    return StringPiece(layout);
  }

  pub fn new_with_offset(
    context: &mut impl Context,
    str: FixedU16CodePointArray,
    offset: usize,
    length: usize,
  ) -> StringPiece {
    let mut layout = StringPiece::init(context, str);
    let body = layout.as_ref_mut();
    body.offset = offset;
    body.length = length;
    return StringPiece(layout);
  }

  #[inline]
  pub fn wrap(heap: Addr) -> StringPiece {
    return StringPiece(HeapLayout::<StringPieceBody>::wrap(heap));
  }

  #[inline]
  pub fn length(&self) -> usize {
    return self.0.as_ref().length;
  }

  pub fn split(&mut self, context: &mut impl Context, index: usize) {
    let length = self.length();
    let str = self.str();
    let body = self.0.as_ref_mut();
    body.left = StringPiece::new_with_offset(context, str, 0, index).raw_heap();
    body.right = StringPiece::new_with_offset(context, str, index, length - index).raw_heap();
  }

  #[inline]
  pub fn left(&self) -> Option<StringPiece> {
    return if self.0.as_ref().left.is_null() {
      None
    } else {
      Some(StringPiece::wrap(self.0.as_ref().left))
    };
  }

  #[inline]
  pub fn right(&self) -> Option<StringPiece> {
    return if self.0.as_ref().right.is_null() {
      None
    } else {
      Some(StringPiece::wrap(self.0.as_ref().right))
    };
  }

  #[inline]
  pub fn offset(&self) -> usize {
    return self.0.as_ref().offset;
  }

  #[inline]
  pub fn str(&self) -> FixedU16CodePointArray {
    return FixedU16CodePointArray::wrap(self.0.as_ref().data);
  }

  #[inline]
  fn str_ptr(&self) -> *mut u16 {
    return FixedU16CodePointArray::wrap(self.0.as_ref().data).data();
  }

  fn init(context: &mut impl Context, str: FixedU16CodePointArray) -> HeapLayout<StringPieceBody> {
    let mut layout = HeapLayout::<StringPieceBody>::new(context, StringPiece::SIZE, Shape::string_piece());
    let body = layout.as_ref_mut();
    body.length = str.length();
    body.data = str.raw_heap();
    body.left = std::ptr::null_mut();
    body.right = std::ptr::null_mut();
    return layout;
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

  use crate::context::testing::MockedContext;
  use crate::context::Context;

  #[test]
  fn string_piece_split_test() {
    let a = "test string value | テスト　文字列";
    let u16_vec = a.encode_utf16().collect::<Vec<_>>();
    let mut mc = MockedContext::new();
    let mut array = FixedU16CodePointArray::new(&mut mc, u16_vec.len() * 2);
    for w in &u16_vec {
      array.push(*w);
    }
    let mut piece = StringPiece::new(&mut mc, array);
    piece.split(&mut mc, 4);
    assert!(piece.left().is_some());
    assert!(piece.right().is_some());
    let left_piece = piece.left().unwrap();
    let right_piece = piece.right().unwrap();
    let left_expected = [116, 101, 115, 116];
    let right_expected = [
      32, 115, 116, 114, 105, 110, 103, 32, 118, 97, 108, 117, 101, 32, 124, 32, 12486, 12473, 12488, 12288, 25991,
      23383, 21015,
    ];
    let mut i = 0;
    for w in left_piece {
      assert_eq!(w, left_expected[i]);
      i += 1;
    }
    i = 0;
    for w in right_piece {
      assert_eq!(w, right_expected[i]);
      i += 1;
    }
    println!("");
  }
}

#[repr(C)]
#[derive(Copy, Clone)]
struct StringRopeBody {
  tree: Addr,
}

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct StringRope(HeapLayout<StringRopeBody>);
impl_heap_object!(StringRope);
impl_repr_convertion!(StringRope);

impl StringRope {
  const SIZE: usize = Cell::SIZE + size_of::<StringRopeBody>();

  pub fn new(context: &mut impl Context, str: FixedU16CodePointArray) -> StringRope {
    let mut layout = HeapLayout::<StringRopeBody>::new(context, StringRope::SIZE, Shape::string_rope());
    layout.as_ref_mut().tree = StringPiece::new(context, str).raw_heap();
    return StringRope(layout);
  }

  fn construct(context: &mut impl Context, str: StringPiece) -> StringRope {
    let mut layout = HeapLayout::<StringRopeBody>::new(context, StringRope::SIZE, Shape::string_rope());
    layout.as_ref_mut().tree = str.raw_heap();
    return StringRope(layout);
  }

  pub fn split(&mut self, context: &mut impl Context, index: usize) -> (StringRope, StringRope) {
    let mut piece = StringPiece::wrap(self.0.as_ref().tree);
    piece.split(context, index);
    return (
      StringRope::construct(context, piece.left().unwrap()),
      StringRope::construct(context, piece.right().unwrap()),
    );
  }

  fn wrap(heap: Addr) -> StringRope {
    return StringRope(HeapLayout::wrap(heap));
  }
}
