use super::super::cell::{field_addr, Cell, HeapLayout, HeapObject};
use super::super::repr::*;
use super::super::shape::Shape;
use super::u16_str::FixedU16CodePointArray;
use crate::context::Context;
use crate::def::*;
use crate::impl_heap_object;
use crate::impl_repr_convertion;
use std::mem::size_of;

#[repr(C)]
#[derive(Copy, Clone)]
struct StringPieceBody {
  capacity: usize,
  data: HeapLayout<FixedU16CodePointArray>,
}

#[repr(C)]
#[derive(Copy, Clone)]
struct StringPiece {
  heap: HeapLayout<StringPieceBody>,
}
impl_heap_object!(StringPiece);
impl_repr_convertion!(StringPiece);

const STRING_PIECE_CAPACITY_OFFSET: usize = Cell::SIZE;
const STRING_PIECE_CAPACITY_SIZE: usize = size_of::<usize>();
const STRING_PIECE_DATA_OFFSET: usize = STRING_PIECE_CAPACITY_OFFSET + STRING_PIECE_CAPACITY_SIZE;
const STRING_PIECE_DATA_SIZE: usize = PTR_SIZE;

impl StringPiece {
  const SIZE: usize = Cell::SIZE + STRING_PIECE_CAPACITY_SIZE + STRING_PIECE_DATA_SIZE;

  pub fn new(context: &mut impl Context, str: FixedU16CodePointArray) -> StringPiece {
    let cell = Cell::new(context, StringPiece::SIZE, Shape::string_piece());
    return StringPiece::init(cell, str);
  }

  pub fn wrap(heap: Addr) -> StringPiece {
    return StringPiece {
      heap: HeapLayout::<StringPieceBody>::new(heap),
    };
  }

  pub fn byte_length(&self) -> usize {
    return StringPiece::SIZE;
  }

  pub fn capacity(&self) -> usize {
    return self.heap.as_ref().capacity;
  }

  fn set_capacity(&mut self, size: usize) {
    self.heap.as_ref_mut().capacity = size;
  }

  pub fn split(&self, context: &mut impl Context, index: usize) -> StringPiece {
    let right = self.str().slice(context, 0, index);
    return StringPiece::new(context, right);
  }

  #[inline]
  pub fn str(&self) -> FixedU16CodePointArray {
    return *self.heap.as_ref().data.as_ref();
  }

  #[inline]
  fn init(cell: Cell, str: FixedU16CodePointArray) -> StringPiece {
    let heap = cell.raw_heap();
    let layout = HeapLayout::<StringPieceBody>::new(heap);
    let body = layout.as_ref_mut();
    body.capacity = str.length();
    body.data.set(&str);
    return StringPiece { heap: layout };
  }
}

#[cfg(test)]
mod string_piece_test {
  use super::*;

  use crate::context::testing::MockedContext;
  use crate::context::Context;

  #[test]
  fn string_piece_new_test() {
    let a = "test string value | テスト　文字列";
    let u16_vec = a.encode_utf16().collect::<Vec<_>>();
    let mut mc = MockedContext::new();
    let mut array = FixedU16CodePointArray::new(&mut mc, u16_vec.len() * 2);
    for w in &u16_vec {
      array.push(*w);
    }
    let piece = StringPiece::new(&mut mc, array);
    let first_piece = piece.split(&mut mc, 4);
    let expected = [116, 101, 115, 116];
    let mut i = 0;
    for w in first_piece.str() {
      assert_eq!(w, expected[i]);
      i += 1;
    }
  }
}
