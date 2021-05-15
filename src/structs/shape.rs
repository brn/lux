use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

#[derive(Debug, PartialEq, FromPrimitive)]
enum ShapeTag {
  Cell,
  Undefined,
  Null,
  Boolean,
  String,
  Symbol,
  Number,
  Object,
  Array,
  InternalArray,
  StringPiece,
  FixedU16Array,
}
assert_eq_size!(ShapeTag, u8);

#[derive(Debug)]
pub struct Shape {
  tag: ShapeTag,
}

impl std::cmp::PartialEq for Shape {
  fn eq(&self, other: &Self) -> bool {
    return self.tag == other.tag;
  }
}

impl Shape {
  pub fn from_tag(tag: u8) -> Shape {
    debug_assert!(tag <= ShapeTag::Array as u8);
    return Shape {
      tag: ShapeTag::from_u8(tag).unwrap(),
    };
  }

  pub const fn cell() -> Shape {
    return Shape { tag: ShapeTag::Cell };
  }

  pub const fn undefined() -> Shape {
    return Shape {
      tag: ShapeTag::Undefined,
    };
  }

  pub const fn null() -> Shape {
    return Shape { tag: ShapeTag::Null };
  }

  pub const fn boolean() -> Shape {
    return Shape { tag: ShapeTag::Boolean };
  }

  pub const fn string() -> Shape {
    return Shape { tag: ShapeTag::String };
  }

  pub const fn symbol() -> Shape {
    return Shape { tag: ShapeTag::Symbol };
  }

  pub const fn number() -> Shape {
    return Shape { tag: ShapeTag::Number };
  }

  pub const fn object() -> Shape {
    return Shape { tag: ShapeTag::Object };
  }

  pub const fn array() -> Shape {
    return Shape { tag: ShapeTag::Array };
  }

  pub const fn internal_array() -> Shape {
    return Shape {
      tag: ShapeTag::InternalArray,
    };
  }

  pub const fn string_piece() -> Shape {
    return Shape {
      tag: ShapeTag::StringPiece,
    };
  }

  pub const fn fixed_u16_array() -> Shape {
    return Shape {
      tag: ShapeTag::FixedU16Array,
    };
  }
}

impl Into<u8> for Shape {
  fn into(self) -> u8 {
    return self.tag as u8;
  }
}

impl Into<u64> for Shape {
  fn into(self) -> u64 {
    return self.tag as u64;
  }
}
