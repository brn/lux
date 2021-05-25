use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

#[derive(Copy, Clone, Debug, PartialEq, FromPrimitive)]
pub enum ShapeTag {
  Invalid = 0,
  Context,
  Cell,
  Name,
  ShadowClass,
  SymbolRegistry,
  _JsValueSepBeg,
  Undefined,
  Null,
  Boolean,
  String,
  Symbol,
  Number,
  Object,
  Array,
  _JsValueSepEnd,
  OwnProperties,
  HashMap,
  HashMapEntry,
  PropertyDescriptor,
  InternalArray,
  StringPiece,
  FixedU16Array,
  StringRope,
  FlattenString,
  SmallString,
  OneByteChar,
  Error,
  TypeError,
  GlobalObjects,
  __Sentinel,
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
  pub fn is_js_value(shape: Shape) -> bool {
    return shape.tag() as u8 > ShapeTag::_JsValueSepBeg as u8 && (shape.tag() as u8) < ShapeTag::_JsValueSepEnd as u8;
  }

  pub fn tag(&self) -> ShapeTag {
    return self.tag;
  }

  pub fn from_tag(tag: u8) -> Shape {
    debug_assert!(tag <= ShapeTag::__Sentinel as u8);
    return Shape {
      tag: ShapeTag::from_u8(tag).unwrap(),
    };
  }

  pub const fn invalid() -> Shape {
    return Shape { tag: ShapeTag::Invalid };
  }

  pub const fn context() -> Shape {
    return Shape { tag: ShapeTag::Context };
  }

  pub const fn shadow_class() -> Shape {
    return Shape {
      tag: ShapeTag::ShadowClass,
    };
  }

  pub const fn symbol_registry() -> Shape {
    return Shape {
      tag: ShapeTag::SymbolRegistry,
    };
  }

  pub const fn cell() -> Shape {
    return Shape { tag: ShapeTag::Cell };
  }

  pub const fn name() -> Shape {
    return Shape { tag: ShapeTag::Name };
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

  pub const fn property_descriptor() -> Shape {
    return Shape {
      tag: ShapeTag::PropertyDescriptor,
    };
  }

  pub const fn own_properties() -> Shape {
    return Shape {
      tag: ShapeTag::OwnProperties,
    };
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

  pub const fn string_rope() -> Shape {
    return Shape {
      tag: ShapeTag::StringRope,
    };
  }

  pub const fn flatten_string() -> Shape {
    return Shape {
      tag: ShapeTag::FlattenString,
    };
  }

  pub const fn small_string() -> Shape {
    return Shape {
      tag: ShapeTag::SmallString,
    };
  }

  pub const fn one_byte_char() -> Shape {
    return Shape {
      tag: ShapeTag::OneByteChar,
    };
  }

  pub const fn fixed_u16_array() -> Shape {
    return Shape {
      tag: ShapeTag::FixedU16Array,
    };
  }

  pub const fn error() -> Shape {
    return Shape { tag: ShapeTag::Error };
  }

  pub const fn type_error() -> Shape {
    return Shape {
      tag: ShapeTag::TypeError,
    };
  }

  pub const fn global_objects() -> Shape {
    return Shape {
      tag: ShapeTag::GlobalObjects,
    };
  }

  pub const fn hash_map() -> Shape {
    return Shape { tag: ShapeTag::HashMap };
  }

  pub const fn hash_map_entry() -> Shape {
    return Shape {
      tag: ShapeTag::HashMapEntry,
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
