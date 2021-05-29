use super::cell::*;
use super::shadow_class::{ShadowClass, ShadowInstance};
use super::shape::ShapeTag;
use crate::def::*;

const NAN_BIT: u64 = 0x7ff8000000000000;
const NAN_MASK: u64 = !NAN_BIT;
const INFINITY: u64 = 0x7ff0000000000002;
const UNDEFINED: u64 = 0x7ff0000000000003;
const NULL: u64 = 0x7ff0000000000004;
const TRUE: u64 = 0x7ff0000000000005;
const FALSE: u64 = 0x7ff0000000000006;
const MAX_SAFE_INTEGER: u64 = 0x1fffffffffffff;

#[derive(Copy, Clone)]
pub struct Repr(f64);

impl Repr {
  #[inline(always)]
  pub fn new(heap: Addr) -> Repr {
    return Repr(f64::from_bits(heap as u64 | NAN_BIT));
  }

  #[inline(always)]
  pub fn from_nan() -> Repr {
    return Repr(f64::NAN);
  }

  #[inline(always)]
  pub fn invalid() -> Repr {
    return Repr(f64::from_bits(0x7ff8000000000000));
  }

  #[inline(always)]
  pub fn js_true() -> Repr {
    return Repr(f64::from_bits(TRUE));
  }

  #[inline(always)]
  pub fn js_false() -> Repr {
    return Repr(f64::from_bits(FALSE));
  }

  #[inline(always)]
  pub fn js_null() -> Repr {
    return Repr(f64::from_bits(NULL));
  }

  #[inline(always)]
  pub fn js_undefined() -> Repr {
    return Repr(f64::from_bits(UNDEFINED));
  }

  #[inline(always)]
  pub fn is_js_true(&self) -> bool {
    return self.0.to_bits() == TRUE;
  }

  #[inline(always)]
  pub fn is_js_false(&self) -> bool {
    return self.0.to_bits() == FALSE;
  }

  #[inline(always)]
  pub fn is_js_null(&self) -> bool {
    return self.0.to_bits() == NULL;
  }

  #[inline(always)]
  pub fn is_js_undefined(&self) -> bool {
    return self.0.to_bits() == UNDEFINED;
  }

  #[inline(always)]
  pub fn is_js_undefined_or_null(&self) -> bool {
    return self.is_js_undefined() || self.is_js_null();
  }

  #[inline(always)]
  pub fn is_invalid(&self) -> bool {
    return self.0.to_bits() == NAN_BIT;
  }

  #[inline(always)]
  pub fn unbox(self) -> Option<Addr> {
    if self.is_boxed() {
      return Some((self.0.to_bits() & NAN_MASK) as Addr);
    };
    return None;
  }

  #[inline(always)]
  pub fn unbox_unchecked<T>(&self) -> *mut T {
    return (self.0.to_bits() & NAN_MASK) as *mut T;
  }

  #[inline(always)]
  pub fn unbox_unchecked_ref<T>(&self) -> &T {
    return unsafe { &*((self.0.to_bits() & NAN_MASK) as *mut T) };
  }

  #[inline(always)]
  pub fn unbox_unchecked_ref_mut<T>(&mut self) -> &mut T {
    return unsafe { &mut *((self.0.to_bits() & NAN_MASK) as *mut T) };
  }

  #[inline]
  pub fn into_cell_unchecked(&self) -> Cell {
    return Cell::from(self.unbox_unchecked());
  }

  #[inline(always)]
  pub fn is_boxed(&self) -> bool {
    return self.0.is_nan();
  }

  #[inline(always)]
  pub fn to_number(&self) -> Option<f64> {
    if self.is_boxed() {
      return None;
    }
    return Some(self.0);
  }

  #[inline(always)]
  pub fn to_number_unchecked(&self) -> f64 {
    return self.0;
  }

  #[inline(always)]
  pub fn infinity() -> Repr {
    return Repr(f64::from_bits(INFINITY));
  }

  #[inline(always)]
  pub fn is_infinity(&self) -> bool {
    return self.0.to_bits() == INFINITY;
  }

  #[inline(always)]
  pub fn nan() -> Repr {
    return Repr(f64::NAN);
  }

  #[inline]
  pub fn is_object_type(&self) -> bool {
    if self.is_boxed() {
      match Cell::from(self.unbox_unchecked()).shape().tag() {
        ShapeTag::Object
        | ShapeTag::String
        | ShapeTag::Array
        | ShapeTag::Boolean
        | ShapeTag::Number
        | ShapeTag::Symbol => return true,
        _ => return false,
      }
    }
    return false;
  }

  #[inline(always)]
  pub fn is_symbol(&self) -> bool {
    return self.is_type_of(ShapeTag::Symbol);
  }

  #[inline(always)]
  pub fn is_string(&self) -> bool {
    return self.is_type_of(ShapeTag::String);
  }

  #[inline(always)]
  pub fn is_object(&self) -> bool {
    return self.is_boxed() && Cell::from(*self).has_shadow_class();
  }

  #[inline(always)]
  fn is_type_of(&self, shape_tag: ShapeTag) -> bool {
    match self.unbox() {
      Some(addr) => return Cell::from(addr).shape().tag() == shape_tag,
      _ => return false,
    };
  }
}

impl Default for Repr {
  fn default() -> Repr {
    return Repr::invalid();
  }
}

impl ShadowInstance for Repr {
  fn class(&self) -> ShadowClass {
    assert!(self.is_object());
    return Cell::from(*self).class();
  }
}

pub trait FromUnchecked<T> {
  fn from_unchecked(a: T) -> Self;
}

macro_rules! impl_repr_convertion {
  ($name:ident, $layout:ident) => {
    impl From<$name> for crate::structs::Repr {
      fn from(obj: $name) -> crate::structs::Repr {
        return crate::structs::Repr::new(obj.raw_heap());
      }
    }
    impl From<crate::structs::Repr> for $name {
      fn from(obj: crate::structs::Repr) -> $name {
        return $name($layout::wrap(obj.unbox().unwrap()));
      }
    }
    impl crate::structs::FromUnchecked<crate::structs::Repr> for $name {
      fn from_unchecked(obj: crate::structs::Repr) -> $name {
        return $name($layout::wrap(obj.unbox_unchecked()));
      }
    }
  };
  ($name:ident<$($type:tt : $bound:ident),+$(,)?>, $layout:ident, $($args:expr),+) => {
    impl<$($type : $bound,)+> From<$name<$($type,)+>> for crate::structs::Repr {
      fn from(obj: $name<$($type,)+>) -> crate::structs::Repr {
        return crate::structs::Repr::new(obj.raw_heap());
      }
    }
    impl<$($type : $bound,)+> From<crate::structs::Repr> for $name<$($type,)+> {
      fn from(obj: Repr) -> $name<$($type,)+> {
        return $name($layout::wrap(obj.unbox().unwrap()), $($args),*);
      }
    }
    impl<$($type : $bound,)+> crate::structs::FromUnchecked<crate::structs::Repr> for $name<$($type,)+> {
      fn from_unchecked(obj: Repr) -> $name<$($type,)+> {
        return $name($layout::wrap(obj.unbox_unchecked()), $($args),*);
      }
    }
  };
  ($name:ident<$($type:tt : $bound:ident),+$(,)?>, $layout:ident) => {
    impl<$($type : $bound,)+> From<$name<$($type,)+>> for crate::structs::Repr {
      fn from(obj: $name<$($type,)+>) -> crate::structs::Repr {
        return crate::structs::Repr::new(obj.raw_heap());
      }
    }
    impl<$($type : $bound,)+> From<crate::structs::Repr> for $name<$($type,)+> {
      fn from(obj: Repr) -> $name<$($type,)+> {
        return $name($layout::wrap(obj.unbox().unwrap()));
      }
    }
    impl<$($type : $bound,)+> crate::structs::FromUnchecked<crate::structs::Repr> for $name<$($type,)+> {
      fn from_unchecked(obj: Repr) -> $name<$($type,)+> {
        return $name($layout::wrap(obj.unbox_unchecked()));
      }
    }
  }
}

impl From<Repr> for u64 {
  fn from(r: Repr) -> u64 {
    return r.to_number().unwrap() as u64;
  }
}

impl From<u64> for Repr {
  fn from(r: u64) -> Repr {
    return Repr(r as f64);
  }
}

impl From<f64> for Repr {
  fn from(a: f64) -> Repr {
    return Repr(a);
  }
}

impl From<usize> for Repr {
  fn from(a: usize) -> Repr {
    return Repr(a as f64);
  }
}

impl From<Repr> for usize {
  fn from(a: Repr) -> usize {
    return a.to_number().unwrap() as usize;
  }
}

impl Into<f64> for Repr {
  fn into(self) -> f64 {
    return self.to_number().unwrap();
  }
}

impl From<Addr> for Repr {
  fn from(addr: Addr) -> Repr {
    return Repr::new(addr);
  }
}

impl From<Repr> for Addr {
  fn from(r: Repr) -> Addr {
    return r.unbox().unwrap();
  }
}
