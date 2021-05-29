use super::cell::*;
use super::shadow_class::{ShadowClass, ShadowInstance};
use super::shape::ShapeTag;
use crate::def::*;
use static_assertions;

const NAN_BIT: u64 = 0xfff8000000000000;
const NAN_MASK: u64 = !NAN_BIT;
const UPPER_STRANGERS_MASK: u64 = 0xfff0000000000000;
const UPPER_STRANGERS_MASK_RESULT: u64 = 0x7ff0000000000000;
const LOWER_STRANGERS_MASK: u64 = !0xfff8000000000000;
const INFINITY: u64 = 0x7ff0000000000002;
const HEAP_OBJECT_MASK: u64 = 0xfff8000000000000;

const INVALID_VALUE: f64 = unsafe { std::mem::transmute(NAN_BIT) };
const INFINITY_VALUE: f64 = unsafe { std::mem::transmute(INFINITY) };

const UNDEFINED: u64 = 0x7ff0000000000004;
const NULL: u64 = 0x7ff0000000000008;
const NULL_OR_UNDEFINED: u64 = (UNDEFINED | NULL) & LOWER_STRANGERS_MASK;
const NULL_VALUE: f64 = unsafe { std::mem::transmute(NULL) };
const UNDEFINED_VALUE: f64 = unsafe { std::mem::transmute(UNDEFINED) };

const TRUE: u64 = 0x7ff0000000000010;
const FALSE: u64 = 0x7ff0000000000020;
const TRUE_VALUE: f64 = unsafe { std::mem::transmute(TRUE) };
const FALSE_VALUE: f64 = unsafe { std::mem::transmute(FALSE) };
const BOOLEAN: u64 = (TRUE | FALSE) & LOWER_STRANGERS_MASK;

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
    return Repr(INVALID_VALUE);
  }

  #[inline(always)]
  pub fn js_true() -> Repr {
    return Repr(TRUE_VALUE);
  }

  #[inline(always)]
  pub fn js_false() -> Repr {
    return Repr(FALSE_VALUE);
  }

  #[inline(always)]
  pub fn js_null() -> Repr {
    return Repr(NULL_VALUE);
  }

  #[inline(always)]
  pub fn js_undefined() -> Repr {
    return Repr(UNDEFINED_VALUE);
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
  pub fn is_js_null_or_undefined(&self) -> bool {
    let bits = self.0.to_bits();
    return (bits & UPPER_STRANGERS_MASK) == UPPER_STRANGERS_MASK_RESULT && (self.0.to_bits() & NULL_OR_UNDEFINED) > 1;
  }

  #[inline(always)]
  pub fn is_js_boolean(&self) -> bool {
    let bits = self.0.to_bits();
    return (bits & UPPER_STRANGERS_MASK) == UPPER_STRANGERS_MASK_RESULT && (bits & BOOLEAN) > 1;
  }

  #[inline(always)]
  pub fn is_js_strangers(&self) -> bool {
    let bits = self.0.to_bits();
    return (bits & UPPER_STRANGERS_MASK) == UPPER_STRANGERS_MASK_RESULT && (bits & LOWER_STRANGERS_MASK) > 1;
  }

  #[inline(always)]
  pub fn is_invalid(&self) -> bool {
    return self.0.to_bits() == NAN_BIT;
  }

  #[inline(always)]
  pub fn is_heap_object(&self) -> bool {
    return !self.is_invalid() && (self.0.to_bits() & HEAP_OBJECT_MASK) == HEAP_OBJECT_MASK;
  }

  #[inline(always)]
  pub fn unbox(self) -> Option<Addr> {
    if self.is_heap_object() {
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
    return Repr(INFINITY_VALUE);
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
    if self.is_heap_object() {
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
    return self.is_heap_object() && Cell::from(*self).has_shadow_class();
  }

  #[inline(always)]
  fn is_type_of(&self, shape_tag: ShapeTag) -> bool {
    if !self.is_heap_object() {
      return false;
    }
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
        if obj.is_invalid() {
          return $name($layout::wrap(obj.unbox_unchecked()));
        }
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
        if obj.is_invalid() {
          return $name($layout::wrap(obj.unbox_unchecked()), $($args),*);
        }
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
        if obj.is_invalid() {
          return $name($layout::wrap(obj.unbox_unchecked()));
        }
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

#[cfg(test)]
mod repr_test {
  use super::*;

  #[test]
  fn test_invalid_repr() {
    let inv = Repr::invalid();
    assert_eq!(inv.unbox_unchecked::<u8>(), std::ptr::null_mut());
    assert!(inv.is_boxed());
    assert!(!inv.is_js_strangers());
    assert!(!inv.is_infinity());
    assert!(!inv.is_js_false());
    assert!(!inv.is_js_true());
    assert!(!inv.is_js_null());
    assert!(!inv.is_object());
    assert!(!inv.is_object_type());
    assert!(!inv.is_string());
    assert!(!inv.is_symbol());
    assert!(!inv.is_js_null_or_undefined());
    assert!(!inv.is_js_undefined());
    assert!(inv.is_invalid());
  }

  #[test]
  fn test_js_infinity() {
    let inf = Repr::infinity();
    assert!(inf.is_boxed());
    assert!(inf.is_js_strangers());
    assert!(!inf.is_js_false());
    assert!(!inf.is_js_true());
    assert!(!inf.is_js_null());
    assert!(!inf.is_object());
    assert!(!inf.is_object_type());
    assert!(!inf.is_string());
    assert!(!inf.is_symbol());
    assert!(!inf.is_js_null_or_undefined());
    assert!(!inf.is_js_undefined());
    assert!(!inf.is_invalid());
    assert!(inf.is_infinity());
  }

  #[test]
  fn test_js_undefined() {
    let undef = Repr::js_undefined();
    assert!(undef.is_boxed());
    assert!(undef.is_js_strangers());
    assert!(!undef.is_infinity());
    assert!(!undef.is_invalid());
    assert!(!undef.is_js_false());
    assert!(!undef.is_js_true());
    assert!(!undef.is_js_null());
    assert!(!undef.is_object());
    assert!(!undef.is_object_type());
    assert!(!undef.is_string());
    assert!(!undef.is_symbol());
    assert!(undef.is_js_null_or_undefined());
    assert!(undef.is_js_undefined());
  }

  #[test]
  fn test_js_null() {
    let null = Repr::js_null();
    assert!(null.is_boxed());
    assert!(null.is_js_strangers());
    assert!(!null.is_infinity());
    assert!(!null.is_invalid());
    assert!(!null.is_js_false());
    assert!(!null.is_js_true());
    assert!(!null.is_object());
    assert!(!null.is_object_type());
    assert!(!null.is_string());
    assert!(!null.is_symbol());
    assert!(!null.is_js_undefined());
    assert!(null.is_js_null());
    assert!(null.is_js_null_or_undefined());
  }

  #[test]
  fn test_js_true() {
    let js_true = Repr::js_true();
    assert!(js_true.is_boxed());
    assert!(js_true.is_js_strangers());
    assert!(!js_true.is_infinity());
    assert!(!js_true.is_invalid());
    assert!(!js_true.is_js_false());
    assert!(!js_true.is_object());
    assert!(!js_true.is_object_type());
    assert!(!js_true.is_string());
    assert!(!js_true.is_symbol());
    assert!(!js_true.is_js_undefined());
    assert!(!js_true.is_js_null());
    assert!(!js_true.is_js_null_or_undefined());
    assert!(js_true.is_js_true());
  }

  #[test]
  fn test_js_false() {
    let js_false = Repr::js_false();
    assert!(js_false.is_boxed());
    assert!(js_false.is_js_strangers());
    assert!(!js_false.is_infinity());
    assert!(!js_false.is_invalid());
    assert!(!js_false.is_object());
    assert!(!js_false.is_object_type());
    assert!(!js_false.is_string());
    assert!(!js_false.is_symbol());
    assert!(!js_false.is_js_undefined());
    assert!(!js_false.is_js_null());
    assert!(!js_false.is_js_true());
    assert!(!js_false.is_js_null_or_undefined());
    assert!(js_false.is_js_false());
  }

  struct Test {
    a: u32,
    b: u32,
  }

  #[test]
  fn test_addr() {
    let mut test = Test { a: 20, b: 100 };
    let test_value = Repr::new(&mut test as *mut Test as Addr);
    assert!(!test_value.is_js_strangers());
    assert!(!test_value.is_infinity());
    assert!(!test_value.is_invalid());
    assert!(!test_value.is_js_undefined());
    assert!(!test_value.is_js_null());
    assert!(!test_value.is_js_true());
    assert!(!test_value.is_js_null_or_undefined());
    assert!(!test_value.is_js_false());
    assert!(test_value.is_boxed());
    assert!(test_value.is_heap_object());
    let unbox = test_value.unbox();
    assert!(unbox.is_some());
    assert_eq!(test_value.unbox_unchecked_ref::<Test>().a, 20);
    assert_eq!(test_value.unbox_unchecked_ref::<Test>().b, 100);
  }
}
