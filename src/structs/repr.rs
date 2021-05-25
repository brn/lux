use crate::def::*;

const NAN_BIT: u64 = 0x7ff8000000000000;
const NAN_MASK: u64 = !NAN_BIT;
const INFINITY: u64 = 0x7ff0000000000002;
const UNDEFINED: u64 = 0x7ff0000000000003;
const NULL: u64 = 0x7ff0000000000004;
const TRUE: u64 = 0x7ff0000000000005;
const FALSE: u64 = 0x7ff0000000000006;

#[derive(Copy, Clone)]
pub struct Repr(f64);

impl Repr {
  pub fn new(heap: Addr) -> Repr {
    return Repr(f64::from_bits(heap as u64 | NAN_BIT));
  }

  pub fn from_nan() -> Repr {
    return Repr(f64::NAN);
  }

  pub fn invalid() -> Repr {
    return Repr(f64::from_bits(0x7ff8000000000000));
  }

  pub fn js_true() -> Repr {
    return Repr(f64::from_bits(TRUE));
  }

  pub fn js_false() -> Repr {
    return Repr(f64::from_bits(FALSE));
  }

  pub fn js_null() -> Repr {
    return Repr(f64::from_bits(NULL));
  }

  pub fn js_undefined() -> Repr {
    return Repr(f64::from_bits(UNDEFINED));
  }

  pub fn is_js_true(&self) -> bool {
    return self.0.to_bits() == TRUE;
  }

  pub fn is_js_false(&self) -> bool {
    return self.0.to_bits() == FALSE;
  }

  pub fn is_js_null(&self) -> bool {
    return self.0.to_bits() == NULL;
  }

  pub fn is_js_undefined(&self) -> bool {
    return self.0.to_bits() == UNDEFINED;
  }

  pub fn is_invalid(&self) -> bool {
    return self.0.to_bits() == NAN_BIT;
  }

  pub fn unbox(self) -> Option<Addr> {
    if self.is_boxed() {
      return Some((self.0.to_bits() & NAN_MASK) as Addr);
    };
    return None;
  }

  pub fn unbox_unchecked<T>(&self) -> *mut T {
    return (self.0.to_bits() & NAN_MASK) as *mut T;
  }

  pub fn unbox_unchecked_ref<T>(&mut self) -> &T {
    return unsafe { &*((self.0.to_bits() & NAN_MASK) as *mut T) };
  }

  pub fn unbox_unchecked_ref_mut<T>(&mut self) -> &mut T {
    return unsafe { &mut *((self.0.to_bits() & NAN_MASK) as *mut T) };
  }

  pub fn is_boxed(&self) -> bool {
    return self.0.is_nan();
  }

  pub fn to_number(&self) -> Option<f64> {
    if self.is_boxed() {
      return None;
    }
    return Some(self.0);
  }

  pub fn to_number_unchecked(&self) -> f64 {
    return self.0;
  }

  pub fn infinity() -> Repr {
    return Repr(f64::from_bits(INFINITY));
  }

  pub fn is_infinity(&self) -> bool {
    return self.0.to_bits() == INFINITY;
  }

  pub fn nan() -> Repr {
    return Repr(f64::NAN);
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

impl From<f64> for Repr {
  fn from(a: f64) -> Repr {
    return Repr(a);
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

impl Into<Addr> for Repr {
  fn into(self) -> Addr {
    return self.unbox().unwrap();
  }
}
