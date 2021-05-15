use crate::def::*;

const NAN_BIT: u64 = 0xFFFE00000000;
const NAN_MASK: u64 = !NAN_BIT;

pub struct Repr(f64);

impl Repr {
  pub fn new(heap: Addr) -> Repr {
    return Repr(f64::from_bits(heap as u64 | NAN_BIT));
  }
  pub fn unbox<T>(self) -> Option<*mut T> {
    if self.is_boxed() {
      return Some((self.0.to_bits() & NAN_MASK) as *mut T);
    }
    return None;
  }

  pub fn unbox_unchecked<T>(&self) -> *mut T {
    return (self.0.to_bits() & NAN_MASK) as *mut T;
  }

  pub fn is_boxed(&self) -> bool {
    return self.0 == f64::NAN;
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
}

#[macro_export]
macro_rules! impl_repr_convertion {
  ($name:ident) => {
    impl From<$name> for Repr {
      fn from(obj: $name) -> Repr {
        return Repr::new(obj.heap);
      }
    }
  };
  ($name:ty) => {
    fn from(obj: $name) -> Repr {
      return Repr::new(obj.heap);
    }
  };
}
