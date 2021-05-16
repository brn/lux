use crate::def::*;
use crate::structs::{HeapObject, Repr};
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

pub struct GcHandle<T: Into<Repr> + HeapObject> {
  value: T,
  _phantom_type: PhantomData<T>,
}

impl<T: Into<Repr> + HeapObject> GcHandle<T> {
  pub fn new(value: T) -> GcHandle<T> {
    return GcHandle::<T> {
      value,
      _phantom_type: PhantomData,
    };
  }
}

impl<T: Into<Repr> + HeapObject> Deref for GcHandle<T> {
  type Target = T;
  fn deref(&self) -> &Self::Target {
    return &self.value;
  }
}

impl<T: Into<Repr> + HeapObject> DerefMut for GcHandle<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    return &mut self.value;
  }
}

impl<'a, T: Into<Repr> + HeapObject> Drop for GcHandle<T> {
  fn drop(&mut self) {}
}

#[cfg(test)]
mod gc_handle_test {
  use super::*;

  #[test]
  fn gc_handle_new_test() {}
}
