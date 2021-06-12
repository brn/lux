use crate::structs::{HeapObject, Repr};
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

pub struct Gc<T: HeapObject>(T, PhantomData<T>);

impl<T: HeapObject> Gc<T> {
  pub fn new(value: T) -> Gc<T> {
    return Gc::<T>(value, PhantomData);
  }
}

impl<T: Into<Repr> + HeapObject> Deref for Gc<T> {
  type Target = T;
  fn deref(&self) -> &Self::Target {
    return &self.0;
  }
}

impl<T: Into<Repr> + HeapObject> DerefMut for Gc<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    return &mut self.0;
  }
}

impl<T: HeapObject> Drop for Gc<T> {
  fn drop(&mut self) {
    if !self.0.is_null() {}
  }
}

#[cfg(test)]
mod gc_handle_test {

  #[test]
  fn gc_handle_new_test() {}
}
