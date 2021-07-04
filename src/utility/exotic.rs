use std::marker::PhantomData;
use std::mem::transmute;

pub struct Exotic<T>(usize, PhantomData<T>);

impl<T> std::ops::Deref for Exotic<T> {
  type Target = T;
  fn deref(&self) -> &Self::Target {
    return unsafe { transmute::<usize, *mut T>(self.0 & !1_usize).as_ref().unwrap() };
  }
}

impl<T> std::ops::DerefMut for Exotic<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    return unsafe { transmute::<usize, *mut T>(self.0 & !1_usize).as_mut().unwrap() };
  }
}

impl<T> AsRef<T> for Exotic<T> {
  fn as_ref(&self) -> &T {
    return unsafe { transmute::<usize, *mut T>(self.0 & !1_usize).as_ref().unwrap() };
  }
}

impl<T> AsMut<T> for Exotic<T> {
  fn as_mut(&mut self) -> &mut T {
    return unsafe { transmute::<usize, *mut T>(self.0 & !1_usize).as_mut().unwrap() };
  }
}

impl<T> Exotic<T> {
  pub fn new(ptr: *mut T) -> Exotic<T> {
    return Exotic(ptr as usize, PhantomData);
  }

  pub fn to_box(&self) -> Result<Box<T>, &'static str> {
    if self.0 & 1 == 1 {
      return unsafe { Ok(Box::from_raw((self.0 & !1_usize) as *mut T)) };
    }
    return Err("Value that stored in Exotic was not from the Box.");
  }

  fn from_box(ptr: *mut T) -> Exotic<T> {
    return Exotic((ptr as usize) | 1, PhantomData);
  }
}

impl<T> std::ops::Drop for Exotic<T> {
  fn drop(&mut self) {
    let _ = self.to_box();
  }
}

impl<T> From<Box<T>> for Exotic<T> {
  fn from(a: Box<T>) -> Exotic<T> {
    let l = Box::leak(a);
    return Exotic::from_box(l);
  }
}
