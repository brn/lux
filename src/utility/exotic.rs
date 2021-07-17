use std::marker::PhantomData;
use std::mem::transmute;

pub struct Exotic<T>(usize, PhantomData<T>);
impl<T> Copy for Exotic<T> {}
impl<T> Clone for Exotic<T> {
  fn clone(&self) -> Self {
    *self
  }
}

impl<T> std::ops::Deref for Exotic<T> {
  type Target = T;
  #[inline(always)]
  fn deref(&self) -> &Self::Target {
    return unsafe { transmute::<usize, *mut T>(self.0 & !1_usize).as_ref().unwrap() };
  }
}

impl<T> std::ops::DerefMut for Exotic<T> {
  #[inline(always)]
  fn deref_mut(&mut self) -> &mut Self::Target {
    return unsafe { transmute::<usize, *mut T>(self.0 & !1_usize).as_mut().unwrap() };
  }
}

impl<T> AsRef<T> for Exotic<T> {
  #[inline(always)]
  fn as_ref(&self) -> &T {
    return unsafe { transmute::<usize, *mut T>(self.0 & !1_usize).as_ref().unwrap() };
  }
}

impl<T> AsMut<T> for Exotic<T> {
  #[inline(always)]
  fn as_mut(&mut self) -> &mut T {
    return unsafe { transmute::<usize, *mut T>(self.0 & !1_usize).as_mut().unwrap() };
  }
}

impl<T> Exotic<T> {
  #[inline]
  pub fn new(ptr: *mut T) -> Exotic<T> {
    return Exotic(ptr as usize, PhantomData);
  }

  #[inline]
  pub fn to_box(&self) -> Result<Box<T>, &'static str> {
    if self.0 & 1 == 1 {
      return unsafe { Ok(Box::from_raw((self.0 & !1_usize) as *mut T)) };
    }
    return Err("Value that stored in Exotic was not from the Box.");
  }

  #[inline(always)]
  fn from_box(b: Box<T>) -> Exotic<T> {
    return Exotic(unsafe { transmute::<*mut T, usize>(Box::leak(b)) } | 1, PhantomData);
  }
}

impl<T> From<Box<T>> for Exotic<T> {
  #[inline(always)]
  fn from(a: Box<T>) -> Exotic<T> {
    return Exotic::from_box(a);
  }
}
