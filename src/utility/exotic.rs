use std::marker::PhantomData;
use std::mem::transmute;

pub struct Exotic<T>(usize, PhantomData<T>);

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

  #[inline]
  pub fn copy(&self) -> Result<Exotic<T>, &'static str> {
    if Exotic::<T>::is_copyable(self) {
      return Ok(Exotic::<T>(self.0, PhantomData));
    }
    return Err("Cannot copy pointer from box");
  }

  #[inline]
  pub fn copy_unchecked(&self) -> Exotic<T> {
    return self.copy().unwrap();
  }

  #[inline(always)]
  fn from_box(b: Box<T>) -> Exotic<T> {
    return Exotic(unsafe { transmute::<*mut T, usize>(Box::leak(b)) } | 1, PhantomData);
  }

  #[inline(always)]
  fn is_copyable(this: &Self) -> bool {
    return this.0 & 1 == 0;
  }
}

impl<T> std::ops::Drop for Exotic<T> {
  #[inline(always)]
  fn drop(&mut self) {
    let _ = self.to_box();
  }
}

impl<T> From<Box<T>> for Exotic<T> {
  #[inline(always)]
  fn from(a: Box<T>) -> Exotic<T> {
    return Exotic::from_box(a);
  }
}
