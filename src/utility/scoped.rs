use super::exotic::Exotic;

pub struct _Scoped<C, T: FnMut(&mut C)>(C, T);
impl<C, T: FnMut(&mut C)> _Scoped<C, T> {
  pub fn new(context: C, a: T) -> Self {
    return _Scoped(context, a);
  }
}
impl<C, T: FnMut(&mut C)> std::ops::Deref for _Scoped<C, T> {
  type Target = C;
  fn deref(&self) -> &Self::Target {
    return &self.0;
  }
}
impl<C, T: FnMut(&mut C)> std::ops::DerefMut for _Scoped<C, T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    return &mut self.0;
  }
}
impl<C, T: FnMut(&mut C)> Drop for _Scoped<C, T> {
  fn drop(&mut self) {
    self.1(&mut self.0);
  }
}

macro_rules! scoped {
  ($self:tt, $fn:expr) => {{
    let __scoped_var__ = _Scoped::new($self, $fn);
    __scoped_var__
  }};
}

pub struct _Defer<C, T: FnMut(Exotic<C>)>(Exotic<C>, T);
impl<C, T: FnMut(Exotic<C>)> _Defer<C, T> {
  pub fn new(context: Exotic<C>, mut a: T) -> Self {
    return _Defer(context, a);
  }
}
impl<C, T: FnMut(Exotic<C>)> std::ops::Deref for _Defer<C, T> {
  type Target = C;
  fn deref(&self) -> &Self::Target {
    return &(*self.0);
  }
}
impl<C, T: FnMut(Exotic<C>)> std::ops::DerefMut for _Defer<C, T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    return &mut (*self.0);
  }
}
impl<C, T: FnMut(Exotic<C>)> Drop for _Defer<C, T> {
  fn drop(&mut self) {
    self.1(self.0);
  }
}

macro_rules! defer {
  ($self:tt, $fn:expr) => {{
    let __scoped_var__ = _Defer::new(Exotic::from_self($self), $fn);
    __scoped_var__
  }};
}
