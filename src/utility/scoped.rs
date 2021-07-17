pub struct _Scoped<T: FnMut()>(T);
impl<T: FnMut()> _Scoped<T> {
  pub fn new(a: T) -> Self {
    return _Scoped(a);
  }
}
impl<T: FnMut()> Drop for _Scoped<T> {
  fn drop(&mut self) {
    self.0();
  }
}

macro_rules! scoped {
  ($fn:expr) => {
    let _ = _Scoped::new($fn);
  };
}
