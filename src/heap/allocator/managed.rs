pub struct Managed<'a, T> {
  value: &'a mut T,
}

impl<'a, T> Managed<'a, T> {
  pub fn wrap(value: &'a mut T) -> Managed<'a, T> {
    return Managed::<T> { value };
  }
}

impl<'a, T> Drop for Managed<'a, T> {
  fn drop(&mut self) {}
}
