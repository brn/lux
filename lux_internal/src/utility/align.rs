#[inline]
pub const fn align(size: usize, alignment: usize) -> usize {
  return (size + (alignment - 1)) & !(alignment - 1);
}
