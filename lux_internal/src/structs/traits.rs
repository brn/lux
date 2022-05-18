pub trait ObjectEquals {
  fn equals(&self, other: &Self) -> bool;
}
