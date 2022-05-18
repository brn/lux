use super::node_ops::*;
use crate::utility::WeakRegion;

pub struct AstBuilder {
  region: WeakRegion,
}
impl AstBuilder {
  pub fn new(region: WeakRegion) -> Self {
    return AstBuilder { region };
  }
}

impl NodeOps for AstBuilder {
  fn region(&mut self) -> &mut WeakRegion {
    return &mut self.region;
  }
}
