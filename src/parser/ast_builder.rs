use super::node_ops::*;
use crate::utility::Region;

pub struct AstBuilder {
  region: Region,
}
impl AstBuilder {
  pub fn new(region: Region) -> Self {
    return AstBuilder { region };
  }
}

impl NodeOps for AstBuilder {
  fn region(&mut self) -> &mut Region {
    return &mut self.region;
  }
}
