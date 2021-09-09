use super::ast::*;
use super::source_position::*;
use crate::structs::FixedU16CodePointArray;
use crate::utility::{Exotic, Region};
use property::Property;
use std::collections::HashMap;

bitflags! {
  pub struct ScopeFlag: u8 {
    const NONE = 0;
    const STRICT_MODE = 1;
    const OPAQUE = 2;
    const TRANSPARENT = 4;
    const HAS_SUPER_CALL = 8;
  }
}

#[derive(Property)]
pub struct Scope {
  #[property(skip)]
  first_super_call_position: Option<SourcePosition>,

  #[property(skip)]
  scope_flag: ScopeFlag,

  #[property(skip)]
  var_map: HashMap<FixedU16CodePointArray, Ast>,

  #[property(skip)]
  children: Vec<Exotic<Scope>>,

  #[property(get(type = "copy"), set(type = "ref"))]
  parent_scope: Option<Exotic<Scope>>,

  #[property(skip)]
  nearest_opaque_scope: Option<Exotic<Scope>>,
}

impl Scope {
  pub fn new(mut region: Region, scope_flag: ScopeFlag) -> Exotic<Self> {
    return region.alloc(Scope {
      scope_flag,
      children: Vec::new(),
      var_map: HashMap::new(),
      parent_scope: None,
      nearest_opaque_scope: None,
      first_super_call_position: None,
    });
  }

  pub fn set_first_super_call_position(&mut self, pos: &SourcePosition) {
    if self.first_super_call_position.is_none() {
      self.first_super_call_position = Some(pos.clone());
    }
  }

  pub fn first_super_call_position(&mut self) -> Option<&SourcePosition> {
    return self.first_super_call_position.as_ref();
  }

  pub fn mark_as_strict_mode(&mut self) {
    self.scope_flag |= ScopeFlag::STRICT_MODE;
  }

  pub fn set_has_super_call(&mut self) {
    self.scope_flag |= ScopeFlag::HAS_SUPER_CALL;
  }

  pub fn has_super_call(&self) -> bool {
    return self.scope_flag.contains(ScopeFlag::HAS_SUPER_CALL);
  }

  pub fn is_strict_mode(&self) -> bool {
    return self.scope_flag.contains(ScopeFlag::STRICT_MODE);
  }

  pub fn is_opaque(&self) -> bool {
    return self.scope_flag.contains(ScopeFlag::OPAQUE);
  }

  pub fn is_transparent(&self) -> bool {
    return self.scope_flag.contains(ScopeFlag::TRANSPARENT);
  }

  pub fn add_child_scope(&mut self, mut scope: Exotic<Scope>) {
    if self.is_strict_mode() {
      scope.mark_as_strict_mode();
    }
    self.children.push(scope);
  }

  pub fn iter(&mut self) -> impl Iterator<Item = &Exotic<Scope>> {
    return self.children.iter();
  }

  pub fn declare_var(&mut self, name: FixedU16CodePointArray, value: Ast) {
    if self.is_opaque() {
      self.var_map.insert(name, value);
    } else {
      if let Some(mut scope) = self.nearest_opaque_scope {
        scope.declare_var(name, value);
        return;
      }
      let mut parent = self.parent_scope;
      while parent.is_some() && parent.unwrap().is_transparent() {
        parent = parent.unwrap().parent_scope;
      }
      self.nearest_opaque_scope = parent;
      parent.unwrap().declare_var(name, value);
    }
  }

  pub fn is_declared(&self, name: FixedU16CodePointArray) -> Option<&Ast> {
    return self.var_map.get(&name);
  }
}

impl std::fmt::Debug for Scope {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return write!(
      f,
      "Scope {{ type = {} strict_mode = {} children = {} has_parent = {} }}",
      if self.is_opaque() { "opaque" } else { "transparent" },
      self.is_strict_mode(),
      self.children.len(),
      self.parent_scope.is_some()
    );
  }
}
