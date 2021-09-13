use super::ast::*;
use super::source_position::*;
use crate::structs::FixedU16CodePointArray;
use crate::utility::{Exotic, Region};
use property::Property;
use std::collections::HashMap;

bitflags! {
  pub struct ScopeFlag: u16 {
    const NONE = 0;
    const ROOT_SCOPE = 0x1;
    const STRICT_MODE = 0x2;
    const OPAQUE = 0x4;
    const TRANSPARENT = 0x8;
    const HAS_SUPER_CALL = 0x10;
    const SIMPLE_PARAMETER = 0x20;
    const ASYNC_CONTEXT = 0x40;
    const GENERATOR_CONTEXT = 0x80;
  }
}

#[derive(Property)]
pub struct Scope {
  #[property(skip)]
  first_super_call_position: Option<SourcePosition>,

  #[property(skip)]
  scope_flag: ScopeFlag,

  #[property(skip)]
  var_list: Vec<(Vec<u16>, SourcePosition)>,

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
      var_list: Vec::new(),
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

  pub fn is_simple_parameter(&self) -> bool {
    return self
      .scope_flag
      .intersects(ScopeFlag::SIMPLE_PARAMETER | ScopeFlag::ROOT_SCOPE);
  }

  pub fn is_async_context(&self) -> bool {
    return self.scope_flag.contains(ScopeFlag::ASYNC_CONTEXT);
  }

  pub fn is_generator_context(&self) -> bool {
    return self.scope_flag.contains(ScopeFlag::GENERATOR_CONTEXT);
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

  pub fn declare_vars(&mut self, vars: &Vec<(Vec<u16>, SourcePosition)>) {
    for var in vars.iter() {
      self.declare_var(var.clone());
    }
  }

  pub fn declare_var(&mut self, var: (Vec<u16>, SourcePosition)) {
    if self.is_opaque() {
      self.var_list.push(var);
    } else {
      if let Some(mut scope) = self.nearest_opaque_scope {
        scope.declare_var(var);
        return;
      }
      let mut parent = self.parent_scope;
      while parent.is_some() && parent.unwrap().is_transparent() {
        parent = parent.unwrap().parent_scope;
      }
      self.nearest_opaque_scope = parent;
      parent.unwrap().declare_var(var);
    }
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
