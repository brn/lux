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
    const LEXICAL = 0x10;
    const HAS_SUPER_CALL = 0x20;
    const SIMPLE_PARAMETER = 0x40;
    const ASYNC_CONTEXT = 0x80;
    const GENERATOR_CONTEXT = 0x100;
  }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum VariableType {
  FormalParameter,
  Lexical,
  LegacyVar,
}

#[derive(Property)]
pub struct Scope {
  #[property(skip)]
  first_super_call_position: Option<SourcePosition>,

  #[property(skip)]
  first_super_property_position: Option<SourcePosition>,

  #[property(skip)]
  scope_flag: ScopeFlag,

  #[property(skip)]
  var_list: Vec<(Vec<u16>, SourcePosition, VariableType)>,

  #[property(skip)]
  children: Vec<Exotic<Scope>>,

  #[property(get(type = "copy"), set(type = "ref"))]
  parent_scope: Option<Exotic<Scope>>,

  #[property(skip)]
  nearest_opaque_scope: Option<Exotic<Scope>>,

  #[property(skip)]
  var_map: HashMap<Vec<u16>, (SourcePosition, VariableType)>,
}

impl Scope {
  pub fn new(mut region: Region, scope_flag: ScopeFlag) -> Exotic<Self> {
    return region.alloc(Scope {
      scope_flag,
      children: Vec::new(),
      var_list: Vec::new(),
      var_map: HashMap::new(),
      parent_scope: None,
      nearest_opaque_scope: None,
      first_super_call_position: None,
      first_super_property_position: None,
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

  pub fn set_first_super_property_position(&mut self, pos: &SourcePosition) {
    if self.first_super_property_position.is_none() {
      self.first_super_property_position = Some(pos.clone());
    }
  }

  pub fn first_super_property_position(&mut self) -> Option<&SourcePosition> {
    return self.first_super_property_position.as_ref();
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
    return self.scope_flag.intersects(ScopeFlag::SIMPLE_PARAMETER | ScopeFlag::ROOT_SCOPE);
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

  pub fn is_lexical(&self) -> bool {
    return self.scope_flag.contains(ScopeFlag::LEXICAL);
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

  pub fn declare_vars(&mut self, var_type: VariableType, vars: &Vec<(Vec<u16>, SourcePosition)>) {
    for var in vars.iter() {
      self.declare_var(var_type, var.clone());
    }
  }

  pub fn declare_vars_without_lexical_duplication(
    &mut self,
    var_type: VariableType,
    vars: &Vec<(Vec<u16>, SourcePosition)>,
  ) -> Option<(SourcePosition, SourcePosition)> {
    for var in vars.iter() {
      if let Some(pos) = self.get_already_declared_var_position(&var.0, var_type == VariableType::LegacyVar) {
        return Some((var.1.clone(), pos.clone()));
      }
      self.declare_var(var_type, var.clone());
    }
    return None;
  }

  pub fn declare_var(&mut self, var_type: VariableType, var: (Vec<u16>, SourcePosition)) {
    if self.is_opaque() {
      self.var_list.push((var.0.clone(), var.1.clone(), var_type));
      self.var_map.insert(var.0, (var.1, var_type));
    } else {
      if let Some(mut scope) = self.nearest_opaque_scope {
        scope.declare_var(var_type, var);
        return;
      }
      let mut parent = self.parent_scope;
      while parent.is_some() && parent.unwrap().is_transparent() {
        parent = parent.unwrap().parent_scope;
      }
      self.nearest_opaque_scope = parent;
      parent.unwrap().declare_var(var_type, var);
    }
  }

  pub fn get_already_declared_var_position(&self, var: &Vec<u16>, should_search_only_lexical_decl: bool) -> Option<&SourcePosition> {
    if let Some(ref val) = self.var_map.get(var) {
      if should_search_only_lexical_decl {
        if val.1 == VariableType::Lexical {
          return Some(&val.0);
        }
      } else {
        return Some(&val.0);
      }
    }
    if self.is_lexical() {
      if let Some(ref scope) = self.parent_scope {
        return scope.get_already_declared_var_position(var, should_search_only_lexical_decl);
      }
    }
    return None;
  }

  pub fn print_var_map(&self) {
    println!("{:?}", self.var_map);
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
