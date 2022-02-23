use super::ast::*;
use super::source_position::*;
use crate::structs::FixedU16CodePointArray;
use crate::utility::{Exotic, WeakRegion};
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
    const ALLOW_SUPER_CALL = 0x200;
    const ALLOW_SUPER_PROPERTY = 0x400;
    const ALLOW_NEW_TARGET = 0x800;
    const DEFAULT_EXPORTED = 0x1000;
    const CLASS = 0x2000;
    const MODULE = 0x4000;
  }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum VariableType {
  FormalParameter,
  Lexical,
  ExportLexical,
  ExportRenamed,
  LegacyVar,
  ExportLegacyVar,
  WillExportVar,
  CatchParameterLegacyVar,
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

  #[property(skip)]
  will_export_map: HashMap<Vec<u16>, (SourcePosition, VariableType)>,

  #[property(skip)]
  label_stack: Vec<Vec<u16>>,

  #[property(skip)]
  label_map: HashMap<Vec<u16>, SourcePosition>,
}

impl Scope {
  pub fn new(mut region: WeakRegion, scope_flag: ScopeFlag) -> Exotic<Self> {
    return region.alloc(Scope {
      scope_flag,
      children: Vec::new(),
      var_list: Vec::new(),
      var_map: HashMap::new(),
      will_export_map: HashMap::new(),
      parent_scope: None,
      nearest_opaque_scope: None,
      first_super_call_position: None,
      first_super_property_position: None,
      label_stack: Vec::new(),
      label_map: HashMap::new(),
    });
  }

  pub fn set_first_super_call_position(&mut self, pos: &SourcePosition) {
    let mut scope = self.get_nearest_non_lexical_scope();
    if scope.first_super_call_position.is_none() {
      scope.first_super_call_position = Some(pos.clone());
    }
  }

  pub fn first_super_call_position(&mut self) -> Option<SourcePosition> {
    let scope = self.get_nearest_non_lexical_scope();
    return scope.first_super_call_position.clone();
  }

  pub fn set_first_super_property_position(&mut self, pos: &SourcePosition) {
    let mut scope = self.get_nearest_non_lexical_scope();
    if scope.first_super_property_position.is_none() {
      scope.first_super_property_position = Some(pos.clone());
    }
  }

  pub fn first_super_property_position(&mut self) -> Option<SourcePosition> {
    let scope = self.get_nearest_non_lexical_scope();
    return scope.first_super_property_position.clone();
  }

  pub fn mark_as_default_exported(&mut self) {
    self.scope_flag |= ScopeFlag::DEFAULT_EXPORTED;
  }

  pub fn is_default_exported(&self) -> bool {
    return self.scope_flag.contains(ScopeFlag::DEFAULT_EXPORTED);
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

  pub fn is_root_scope(&self) -> bool {
    return self.scope_flag.contains(ScopeFlag::ROOT_SCOPE);
  }

  pub fn is_simple_parameter(&self) -> bool {
    return self.scope_flag.intersects(ScopeFlag::SIMPLE_PARAMETER | ScopeFlag::ROOT_SCOPE);
  }

  pub fn mark_as_simple_parameter(&mut self) {
    return self.scope_flag |= ScopeFlag::SIMPLE_PARAMETER;
  }

  pub fn is_async_context(&self) -> bool {
    return self.scope_flag.contains(ScopeFlag::ASYNC_CONTEXT);
  }

  pub fn is_generator_context_origin(&self) -> bool {
    return self.scope_flag.contains(ScopeFlag::GENERATOR_CONTEXT);
  }

  pub fn is_generator_context(&self) -> bool {
    return self.is_generator_context_origin()
      || if let Some(p) = self.parent_scope {
        p.is_generator_context()
      } else {
        false
      };
  }

  pub fn is_strict_mode_origin(&self) -> bool {
    return self.scope_flag.intersects(ScopeFlag::STRICT_MODE | ScopeFlag::CLASS);
  }

  pub fn is_strict_mode(&self) -> bool {
    return self.is_strict_mode_origin()
      || if let Some(p) = self.parent_scope {
        p.is_strict_mode()
      } else {
        false
      };
  }

  pub fn is_module(&self) -> bool {
    return if self.is_root_scope() {
      self.scope_flag.contains(ScopeFlag::MODULE)
    } else {
      if let Some(p) = self.parent_scope {
        p.is_module()
      } else {
        false
      }
    };
  }

  pub fn mark_as_module(&mut self) {
    self.scope_flag |= ScopeFlag::MODULE;
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

  pub fn is_super_call_allowed(&self) -> bool {
    return self.scope_flag.contains(ScopeFlag::ALLOW_SUPER_CALL);
  }

  pub fn is_super_property_allowed(&self) -> bool {
    return self.scope_flag.contains(ScopeFlag::ALLOW_SUPER_PROPERTY);
  }

  pub fn is_new_target_allowed(&self) -> bool {
    return self.scope_flag.contains(ScopeFlag::ALLOW_NEW_TARGET);
  }

  pub fn is_class_scope(&self) -> bool {
    return self.scope_flag.contains(ScopeFlag::CLASS);
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

  pub fn get_unfilled_will_be_exported_var(&self) -> Option<SourcePosition> {
    let mut scope = self.get_declaratable_scope();
    for (_, (pos, var_type)) in scope.var_map.iter() {
      if *var_type == VariableType::WillExportVar {
        return Some(pos.clone());
      }
    }
    return None;
  }

  pub fn declare_vars(
    &mut self,
    var_type: VariableType,
    vars: &Vec<(Vec<u16>, SourcePosition)>,
  ) -> Option<(SourcePosition, SourcePosition)> {
    let mut scope = self.get_declaratable_scope();
    for var in vars.iter() {
      if let Some(ref dup_info) = scope.declare_var(var_type, var.clone()) {
        return Some(dup_info.clone());
      }
    }
    return None;
  }

  pub fn declare_var(&mut self, var_type: VariableType, var: (Vec<u16>, SourcePosition)) -> Option<(SourcePosition, SourcePosition)> {
    let mut scope = self.get_declaratable_scope();
    return scope.declare_var_internal(var_type, var);
  }

  fn declare_var_internal(&mut self, var_type: VariableType, var: (Vec<u16>, SourcePosition)) -> Option<(SourcePosition, SourcePosition)> {
    #[cfg(debug_assertions)]
    {
      if var_type == VariableType::ExportLegacyVar {
        debug_assert!(self.is_root_scope());
      }
    }

    if let Some((ref pos, v_type)) = self.get_already_declared_var_position_internal(&var.0, var_type) {
      if var_type == VariableType::WillExportVar {
        if *v_type == VariableType::ExportLexical || *v_type == VariableType::WillExportVar || *v_type == VariableType::ExportRenamed {
          return Some((var.1.clone(), pos.clone()));
        }
        self.will_export_map.insert(var.0.clone(), (var.1.clone(), var_type));
        return None;
      } else {
        return Some((var.1.clone(), pos.clone()));
      }
    }

    if var_type != VariableType::WillExportVar {
      self.var_list.push((var.0.clone(), var.1.clone(), var_type));
    } else {
      self.will_export_map.insert(var.0.clone(), (var.1.clone(), var_type));
    }
    self.var_map.insert(var.0.clone(), (var.1.clone(), var_type));

    if match var_type {
      VariableType::LegacyVar | VariableType::ExportLegacyVar => true,
      _ => false,
    } {
      if self.is_lexical() {
        let mut scope = self.parent_scope.unwrap();
        while scope.is_lexical() {
          if let Some(ref a) = scope.declare_var(var_type, var.clone()) {
            return Some(*a);
          }
          scope = scope.parent_scope.unwrap();
        }
      }
    }
    return None;
  }

  pub fn get_already_declared_var_position(&self, var: &Vec<u16>, variable_type: VariableType) -> Option<(SourcePosition, VariableType)> {
    let scope = self.get_declaratable_scope();
    return scope.get_already_declared_var_position_internal(var, variable_type).cloned();
  }

  fn get_already_declared_var_position_internal(
    &self,
    var: &Vec<u16>,
    variable_type: VariableType,
  ) -> Option<&(SourcePosition, VariableType)> {
    if variable_type == VariableType::WillExportVar {
      if let Some(ref val) = self.will_export_map.get(var) {
        return Some(val);
      }
    }
    if let Some(ref val) = self.var_map.get(var) {
      use VariableType::*;
      match val.1 {
        Lexical => {
          return match variable_type {
            ExportRenamed => None,
            _ => Some(val),
          };
        }
        ExportLexical => {
          return Some(val);
        }
        FormalParameter => {
          return match variable_type {
            Lexical => Some(val),
            _ => None,
          };
        }
        LegacyVar | CatchParameterLegacyVar => {
          return match variable_type {
            Lexical | ExportLexical => Some(val),
            _ => None,
          };
        }
        ExportLegacyVar => {
          return match variable_type {
            Lexical | ExportLexical | ExportLegacyVar | ExportRenamed => Some(val),
            _ => None,
          };
        }
        WillExportVar => {
          return match variable_type {
            ExportLexical | ExportLegacyVar | WillExportVar | ExportRenamed => Some(val),
            _ => None,
          }
        }
        _ => return None,
      }
    }
    return None;
  }

  pub fn push_label_without_duplication(&mut self, label: Vec<u16>, source_position: SourcePosition) -> Option<SourcePosition> {
    if self.is_lexical() {
      let mut scope = self.get_nearest_non_lexical_scope();
      return scope.push_label_without_duplication(label, source_position);
    }
    if self.label_map.contains_key(&label) {
      return self.label_map.get(&label).cloned();
    }
    self.label_stack.push(label.clone());
    self.label_map.insert(label, source_position);
    return None;
  }

  pub fn is_label_exists(&self, label: &Vec<u16>) -> bool {
    if self.is_lexical() {
      let mut scope = self.get_nearest_non_lexical_scope();
      return scope.is_label_exists(label);
    }

    return self.label_map.contains_key(label);
  }

  pub fn pop_label(&mut self) {
    if self.is_lexical() {
      let mut scope = self.get_nearest_non_lexical_scope();
      scope.pop_label();
      return;
    }
    if let Some(label) = self.label_stack.pop() {
      self.label_map.remove(&label);
    }
  }

  pub fn has_escape_target(&self, name: &Vec<u16>) -> bool {
    let scope = self.get_nearest_non_lexical_scope();
    return scope.label_map.contains_key(name);
  }

  pub fn print_var_map(&self) {
    println!("{:?}", self.var_map);
  }

  pub fn get_nearest_non_lexical_scope(&self) -> Exotic<Scope> {
    let mut scope = self.get_declaratable_scope();
    if !scope.is_lexical() {
      return Exotic::from_self(self);
    }
    let mut parent = scope.parent_scope;
    while parent.is_some() && parent.unwrap().is_lexical() {
      parent = parent.unwrap().parent_scope;
    }
    return parent.unwrap();
  }

  fn get_declaratable_scope(&self) -> Exotic<Scope> {
    let mut scope = Exotic::from_self(self);
    if self.is_class_scope() {
      while !scope.is_root_scope() && scope.is_class_scope() {
        scope = scope.parent_scope().unwrap();
      }
    }
    return scope;
  }
}

impl std::fmt::Debug for Scope {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return write!(
      f,
      "Scope {{ type = {} strict_mode = {} children = {} has_parent = {} }}",
      if self.is_opaque() {
        "opaque"
      } else if self.is_transparent() {
        "transparent"
      } else {
        "lexical"
      },
      self.is_strict_mode(),
      self.children.len(),
      self.parent_scope.is_some(),
    );
  }
}
