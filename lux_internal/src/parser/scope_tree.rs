use super::scope::{Scope, ScopeFlag};
use crate::utility::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Property, Debug)]
pub struct ScopeTreeLayout {
  #[property(get(type = "copy"))]
  root: Exotic<Scope>,

  #[property(get(type = "copy"))]
  current: Exotic<Scope>,
}

impl ScopeTreeLayout {
  pub fn new(root: Exotic<Scope>) -> Self {
    return ScopeTreeLayout { root, current: root };
  }
}

#[derive(Clone)]
pub struct ScopeTree(Rc<RefCell<ScopeTreeLayout>>, WeakRegion);

impl ScopeTree {
  pub fn new(region: WeakRegion, flag: ScopeFlag) -> Self {
    let scope = Scope::new(region.clone(), flag);
    return ScopeTree(Rc::new(RefCell::new(ScopeTreeLayout::new(scope))), region);
  }

  pub fn root(&self) -> Exotic<Scope> {
    return self.0.borrow().root();
  }

  pub fn current(&self) -> Exotic<Scope> {
    return self.0.borrow().current();
  }

  pub fn enter_class_scope(&mut self) -> Exotic<Scope> {
    return self.enter_new_scope(ScopeFlag::CLASS);
  }

  pub fn enter_new_scope(&mut self, mut scope_flag: ScopeFlag) -> Exotic<Scope> {
    let mut layout = self.0.borrow_mut();
    let mut scope = Scope::new(self.1.clone(), scope_flag);
    layout.current.add_child_scope(scope);
    scope.set_parent_scope(layout.current);
    layout.current = scope;
    return scope;
  }

  pub fn leave_current_scope(&mut self, scope: Exotic<Scope>) {
    let mut layout = self.0.borrow_mut();
    assert_eq!(layout.current, scope);
    if let Some(parent) = layout.current.parent_scope() {
      layout.current = parent;
    }
  }
}
