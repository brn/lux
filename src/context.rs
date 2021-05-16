use super::heap::*;
use crate::def::*;
use crate::structs::{HeapObject, JsBoolean, JsNull, JsUndefined};

pub struct LuxContext {
  js_true: Addr,
  js_false: Addr,
  js_undefined: Addr,
  js_null: Addr,
  pub heap: Heap,
}

impl LuxContext {
  pub fn new() -> LuxContext {
    let heap = Heap::new();
    let mut c = LuxContext {
      heap,
      js_true: std::ptr::null_mut(),
      js_false: std::ptr::null_mut(),
      js_undefined: std::ptr::null_mut(),
      js_null: std::ptr::null_mut(),
    };
    c.js_true = JsBoolean::persist(&mut c, true).raw_heap();
    c.js_false = JsBoolean::persist(&mut c, false).raw_heap();
    c.js_undefined = JsUndefined::persist(&mut c).raw_heap();
    c.js_null = JsNull::persist(&mut c).raw_heap();
    return c;
  }

  pub fn js_true(&self) -> JsBoolean {
    return JsBoolean::from(self.js_true);
  }

  pub fn js_false(&self) -> JsBoolean {
    return JsBoolean::from(self.js_false);
  }

  pub fn js_null(&self) -> JsNull {
    return JsNull::from(self.js_null);
  }

  pub fn js_undefined(&self) -> JsUndefined {
    return JsUndefined::from(self.js_undefined);
  }
}

pub trait Context {
  fn allocate(&mut self, size: usize) -> Addr;
  fn allocate_persist(&mut self, size: usize) -> Addr;
}

impl Context for LuxContext {
  fn allocate(&mut self, size: usize) -> Addr {
    return self.heap.allocate(size);
  }

  fn allocate_persist(&mut self, size: usize) -> Addr {
    return self.heap.allocate(size);
  }
}

#[cfg(test)]
pub mod testing {
  use super::*;
  use std::alloc::{alloc, Layout};

  pub struct MockedContext {}

  impl MockedContext {
    pub fn new() -> MockedContext {
      return MockedContext {};
    }
  }

  impl Context for MockedContext {
    fn allocate(&mut self, size: usize) -> Addr {
      return unsafe { alloc(Layout::from_size_align(size, ALIGNMENT).unwrap()) };
    }

    fn allocate_persist(&mut self, size: usize) -> Addr {
      return unsafe { alloc(Layout::from_size_align(size, ALIGNMENT).unwrap()) };
    }
  }
}
