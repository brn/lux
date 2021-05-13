use super::heap::*;
use crate::def::*;
use crate::structs::{HeapObject, JsBoolean, JsNull, JsUndefined};

pub struct Context {
  js_true: Addr,
  js_false: Addr,
  js_undefined: Addr,
  js_null: Addr,
  pub heap: Heap,
}

impl Context {
  pub fn new() -> Context {
    let heap = Heap::new();
    let mut c = Context {
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

  pub fn allocate(&mut self, size: usize) -> *mut Byte {
    return self.heap.allocate(size);
  }

  pub fn allocate_persist(&mut self, size: usize) -> *mut Byte {
    return self.heap.allocate(size);
  }

  pub fn js_true(&self) -> JsBoolean {
    return JsBoolean::from_ptr(self.js_true);
  }

  pub fn js_false(&self) -> JsBoolean {
    return JsBoolean::from_ptr(self.js_false);
  }

  pub fn js_null(&self) -> JsNull {
    return JsNull::from_ptr(self.js_null);
  }

  pub fn js_undefined(&self) -> JsUndefined {
    return JsUndefined::from_ptr(self.js_undefined);
  }
}
