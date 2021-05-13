use super::cell::*;
use super::repr::Repr;
use super::shape::*;
use crate::context::Context;
use crate::def::*;
use crate::impl_repr_convertion;
use crate::utility::align;
use std::mem::size_of;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct JsUndefined {
  heap: *mut Byte,
}
impl_heap_object!(JsUndefined);
impl_repr_convertion!(JsUndefined);

impl JsUndefined {
  pub const TYPE: Shape = Shape::undefined();
  fn byte_length(&self) -> usize {
    return Cell::SIZE;
  }
  pub fn persist<'a>(context: &'a mut Context) -> JsUndefined {
    let cell = Cell::persist(context, Cell::SIZE, Shape::undefined());
    return JsUndefined { heap: cell.raw_heap() };
  }
  pub fn wrap(heap: Addr) -> JsUndefined {
    return JsUndefined { heap };
  }
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct JsNull {
  heap: *mut Byte,
}
impl_heap_object!(JsNull);
impl_repr_convertion!(JsNull);

impl JsNull {
  pub const TYPE: Shape = Shape::null();
  fn byte_length(&self) -> usize {
    return Cell::SIZE;
  }
  pub fn persist<'a>(context: &'a mut Context) -> JsNull {
    let cell = Cell::persist(context, Cell::SIZE, Shape::null());
    return JsNull { heap: cell.raw_heap() };
  }
  pub fn wrap(heap: Addr) -> JsNull {
    return JsNull { heap };
  }
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct JsBoolean {
  heap: *mut Byte,
}
impl_heap_object!(JsBoolean);
impl_repr_convertion!(JsBoolean);

impl JsBoolean {
  pub const TYPE: Shape = Shape::boolean();
  pub const SIZE: usize = align(Cell::SIZE + size_of::<u8>(), ALIGNMENT);

  fn byte_length(&self) -> usize {
    return JsBoolean::SIZE;
  }

  pub fn persist<'a>(context: &'a mut Context, val: bool) -> JsBoolean {
    let cell = Cell::persist(context, JsBoolean::SIZE, Shape::null());
    let heap = cell.raw_heap();
    let value_addr = field_addr(heap, Cell::SIZE);
    unsafe { *value_addr = if val { 1 } else { 0 } };
    return JsBoolean { heap };
  }

  pub fn is_true(&self) -> bool {
    let value = field_addr(self.heap, Cell::SIZE);
    return unsafe { (*value) == 1 };
  }

  pub fn wrap(heap: Addr) -> JsBoolean {
    return JsBoolean { heap };
  }
}
