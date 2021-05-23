use super::super::cell::*;
use super::super::js_object::JsVal;
use super::super::shape::Shape;
use crate::context::{AllocationOnlyContext, Context};
use crate::utility::{BitOperator, Bitset};
use std::mem::size_of;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct DataPropertyDescriptorLayout {
  flags: Bitset<u8>,
  value: BareHeapLayout<JsVal>,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct AccessorPropertyDescriptorLayout {
  flags: Bitset<u8>,
  get: BareHeapLayout<JsVal>,
  set: BareHeapLayout<JsVal>,
}

impl From<HeapLayout<DataPropertyDescriptorLayout>> for HeapLayout<AccessorPropertyDescriptorLayout> {
  fn from(a: HeapLayout<DataPropertyDescriptorLayout>) -> HeapLayout<AccessorPropertyDescriptorLayout> {
    return HeapLayout::<AccessorPropertyDescriptorLayout>::wrap(a.as_addr());
  }
}

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct PropertyDescriptor(HeapLayout<DataPropertyDescriptorLayout>);
impl_object!(PropertyDescriptor, HeapLayout<DataPropertyDescriptorLayout>);

impl PropertyDescriptor {
  const SIZE: usize = Cell::SIZE;
  pub const DEFAULT: u8 = 0;
  pub const WRITABLE: u8 = 0x1;
  pub const ENUMERABLE: u8 = 0x2;
  pub const CONFIGURABLE: u8 = 0x4;

  const WRITABLE_INDEX: usize = 1;
  const ENUMERABLE_INDEX: usize = 2;
  const CONFIGURABLE_INDEX: usize = 3;
  const DATA_PD_INDEX: usize = 4;

  pub fn new_data_descriptor(context: &mut impl AllocationOnlyContext, bit: u8, value: JsVal) -> PropertyDescriptor {
    let mut layout = HeapLayout::<DataPropertyDescriptorLayout>::new(
      context,
      PropertyDescriptor::SIZE + size_of::<DataPropertyDescriptorLayout>(),
      Shape::property_descriptor(),
    );
    layout.value.set(value);
    layout.flags.assign(bit);
    return PropertyDescriptor(layout);
  }

  pub fn new_accessor_descriptor(
    context: &mut impl Context,
    bit: u8,
    get: Option<JsVal>,
    set: Option<JsVal>,
  ) -> PropertyDescriptor {
    let layout = HeapLayout::<DataPropertyDescriptorLayout>::new(
      context,
      PropertyDescriptor::SIZE + size_of::<AccessorPropertyDescriptorLayout>(),
      Shape::property_descriptor(),
    );
    let mut a_layout = HeapLayout::<AccessorPropertyDescriptorLayout>::from(layout);
    a_layout.flags.assign(bit);
    a_layout.flags.set(PropertyDescriptor::DATA_PD_INDEX);
    if get.is_some() {
      a_layout.get.set(get.unwrap());
    } else {
      a_layout.get.set(JsVal::from(context.js_undefined()));
    }
    if set.is_some() {
      a_layout.set.set(set.unwrap());
    } else {
      a_layout.get.set(JsVal::from(context.js_undefined()));
    }
    return PropertyDescriptor(layout);
  }

  pub fn is_writable(&self) -> bool {
    return self.flags.get(PropertyDescriptor::WRITABLE_INDEX);
  }

  pub fn is_enumerable(&self) -> bool {
    return self.flags.get(PropertyDescriptor::ENUMERABLE_INDEX);
  }

  pub fn is_configurable(&self) -> bool {
    return self.flags.get(PropertyDescriptor::CONFIGURABLE_INDEX);
  }

  pub fn value(&self) -> JsVal {
    assert!(self.is_data_descriptor());
    return self.value.handle();
  }

  pub fn getter(&self) -> JsVal {
    assert!(self.is_accessor_descriptor());
    let accessor_property_descriptor = HeapLayout::<AccessorPropertyDescriptorLayout>::from(self.0);
    return accessor_property_descriptor.get.handle();
  }

  pub fn setter(&self) -> JsVal {
    assert!(self.is_accessor_descriptor());
    let accessor_property_descriptor = HeapLayout::<AccessorPropertyDescriptorLayout>::from(self.0);
    return accessor_property_descriptor.set.handle();
  }

  pub fn is_data_descriptor(&self) -> bool {
    return self.flags.get(PropertyDescriptor::DATA_PD_INDEX);
  }

  pub fn is_accessor_descriptor(&self) -> bool {
    return self.is_data_descriptor();
  }
}
