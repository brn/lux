use super::super::cell::*;
use super::super::repr::Repr;
use super::super::shape::Shape;
use crate::context::{AllocationOnlyContext, Context, ObjectRecordsInitializedContext};
use crate::utility::{BitOperator, Bitset};
use std::mem::size_of;

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct DataPropertyDescriptorLayout {
  value: Repr,
}

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct AccessorPropertyDescriptorLayout {
  get: Repr,
  set: Repr,
}

impl From<HeapLayout<DataPropertyDescriptorLayout>> for HeapLayout<AccessorPropertyDescriptorLayout> {
  fn from(a: HeapLayout<DataPropertyDescriptorLayout>) -> HeapLayout<AccessorPropertyDescriptorLayout> {
    return HeapLayout::<AccessorPropertyDescriptorLayout>::wrap(a.as_addr());
  }
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct PropertyDescriptor(HeapLayout<DataPropertyDescriptorLayout>);
impl_object!(PropertyDescriptor, HeapLayout<DataPropertyDescriptorLayout>);

impl PropertyDescriptor {
  pub const DATA_DESCRIPTOR_SIZE: usize = size_of::<DataPropertyDescriptorLayout>();
  pub const ACCESSOR_DESCRIPTOR_SIZE: usize = size_of::<AccessorPropertyDescriptorLayout>();
  pub const DEFAULT: u8 = 0;
  pub const WRITABLE: u8 = 0x1;
  pub const ENUMERABLE: u8 = 0x2;
  pub const CONFIGURABLE: u8 = 0x4;

  const WRITABLE_INDEX: usize = 1;
  const ENUMERABLE_INDEX: usize = 2;
  const CONFIGURABLE_INDEX: usize = 3;
  const DATA_PD_INDEX: usize = 4;

  pub fn new_data_descriptor(
    context: impl ObjectRecordsInitializedContext,
    bit: u8,
    value: Repr,
  ) -> PropertyDescriptor {
    let mut layout =
      HeapLayout::<DataPropertyDescriptorLayout>::new(context, context.object_records().data_descriptor_record());
    let mut descriptor = PropertyDescriptor(layout);
    layout.value = value;
    HeapObject::assign_data_field(&mut descriptor, bit as u64);
    HeapObject::set_data_field(&mut descriptor, PropertyDescriptor::DATA_PD_INDEX);
    return descriptor;
  }

  pub fn new_accessor_descriptor(
    context: impl Context,
    bit: u8,
    get: Option<Repr>,
    set: Option<Repr>,
  ) -> PropertyDescriptor {
    let layout =
      HeapLayout::<DataPropertyDescriptorLayout>::new(context, context.object_records().accessor_descriptor_record());
    let mut a_layout = HeapLayout::<AccessorPropertyDescriptorLayout>::from(layout);
    let mut descriptor = PropertyDescriptor(layout);
    HeapObject::assign_data_field(&mut descriptor, bit as u64);
    HeapObject::set_data_field(&mut descriptor, PropertyDescriptor::DATA_PD_INDEX);
    if get.is_some() {
      a_layout.get = get.unwrap();
    } else {
      a_layout.get = context.globals().js_undefined();
    }
    if set.is_some() {
      a_layout.set = set.unwrap();
    } else {
      a_layout.get = context.globals().js_undefined();
    }
    return descriptor;
  }

  pub fn is_writable(&self) -> bool {
    return HeapObject::get_data_field(self, PropertyDescriptor::WRITABLE_INDEX);
  }

  pub fn is_enumerable(&self) -> bool {
    return HeapObject::get_data_field(self, PropertyDescriptor::ENUMERABLE_INDEX);
  }

  pub fn is_configurable(&self) -> bool {
    return HeapObject::get_data_field(self, PropertyDescriptor::CONFIGURABLE_INDEX);
  }

  pub fn value(&self) -> Repr {
    assert!(self.is_data_descriptor());
    return self.value;
  }

  pub fn getter(&self) -> Repr {
    assert!(self.is_accessor_descriptor());
    let accessor_property_descriptor = HeapLayout::<AccessorPropertyDescriptorLayout>::from(self.0);
    return accessor_property_descriptor.get;
  }

  pub fn setter(&self) -> Repr {
    assert!(self.is_accessor_descriptor());
    let accessor_property_descriptor = HeapLayout::<AccessorPropertyDescriptorLayout>::from(self.0);
    return accessor_property_descriptor.set;
  }

  pub fn is_data_descriptor(&self) -> bool {
    return HeapObject::get_data_field(self, PropertyDescriptor::DATA_PD_INDEX);
  }

  pub fn is_accessor_descriptor(&self) -> bool {
    return self.is_data_descriptor();
  }
}
