use super::super::cell::{Cell, HeapLayout, HeapObject};
use super::super::repr::Repr;
use super::super::shape::{Shape, ShapeTag};
use super::backend::StringBackend;
use super::rope::{FlattenString, StringRope};
use super::string::JsString;
use super::u16_str::FixedU16CodePointArray;
use crate::context::{AllocationOnlyContext, Context};
use crate::def::*;
use std::mem::size_of;

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct SmallStringLayout {
  str: FixedU16CodePointArray,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct SmallString(HeapLayout<SmallStringLayout>);
impl_object!(SmallString, HeapLayout<SmallStringLayout>);

impl SmallString {
  const SIZE: usize = size_of::<SmallStringLayout>();
  pub fn new(context: impl AllocationOnlyContext, str: FixedU16CodePointArray) -> SmallString {
    let mut layout = HeapLayout::<SmallStringLayout>::new(context, SmallString::SIZE, Shape::small_string());
    layout.str = str;
    return SmallString(layout);
  }

  pub fn str(&self) -> FixedU16CodePointArray {
    return self.str;
  }
}

impl StringBackend for SmallString {
  fn at(&self, index: usize) -> Option<u16> {
    if index < self.str().length() {
      return Some(self.str()[index]);
    }
    return None;
  }

  fn concat(&self, context: impl Context, repr: Repr) -> JsString {
    match Cell::from(repr).shape().tag() {
      ShapeTag::SmallString => {
        let str = SmallString::from(repr);
        let mut array = FixedU16CodePointArray::new(context, self.len() + str.len());
        array.append(self.str());
        array.append(str.str());
        return JsString::new(context, array);
      }
      ShapeTag::OneByteChar => {
        let str = OneByteChar::from(repr);
        let mut array = FixedU16CodePointArray::new(context, self.len() + str.len());
        array.append(self.str());
        array.push(str.at(0).unwrap());
        return JsString::new(context, array);
      }
      ShapeTag::FlattenString => {
        let str = FlattenString::from(repr);
        let mut array = FixedU16CodePointArray::new(context, self.len() + str.len());
        array.append(self.str());
        array.append(str.str());
        return JsString::new(context, array);
      }
      ShapeTag::StringRope => {
        let left = StringRope::new(context, self.str());
        return left.concat(context, repr);
      }
      _ => unreachable!(),
    };
  }

  fn slice(&self, context: impl Context, start_index: usize, end_index: usize) -> FixedU16CodePointArray {
    return self.str().slice(context, start_index, end_index);
  }

  fn flatten(&mut self, _: impl AllocationOnlyContext) -> FixedU16CodePointArray {
    return self.str();
  }

  fn len(&self) -> usize {
    return self.str().length();
  }
}

#[repr(C)]
#[derive(Copy, Clone, Default)]
struct OneByte(u16);

impl From<Addr> for OneByte {
  fn from(a: Addr) -> OneByte {
    return OneByte(unsafe { *(a as *mut u16) });
  }
}
impl Into<Addr> for OneByte {
  fn into(self) -> Addr {
    return self.0 as Addr;
  }
}

impl std::ops::Deref for OneByte {
  type Target = u16;
  fn deref(&self) -> &u16 {
    return &self.0;
  }
}

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct OneByteCharLayout {
  ch: OneByte,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct OneByteChar(HeapLayout<OneByteCharLayout>);
impl_object!(OneByteChar, HeapLayout<OneByteCharLayout>);

impl OneByteChar {
  const SIZE: usize = size_of::<OneByteCharLayout>();
  pub fn new(context: impl AllocationOnlyContext, ch: u16) -> OneByteChar {
    let mut layout = HeapLayout::<OneByteCharLayout>::new(context, OneByteChar::SIZE, Shape::small_string());
    layout.ch = OneByte(ch);
    return OneByteChar(layout);
  }
}

impl StringBackend for OneByteChar {
  fn at(&self, index: usize) -> Option<u16> {
    if index > 0 {
      return None;
    }
    return Some(*self.ch);
  }

  fn concat(&self, context: impl Context, repr: Repr) -> JsString {
    match Cell::from(repr).shape().tag() {
      ShapeTag::SmallString => {
        let str = SmallString::from(repr);
        let mut array = FixedU16CodePointArray::new(context, str.len() + 1);
        array.push(self.at(0).unwrap());
        array.append(str.str());
        return JsString::new(context, array);
      }
      ShapeTag::OneByteChar => {
        let str = OneByteChar::from(repr);
        let mut array = FixedU16CodePointArray::new(context, 2);
        array.push(*self.ch);
        array.push(str.at(0).unwrap());
        return JsString::new(context, array);
      }
      ShapeTag::FlattenString => {
        let str = FlattenString::from(repr);
        let mut array = FixedU16CodePointArray::new(context, str.len() + 1);
        array.push(self.at(0).unwrap());
        array.append(str.str());
        return JsString::new(context, array);
      }
      ShapeTag::StringRope => {
        let array = fixed_array!(type: u16, context: context, capacity: 1, self.at(0).unwrap());
        let left = StringRope::new(context, array);
        return left.concat(context, repr);
      }
      _ => unreachable!(),
    };
  }

  fn slice(&self, context: impl Context, start_index: usize, end_index: usize) -> FixedU16CodePointArray {
    if start_index == 0 && end_index == 1 {
      return fixed_array!(type: u16, context: context, capacity: 1, self.at(0).unwrap());
    }
    return context.empty_internal_array();
  }

  fn flatten(&mut self, context: impl AllocationOnlyContext) -> FixedU16CodePointArray {
    return fixed_array!(type: u16, context: context, capacity: 1, self.at(0).unwrap());
  }

  fn len(&self) -> usize {
    return 1;
  }
}
