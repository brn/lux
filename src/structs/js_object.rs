use super::cell::*;
use super::hash_map::{ContextHash, ContextHashMut};
use super::js_globals::*;
use super::repr::*;
use super::shape::Shape;
use super::string::{FixedU16CodePointArray, JsString};
use crate::context::*;
use crate::def::*;
use num_derive::FromPrimitive;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

#[repr(C)]
#[derive(Copy, Clone)]
pub struct JsVal(HeapLayout<UintPtr>);
impl_object!(JsVal, HeapLayout<UintPtr>);

impl JsVal {
  pub fn is_js_null(&self) -> bool {
    return self.shape() == Shape::null();
  }

  pub fn is_js_undefined(&self) -> bool {
    return self.shape() == Shape::undefined();
  }

  pub fn is_js_boolean(&self) -> bool {
    return self.shape() == Shape::boolean();
  }

  pub fn is_js_string(&self) -> bool {
    return self.shape() == Shape::string();
  }

  pub fn is_js_symbol(&self) -> bool {
    return self.shape() == Shape::symbol();
  }

  pub fn is_js_object(&self) -> bool {
    return self.shape() == Shape::object();
  }

  pub fn is_js_array(&self) -> bool {
    return self.shape() == Shape::array();
  }
}

macro_rules! _def_jsobj_convert {
  ($type:tt, $name:ty) => {
    impl From<$type> for $name {
      fn from(a: $type) -> $name {
        let repr: Repr = a.into();
        type X = $name;
        return X::from(repr);
      }
    }
    impl From<$name> for $type {
      fn from(a: $name) -> $type {
        let repr: Repr = a.into();
        return $type::from(repr);
      }
    }
  };
}

_def_jsobj_convert!(JsVal, JsNull);
_def_jsobj_convert!(JsVal, JsUndefined);
_def_jsobj_convert!(JsVal, JsBoolean);
_def_jsobj_convert!(JsVal, JsString);

#[repr(C)]
#[derive(Copy, Clone)]
pub struct NameLayout {
  hash: u64,
  name: BareHeapLayout<Repr>,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct Name(HeapLayout<NameLayout>);
impl_object!(Name, HeapLayout<NameLayout>);

impl Name {
  const SIZE: usize = Cell::SIZE + std::mem::size_of::<NameLayout>();
  pub fn new(context: &mut impl Context, name: Repr) -> Name {
    let mut layout = HeapLayout::<NameLayout>::new(context, Name::SIZE, Shape::name());
    layout.name.set(name);
    Name::calc_hash(layout, context);
    return Name(layout);
  }

  pub fn from_utf8(context: &mut impl AllocationOnlyContext, name: &str) -> Name {
    let mut layout = HeapLayout::<NameLayout>::new(context, Name::SIZE, Shape::name());
    let array = FixedU16CodePointArray::from_utf8(context, name);
    layout.name.set(Repr::from(JsString::new(context, array)));
    layout.hash = Name::calc_hash_from_fixed_array(context, array);
    return Name(layout);
  }

  pub fn hash(&self) -> u64 {
    return self.hash;
  }

  pub fn equals(&self, context: &mut impl Context, other: Name) -> bool {
    if Cell::from(self.name.handle()).shape() == Shape::string() {
      return self.to_string(context) == other.to_string(context);
    }
    unreachable!();
  }

  fn calc_hash(mut layout: HeapLayout<NameLayout>, context: &mut impl Context) {
    let mut hasher = DefaultHasher::new();
    if Cell::from(layout.name.handle()).shape() == Shape::string() {
      layout.hash = JsString::from(layout.name.handle())
        .flatten(context)
        .context_hash(context, &mut hasher);
    }
    unreachable!();
  }

  fn calc_hash_from_fixed_array(context: &mut impl AllocationOnlyContext, array: FixedU16CodePointArray) -> u64 {
    let mut hasher = DefaultHasher::new();
    array.hash(&mut hasher);
    return hasher.finish();
  }

  fn to_string(&self, context: &mut impl Context) -> FixedU16CodePointArray {
    if Cell::from(self.name.handle()).shape() == Shape::string() {
      return JsString::from(self.name.handle()).flatten(context);
    }
    unreachable!();
  }
}

impl std::hash::Hash for Name {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    if self.shape() == Shape::string() {
      let mut c = isolate::context();
      JsString::from(self.name.handle()).flatten(&mut c).hash(state);
    }
    unreachable!();
  }
}

impl ContextHash for Name {
  fn context_hash<H: std::hash::Hasher>(&self, _: &mut impl Context, _: &mut H) -> u64 {
    return self.hash;
  }
}

impl ContextHashMut for Name {
  fn context_hash_mut<H: std::hash::Hasher>(&mut self, _: &mut impl Context, _: &mut H) -> u64 {
    return self.hash;
  }
}

impl std::cmp::PartialEq for Name {
  fn eq(&self, other: &Name) -> bool {
    let shape_a = Cell::from(self.name.handle()).shape();
    let shape_b = Cell::from(other.name.handle()).shape();
    if shape_a != shape_b {
      return false;
    }
    if shape_a == Shape::string() {
      let mut context = isolate::context();
      return JsString::from(self.name.handle()).flatten(&mut context)
        == JsString::from(other.name.handle()).flatten(&mut context);
    }
    unreachable!();
  }
}

impl std::cmp::Eq for Name {}

#[cfg(test)]
pub mod testing {
  use super::super::cell::*;
  use super::super::repr::Repr;
  use super::super::shape::Shape;
  use crate::def::*;
  use std::alloc::{alloc, Layout};
  use std::mem::size_of;

  #[repr(C)]
  #[derive(Copy, Clone)]
  pub struct TestObjectBody {
    value: u32,
  }

  #[repr(C)]
  #[derive(Copy, Clone)]
  pub struct TestObject(HeapLayout<TestObjectBody>);
  impl_object!(TestObject, HeapLayout<TestObjectBody>);

  impl TestObject {
    pub const TYPE: Shape = Shape::boolean();
    pub fn new(value: u32) -> TestObject {
      let heap = unsafe { alloc(Layout::from_size_align(Cell::SIZE + size_of::<u8>(), ALIGNMENT).unwrap()) };
      let mut layout =
        HeapLayout::<TestObjectBody>::new_into_heap(heap, Cell::SIZE + size_of::<u8>(), Shape::undefined());
      layout.value = value;
      return TestObject(layout);
    }
    pub fn value(&self) -> u32 {
      return self.0.value;
    }
    pub fn set_value(&mut self, value: u32) {
      self.value = value;
    }
    fn byte_length(&self) -> usize {
      return size_of::<u8>();
    }

    fn wrap(heap: Addr) -> TestObject {
      return TestObject(HeapLayout::<TestObjectBody>::wrap(heap));
    }
  }
}
