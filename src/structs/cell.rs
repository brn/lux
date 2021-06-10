use super::object_record::{FullObjectRecord, ObjectRecord, ObjectSkin};
use super::repr::*;
use super::shape::Shape;
use crate::context::AllocationOnlyContext;
use crate::def::*;
use crate::utility::*;
use std::marker::PhantomData;
use std::mem::size_of;
use std::ops::{Deref, DerefMut};

pub trait HeapBody: Copy + Default {}
impl<T: Copy + Default> HeapBody for T {}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct HeapLayout<T: HeapBody>(usize, PhantomData<T>);
pub const HEAP_LAYOUT_SIZE: u32 = size_of::<usize>() as u32;

#[derive(Copy, Clone, Default)]
pub struct VoidHeapBody;

impl<T: HeapBody> HeapLayout<T> {
  #[inline(always)]
  pub fn new_raw(mut context: impl AllocationOnlyContext, size: u32) -> HeapLayout<T> {
    let heap = context.allocate(size as usize);
    return HeapLayout::<T>(heap as usize, PhantomData);
  }

  #[inline(always)]
  pub fn new_into_raw_heap(heap: Addr) -> HeapLayout<T> {
    return HeapLayout::<T>(heap as usize, PhantomData);
  }

  #[inline(always)]
  pub fn new(context: impl AllocationOnlyContext, object_record: ObjectRecord) -> HeapLayout<T> {
    let heap = Cell::init_heap(context, object_record);
    let cell = Cell::from(heap);
    unsafe {
      *(cell.get_body() as *mut T) = T::default();
    };
    return HeapLayout::<T>(heap as usize, PhantomData);
  }

  #[inline(always)]
  pub fn new_into_heap(heap: Addr, object_record: ObjectRecord) -> HeapLayout<T> {
    let heap = Cell::init_into_heap(heap, object_record);
    let cell = Cell::from(heap);
    unsafe {
      *(cell.get_body() as *mut T) = T::default();
    };
    return HeapLayout::<T>(heap as usize, PhantomData);
  }

  #[inline(always)]
  pub fn persist(context: impl AllocationOnlyContext, object_record: ObjectRecord) -> HeapLayout<T> {
    let heap = Cell::init_persistent_heap(context, object_record);
    let cell = Cell::from(heap);
    unsafe {
      *(cell.get_body() as *mut T) = T::default();
    };
    return HeapLayout::<T>(heap as usize, PhantomData);
  }

  #[inline(always)]
  pub fn new_object(context: impl AllocationOnlyContext, object_record: FullObjectRecord) -> HeapLayout<T> {
    let heap = Cell::init_object(context, object_record);
    let cell = Cell::from(heap);
    unsafe {
      *(cell.get_body() as *mut T) = T::default();
    };
    return HeapLayout::<T>(heap as usize, PhantomData);
  }

  #[inline(always)]
  pub fn new_object_into_heap(
    context: impl AllocationOnlyContext,
    heap: Addr,
    object_record: FullObjectRecord,
  ) -> HeapLayout<T> {
    let heap = Cell::init_object_into_heap(context, heap, object_record);
    let cell = Cell::from(heap);
    unsafe {
      *(cell.get_body() as *mut T) = T::default();
    };
    return HeapLayout::<T>(heap as usize, PhantomData);
  }

  #[inline(always)]
  pub fn persist_object(context: impl AllocationOnlyContext, object_record: FullObjectRecord) -> HeapLayout<T> {
    let heap = Cell::init_persistent_object(context, object_record);
    let cell = Cell::from(heap);
    unsafe {
      *(cell.get_body() as *mut T) = T::default();
    };
    return HeapLayout::<T>(heap as usize, PhantomData);
  }

  #[inline(always)]
  pub fn wrap(heap: Addr) -> HeapLayout<T> {
    return HeapLayout::<T>(heap as usize, PhantomData);
  }

  #[inline(always)]
  pub fn null() -> HeapLayout<T> {
    return HeapLayout::<T>(std::ptr::null_mut() as Addr as usize, PhantomData);
  }

  #[inline(always)]
  pub fn is_null(&self) -> bool {
    return self.addr().is_null();
  }

  #[inline(always)]
  pub fn as_ref(&self) -> &T {
    return unsafe { std::mem::transmute::<Addr, &T>(self.cell().get_body()) };
  }

  #[inline(always)]
  pub fn as_ref_mut(&mut self) -> &mut T {
    return unsafe { std::mem::transmute::<Addr, &mut T>(self.cell().get_body()) };
  }

  #[inline(always)]
  pub fn as_addr(&self) -> Addr {
    return self.addr();
  }

  #[inline(always)]
  pub fn out(&mut self) {
    self.0 = std::ptr::null_mut() as Addr as usize;
  }

  #[inline(always)]
  pub fn set(this: &HeapLayout<T>, value: &T) {
    unsafe {
      *(this.addr().offset(Cell::SIZE as isize) as *mut T) = *value;
    };
  }

  #[inline(always)]
  pub fn size(this: &HeapLayout<T>) -> usize {
    return this.cell().size() as usize;
  }

  #[inline(always)]
  pub fn cell(&self) -> Cell {
    return Cell::from(self.addr());
  }

  #[inline(always)]
  pub fn shape(&self) -> Shape {
    return self.cell().shape();
  }

  #[inline(always)]
  pub fn set_flag(this: &mut Self) {
    this.0 = this.0 | 1;
  }

  #[inline(always)]
  pub fn unset_flag(this: &mut Self) {
    this.0 = this.0 & !1_usize;
  }

  #[inline(always)]
  pub fn get_flag(this: &Self) -> bool {
    return this.0 & 1 == 1;
  }

  #[inline(always)]
  fn addr(&self) -> Addr {
    return self.0 as Addr;
  }
}

impl<T: HeapBody> ObjectSkin for HeapLayout<T> {
  #[inline(always)]
  fn set_record(&mut self, r: ObjectRecord) {
    return self.cell().set_record(r);
  }

  #[inline(always)]
  fn record(&self) -> ObjectRecord {
    return self.cell().record();
  }
}

impl<T: HeapBody> Deref for HeapLayout<T> {
  type Target = T;
  #[inline(always)]
  fn deref(&self) -> &T {
    return self.as_ref();
  }
}

impl<T: HeapBody> DerefMut for HeapLayout<T> {
  #[inline(always)]
  fn deref_mut(&mut self) -> &mut T {
    return self.as_ref_mut();
  }
}

impl<T: HeapBody> std::fmt::Debug for HeapLayout<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return write!(
      f,
      "{:?}
{:?} {{
  header_addr: {:p},
  body_addr: {:p}
}}",
      1, //      self.cell().get_header(),
      std::any::type_name::<HeapLayout<T>>(),
      self.0 as Addr,
      self.cell().get_body()
    );
  }
}

impl<T: HeapBody> std::fmt::Display for HeapLayout<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return write!(f, "{:?}", self);
  }
}

pub trait HeapObject: Into<Repr> + Copy + From<Addr> {
  fn raw_heap(&self) -> Addr;

  fn raw_address(&self) -> usize {
    return self.raw_heap() as usize;
  }

  #[inline(always)]
  fn size(&self) -> u32 {
    return self.cell().size();
  }

  #[inline(always)]
  fn shape(&self) -> Shape {
    return self.cell().shape();
  }

  #[inline(always)]
  fn cell(&self) -> Cell {
    return Cell::from(self.raw_heap());
  }

  #[inline(always)]
  fn is_null(&self) -> bool {
    return self.raw_heap() == std::ptr::null_mut();
  }

  #[inline(always)]
  fn is_same_heap_object(&self, other: impl HeapObject) -> bool {
    return self.raw_heap() == other.raw_heap();
  }

  #[inline(always)]
  fn set_data_field(this: &mut Self, index: usize) {
    this.cell().set_data_field(index);
  }

  #[inline(always)]
  fn assign_data_field(this: &mut Self, value: u64) {
    this.cell().assign_data_field(value);
  }

  #[inline(always)]
  fn unset_data_field(this: &mut Self, index: usize) {
    this.cell().unset_data_field(index);
  }

  #[inline(always)]
  fn get_data_field(this: &Self, index: usize) -> bool {
    return this.cell().get_data_field(index);
  }

  #[inline(always)]
  fn get_data_field_bits(this: &Self) -> u64 {
    return this.cell().get_data_field_bits();
  }
}

macro_rules! impl_default {
  ($name:tt) => {
    impl Default for $name {
      fn default() -> $name {
        return $name::from(crate::structs::Repr::invalid());
      }
    }
  };
  ($name:ident<$($type:tt : $bound:ident),+$(,)?>) => {
    impl<$($type: $bound),*> Default for $name<$($type,)*> {
      fn default() -> $name<$($type,)*> {
        return $name::from(crate::structs::Repr::invalid());
      }
    }
  };
}

macro_rules! _priv_impl_heap_object_body {
  () => {
    #[inline(always)]
    fn raw_heap(&self) -> crate::def::Addr {
      return self.0.as_addr();
    }
  };
}

macro_rules! impl_heap_object {
  ($name:ident) => {
    impl crate::structs::HeapObject for $name {
      _priv_impl_heap_object_body!();
    }
  };
  ($name:ident<$($type:tt : $bound:ident),+$(,)?>) => {
    impl<$($type : $bound),*> HeapObject for $name<$($type,)*> {
      _priv_impl_heap_object_body!();
    }
  };
}

macro_rules! impl_from_addr {
  ($name:tt, $layout:tt) => {
    impl From<crate::def::Addr> for $name {
      fn from(a: crate::def::Addr) -> $name {
        return $name($layout::wrap(a));
      }
    }
  };
  ($name:ident<$($type:tt : $bound:ident),+$(,)?>, $layout:tt, $($args:expr),+) => {
    impl<$($type : $bound),*> From<crate::def::Addr> for $name<$($type,)*> {
      fn from(a: crate::def::Addr) -> Self {
        return Self($layout::wrap(a), $($args ,)*);
      }
    }
  };
  ($name:ident<$($type:tt : $bound:ident),+$(,)?>, $layout:tt) => {
    impl<$($type : $bound),*> From<crate::def::Addr> for $name<$($type,)*> {
      fn from(a: crate::def::Addr) -> Self {
        return Self($layout::wrap(a));
      }
    }
  }
}

macro_rules! impl_into_addr {
  ($name:tt) => {
    impl Into<crate::def::Addr> for $name {
      fn into(self) -> crate::def::Addr {
        return self.0.as_addr();
      }
    }
  };
  ($name:ident<$($type:tt : $bound:ident),+$(,)?>) => {
    impl<$($type: $bound),*> Into<crate::def::Addr> for $name<$($type,)*> {
      fn into(self) -> crate::def::Addr {
        return self.0.as_addr();
      }
    }
  };
}

macro_rules! impl_deref_heap {
  ($name:tt, $layout:ident, $body:ty) => {
    impl std::ops::Deref for $name {
      type Target = $layout<$body>;
      fn deref(&self) -> &$layout<$body> {
        return &self.0;
      }
    }

    impl std::ops::DerefMut for $name {
      fn deref_mut(&mut self) -> &mut $layout<$body> {
        return &mut self.0;
      }
    }
  };
  ($name:ident<$($type:tt: $bound:ident),+$(,)?>, $layout:ident, $body:ty) => {
    impl<$($type : $bound),*> std::ops::Deref for $name<$($type,)+> {
      type Target = $layout<$body>;
      fn deref(&self) -> &$layout<$body> {
        return &self.0;
      }
    }
    impl<$($type : $bound,)*> std::ops::DerefMut for $name<$($type,)+> {
      fn deref_mut(&mut self) -> &mut $layout<$body> {
        return &mut self.0;
      }
    }
  };
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct BareHeapLayout<T: Copy>(Addr, PhantomData<T>);
impl<T: Copy> BareHeapLayout<T> {
  pub fn as_addr(&self) -> Addr {
    return self.0;
  }

  pub fn wrap(value: Addr) -> BareHeapLayout<T> {
    return BareHeapLayout::<T>(value, PhantomData);
  }

  pub fn as_ref(&self) -> &T {
    return unsafe { &*(self.0 as *mut T) };
  }

  pub fn as_mut(&mut self) -> &mut T {
    return unsafe { &mut *(self.0 as *mut T) };
  }

  pub fn raw_heap(&self) -> Addr {
    return self.0;
  }

  pub fn is_null(&self) -> bool {
    return self.0.is_null();
  }
}

impl<T: Copy> Deref for BareHeapLayout<T> {
  type Target = T;
  fn deref(&self) -> &Self::Target {
    return self.as_ref();
  }
}

impl<T: Copy> DerefMut for BareHeapLayout<T> {
  fn deref_mut(&mut self) -> &mut T {
    return self.as_mut();
  }
}

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct CellLayout {
  object_record: ObjectRecord,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct Cell(BareHeapLayout<CellLayout>);

impl Cell {
  pub const TYPE: Shape = Shape::cell();
  pub const SIZE: usize = size_of::<CellLayout>();

  #[inline(always)]
  pub fn init_heap(mut context: impl AllocationOnlyContext, object_record: ObjectRecord) -> Addr {
    let heap = context.allocate((object_record.size() + size_of::<CellLayout>() as u32) as usize);
    return Cell::init_into_heap(heap, object_record);
  }

  #[inline(always)]
  pub fn init_persistent_heap(mut context: impl AllocationOnlyContext, object_record: ObjectRecord) -> Addr {
    let heap = context.allocate_persist((object_record.size() + size_of::<CellLayout>() as u32) as usize);
    return Cell::init_into_heap(heap, object_record);
  }

  #[inline(always)]
  pub fn init_into_heap(heap: Addr, object_record: ObjectRecord) -> Addr {
    let mut layout = BareHeapLayout::<CellLayout>::wrap(heap);
    layout.object_record = object_record;
    return heap;
  }

  #[inline(always)]
  pub fn init_object(mut context: impl AllocationOnlyContext, object_record: FullObjectRecord) -> Addr {
    let heap = context.allocate((object_record.size() + size_of::<CellLayout>() as u32) as usize);
    return Cell::init_object_into_heap(context, heap, object_record);
  }

  #[inline(always)]
  pub fn init_persistent_object(mut context: impl AllocationOnlyContext, object_record: FullObjectRecord) -> Addr {
    let heap = context.allocate_persist((object_record.size() + size_of::<CellLayout>() as u32) as usize);
    return Cell::init_object_into_heap(context, heap, object_record);
  }

  pub fn init_object_into_heap(
    context: impl AllocationOnlyContext,
    heap: Addr,
    object_record: FullObjectRecord,
  ) -> Addr {
    let mut layout = BareHeapLayout::<CellLayout>::wrap(heap);
    layout.object_record = object_record.into();
    return heap;
  }

  #[inline(always)]
  pub fn size(&self) -> u32 {
    return self.record().size();
  }

  #[inline(always)]
  pub fn shape(&self) -> Shape {
    return self.record().shape();
  }

  #[inline(always)]
  pub fn get_body(&self) -> Addr {
    return unsafe { self.as_addr().offset(size_of::<CellLayout>() as isize) };
  }

  #[inline(always)]
  pub fn set_data_field(&mut self, index: usize) {
    self.record().data_field().set(index);
  }

  #[inline(always)]
  pub fn assign_data_field(&mut self, value: u64) {
    self.record().data_field().assign(value);
  }

  #[inline(always)]
  pub fn unset_data_field(&mut self, index: usize) {
    self.record().data_field().unset(index);
  }

  #[inline(always)]
  pub fn get_data_field(&self, index: usize) -> bool {
    return self.record().data_field().get(index);
  }

  #[inline(always)]
  pub fn get_data_field_bits(&self) -> u64 {
    return self.record().data_field().bits();
  }
}

impl ObjectSkin for Cell {
  #[inline(always)]
  fn set_record(&mut self, r: ObjectRecord) {
    return self.object_record = r;
  }

  #[inline(always)]
  fn record(&self) -> ObjectRecord {
    return ObjectRecord::from(self.object_record);
  }
}

macro_rules! impl_bare_object {
  ($name:ident, $layout:ident<$body:ty>) => {
    impl_from_addr!($name, $layout);
    impl_into_addr!($name);
    impl_repr_convertion!($name, $layout);
    impl_default!($name);
    impl_deref_heap!($name, $layout, $body);
  };
}

macro_rules! impl_object {
  ($name:ident, $layout:ident<$body:ty>) => {
    impl_from_addr!($name, $layout);
    impl_into_addr!($name);
    impl_heap_object!($name);
    impl_repr_convertion!($name, $layout);
    impl_deref_heap!($name, $layout, $body);
    impl_default!($name);
  };
  ($name:ident<$($type:tt : $bound:ident),+$(,)?>, $layout:ident<$body:ty>) => {
    impl_from_addr!($name<$($type: $bound),+>, $layout);
    impl_into_addr!($name<$($type: $bound),+>);
    impl_heap_object!($name<$($type: $bound),+>);
    impl_repr_convertion!($name<$($type: $bound),+>,$layout);
    impl_deref_heap!($name<$($type: $bound),+>, $layout, $body);
    impl_default!($name<$($type: $bound),+>);
  };
  ($name:ident<$($type:tt : $bound:ident),+$(,)?>, $layout:ident<$body:ty>, $($args:expr),+) => {
    impl_from_addr!($name<$($type: $bound),+>, $layout, $($args),*);
    impl_into_addr!($name<$($type: $bound),+>);
    impl_heap_object!($name<$($type: $bound),+>);
    impl_repr_convertion!($name<$($type: $bound),+>,$layout, $($args),*);
    impl_deref_heap!($name<$($type: $bound),+>, $layout, $body);
    impl_default!($name<$($type: $bound),+>);
  };
}

impl_bare_object!(Cell, BareHeapLayout<CellLayout>);
