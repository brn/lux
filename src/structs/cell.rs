use super::repr::*;
use super::shadow_class::{ShadowClass, ShadowInstance};
use super::shape::Shape;
use super::string::JsString;
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

#[derive(Copy, Clone, Default)]
pub struct VoidHeapBody;

impl<T: HeapBody> HeapLayout<T> {
  #[inline(always)]
  pub fn new(context: impl AllocationOnlyContext, size: usize, shape: Shape) -> HeapLayout<T> {
    let heap = Cell::init_heap(context, size, shape);
    let cell = Cell::from(heap);
    unsafe {
      *(cell.get_body() as *mut T) = T::default();
    };
    return HeapLayout::<T>(heap as usize, PhantomData);
  }

  #[inline(always)]
  pub fn new_into_heap(heap: Addr, size: usize, shape: Shape) -> HeapLayout<T> {
    let heap = Cell::init_into_heap(heap, size, shape);
    let cell = Cell::from(heap);
    unsafe {
      *(cell.get_body() as *mut T) = T::default();
    };
    return HeapLayout::<T>(heap as usize, PhantomData);
  }

  #[inline(always)]
  pub fn persist(context: impl AllocationOnlyContext, size: usize, shape: Shape) -> HeapLayout<T> {
    let heap = Cell::init_persistent_heap(context, size, shape);
    let cell = Cell::from(heap);
    unsafe {
      *(cell.get_body() as *mut T) = T::default();
    };
    return HeapLayout::<T>(heap as usize, PhantomData);
  }

  #[inline(always)]
  pub fn new_object(context: impl AllocationOnlyContext, size: usize, shape: Shape) -> HeapLayout<T> {
    let heap = Cell::init_object(context, size, shape);
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
    size: usize,
    shape: Shape,
  ) -> HeapLayout<T> {
    let heap = Cell::init_object_into_heap(context, heap, size, shape);
    let cell = Cell::from(heap);
    unsafe {
      *(cell.get_body() as *mut T) = T::default();
    };
    return HeapLayout::<T>(heap as usize, PhantomData);
  }

  #[inline(always)]
  pub fn persist_object(context: impl AllocationOnlyContext, size: usize, shape: Shape) -> HeapLayout<T> {
    let heap = Cell::init_persistent_object(context, size, shape);
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
    return unsafe { std::mem::transmute::<Addr, &T>(self.addr().offset(self.cell().size() as isize)) };
  }

  #[inline(always)]
  pub fn as_ref_mut(&mut self) -> &mut T {
    return unsafe { std::mem::transmute::<Addr, &mut T>(self.addr().offset(self.cell().size() as isize)) };
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
      *(this.addr().offset(this.cell().size() as isize) as *mut T) = *value;
    };
  }

  #[inline(always)]
  pub fn size(this: &HeapLayout<T>) -> usize {
    return this.cell().get_header().size() as usize;
  }

  #[inline(always)]
  pub fn cell(&self) -> Cell {
    return Cell::from(self.addr());
  }

  #[inline(always)]
  pub fn shape(&self) -> Shape {
    return self.cell().get_header().shape();
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

impl<T: HeapBody> ShadowInstance for HeapLayout<T> {
  #[inline(always)]
  fn class(&self) -> ShadowClass {
    assert!(self.cell().has_shadow_class());
    return self.cell().class();
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
      self.cell().get_header(),
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

  #[inline(always)]
  fn size(&self) -> usize {
    return self.cell().get_header().size() as usize;
  }

  #[inline(always)]
  fn shape(&self) -> Shape {
    return self.cell().get_header().shape();
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
  fn get_data_field(this: &Self) -> MaskedBitsetMut<u64> {
    return unsafe { (*(this.raw_heap() as *mut Header)).data() };
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
#[derive(Copy, Clone, Default)]
pub struct Header {
  ///
  /// Use as object size information and used as forwarded pointer and mark bit.
  ///
  /// |---data----|--size---|--shadow_class bit--|--size tag--|--type tag--|--mark bit--|
  /// |---23bit---|--32bit--|--------1bit--------|----1bit----|----6bit----|----1bit----|
  ///
  field: Bitset<u64>,
}

const MARK_BIT_START: usize = 1;
const MARK_BIT_SIZE: usize = 1;
const TYPE_TAG_START: usize = MARK_BIT_START + MARK_BIT_SIZE;
const TYPE_TAG_SIZE: usize = 6;
const SIZE_TAG_START: usize = TYPE_TAG_START + TYPE_TAG_SIZE;
const SIZE_TAG_SIZE: usize = 1;
const SHADOW_CLASS_BIT_START: usize = SIZE_TAG_START + SIZE_TAG_SIZE;
const SHADOW_CLASS_BIT_SIZE: usize = 1;
const SIZE_FIELD_START: usize = SHADOW_CLASS_BIT_START + SHADOW_CLASS_BIT_SIZE;
const SIZE_FIELD_SIZE: usize = 32;
const DATA_FIELD_START: usize = SIZE_FIELD_START + SIZE_FIELD_SIZE;
const DATA_FIELD_SIZE: usize = 23;

impl Header {
  #[inline]
  fn init(&mut self) {
    self.field.assign(0);
  }

  #[inline]
  pub fn set_size(&mut self, size: u64) {
    debug_assert!(size <= 0x7fffffffffffff_u64, "Size must be less than 55bit integer");
    self.field.set(SIZE_TAG_START);
    let mut range = self.field.mask_lower_mut(SIZE_FIELD_START);
    range.assign(size as u64);
  }

  #[inline]
  pub fn size(&self) -> u32 {
    if self.is_size_used_as_size() {
      let mask = self.field.mask_lower(SIZE_FIELD_START);
      return mask.bits() as u32;
    }
    return 0;
  }

  #[inline]
  pub fn data(&mut self) -> MaskedBitsetMut<u64> {
    return self.field.mask_lower_mut(DATA_FIELD_START);
  }

  #[inline]
  pub fn set_forwarded_pointer(&mut self, addr: *mut Byte) {
    self.field.unset(SIZE_TAG_START);
    let mut mask = self.field.mask_lower_mut(SIZE_FIELD_START);
    mask.assign(addr as u64);
  }

  #[inline]
  pub fn forwarded_pointer(&self) -> *mut Byte {
    if !self.is_size_used_as_size() {
      return self.field.mask_lower(SIZE_FIELD_START).bits() as *mut Byte;
    }
    return 0xdeadbeef as *mut Byte;
  }

  #[inline]
  pub fn set_shadow_class(&mut self) {
    self.field.set(SHADOW_CLASS_BIT_START);
  }

  #[inline]
  pub fn has_shadow_class(&self) -> bool {
    return self.field.get(SHADOW_CLASS_BIT_START);
  }

  #[inline]
  pub fn mark(&mut self) {
    self.field.set(MARK_BIT_START);
  }

  #[inline]
  pub fn unmark(&mut self) {
    self.field.unset(MARK_BIT_START);
  }

  #[inline]
  pub fn is_marked(&self) -> bool {
    return self.field.get(MARK_BIT_START);
  }

  #[inline]
  pub fn set_shape(&mut self, shape: Shape) {
    let mut mask = self.field.mask_range_mut(TYPE_TAG_START, SIZE_TAG_START);
    mask.assign(shape.into());
  }

  #[inline]
  pub fn shape(&self) -> Shape {
    let mask = self.field.mask_range(TYPE_TAG_START, SIZE_TAG_START);
    return Shape::from_tag(mask.bits() as u8);
  }

  #[inline]
  fn is_size_used_as_size(&self) -> bool {
    return self.field.get(SIZE_TAG_START);
  }
}

impl std::fmt::Debug for Header {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return write!(
      f,
      "
{:?} {{
  size: {:?},
  shape: {:?},
  mark: {:?},
  is_size_used_as_size: {:?},
  has_shadow_class: {:?}
}}",
      std::any::type_name::<Header>(),
      self.size(),
      self.shape(),
      self.is_marked(),
      self.is_size_used_as_size(),
      self.has_shadow_class()
    );
  }
}

#[cfg(test)]
mod header_tests {
  use super::*;
  #[test]
  fn header_set_size_test() {
    let mut h = Header {
      field: Bitset::<u64>::new(),
    };
    h.set_size(4294967295);
    assert_eq!(h.size(), 4294967295);
  }

  #[test]
  fn header_set_forwarded_pointer() {
    let p = 0xdeadbeef as *mut Byte;
    let mut h = Header {
      field: Bitset::<u64>::new(),
    };
    h.set_forwarded_pointer(p);
    assert_eq!(h.forwarded_pointer() as usize, p as usize);
  }

  #[test]
  fn header_mark() {
    let mut h = Header {
      field: Bitset::<u64>::new(),
    };
    h.mark();
    assert_eq!(h.is_marked(), true);
  }

  #[test]
  fn header_unmark() {
    let mut h = Header {
      field: Bitset::<u64>::new(),
    };
    h.mark();
    assert_eq!(h.is_marked(), true);
    h.unmark();
    assert_eq!(h.is_marked(), false);
  }

  #[test]
  fn header_set_shape() {
    let mut h = Header {
      field: Bitset::<u64>::new(),
    };
    h.set_shape(Shape::undefined());
    assert_eq!(h.shape(), Shape::undefined());
  }
}

#[repr(C)]
#[derive(Copy, Clone, Debug, Default)]
pub struct CellLayout {
  header: Header,
}

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct CellWithShadowClassLayout {
  header: Header,
  shadow_class: ShadowClass,
}

impl From<Addr> for CellLayout {
  #[inline(always)]
  fn from(a: Addr) -> CellLayout {
    unreachable!();
  }
}

impl Into<Addr> for CellLayout {
  #[inline(always)]
  fn into(self) -> Addr {
    unreachable!();
  }
}

impl From<Addr> for CellWithShadowClassLayout {
  #[inline(always)]
  fn from(a: Addr) -> CellWithShadowClassLayout {
    unreachable!();
  }
}

impl Into<Addr> for CellWithShadowClassLayout {
  #[inline(always)]
  fn into(self) -> Addr {
    unreachable!();
  }
}

#[repr(C)]
#[derive(Copy, Clone)]
struct BareHeapLayout<T: Copy>(Addr, PhantomData<T>);
impl<T: Copy> BareHeapLayout<T> {
  fn as_addr(&self) -> Addr {
    return self.0;
  }

  fn wrap(heap: Addr) -> BareHeapLayout<T> {
    return BareHeapLayout::<T>(heap, PhantomData);
  }

  fn as_ref(&self) -> &T {
    return unsafe { &*(self.0 as *mut T) };
  }

  fn as_ref_mut(&mut self) -> &mut T {
    return unsafe { &mut *(self.0 as *mut T) };
  }
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct Cell(BareHeapLayout<CellLayout>);

impl Cell {
  pub const TYPE: Shape = Shape::cell();

  pub const OBJECT_SIZE: usize = size_of::<CellWithShadowClassLayout>();

  #[inline(always)]
  pub fn init_heap(mut context: impl AllocationOnlyContext, size: usize, shape: Shape) -> Addr {
    let heap = context.allocate(size + size_of::<CellLayout>());
    return Cell::init_into_heap(heap, size, shape);
  }

  #[inline(always)]
  pub fn init_persistent_heap(mut context: impl AllocationOnlyContext, size: usize, shape: Shape) -> Addr {
    let heap = context.allocate_persist(size + size_of::<CellLayout>());
    return Cell::init_into_heap(heap, size, shape);
  }

  #[inline(always)]
  pub fn init_into_heap(heap: Addr, size: usize, shape: Shape) -> Addr {
    let mut layout = BareHeapLayout::<CellLayout>::wrap(heap);
    let cell_layout = layout.as_ref_mut();
    cell_layout.header.init();
    cell_layout.header.set_size(size as u64);
    cell_layout.header.set_shape(shape);
    return heap;
  }

  #[inline(always)]
  pub fn init_object(mut context: impl AllocationOnlyContext, size: usize, shape: Shape) -> Addr {
    let heap = context.allocate(size + size_of::<CellWithShadowClassLayout>());
    return Cell::init_object_into_heap(context, heap, size, shape);
  }

  #[inline(always)]
  pub fn init_persistent_object(mut context: impl AllocationOnlyContext, size: usize, shape: Shape) -> Addr {
    let heap = context.allocate_persist(size + size_of::<CellWithShadowClassLayout>());
    return Cell::init_object_into_heap(context, heap, size, shape);
  }

  pub fn init_object_into_heap(context: impl AllocationOnlyContext, heap: Addr, size: usize, shape: Shape) -> Addr {
    let mut layout = BareHeapLayout::<CellWithShadowClassLayout>::wrap(heap);
    let mut cell_layout = layout.as_ref_mut();
    cell_layout.header.init();
    cell_layout.header.set_size(size as u64);
    cell_layout.header.set_shadow_class();
    cell_layout.header.set_shape(shape);
    cell_layout.shadow_class = ShadowClass::empty(context);
    return heap;
  }

  #[inline(always)]
  pub fn get_header(&self) -> &Header {
    return &self.layout().header;
  }

  #[inline(always)]
  pub fn get_header_mut(&mut self) -> &mut Header {
    return &mut self.layout_mut().header;
  }

  #[inline(always)]
  pub fn get_body(&self) -> Addr {
    return unsafe { self.0.as_addr().offset(self.size() as isize) };
  }

  #[inline(always)]
  pub fn get_data_field(&mut self) -> MaskedBitsetMut<u64> {
    return self.get_header_mut().data();
  }

  #[inline(always)]
  pub fn size(&self) -> usize {
    return if self.has_shadow_class() {
      size_of::<CellWithShadowClassLayout>()
    } else {
      size_of::<CellLayout>()
    };
  }

  #[inline(always)]
  pub fn has_shadow_class(&self) -> bool {
    return self.get_header().has_shadow_class();
  }

  #[inline(always)]
  fn layout_mut(&mut self) -> &mut CellLayout {
    return self.0.as_ref_mut();
  }

  #[inline(always)]
  fn layout(&self) -> &CellLayout {
    return self.0.as_ref();
  }

  #[inline(always)]
  fn object_layout_mut(&mut self) -> &mut CellWithShadowClassLayout {
    return unsafe { &mut *(self.0.as_addr() as *mut CellWithShadowClassLayout) };
  }

  #[inline(always)]
  fn object_layout(&self) -> &CellWithShadowClassLayout {
    return unsafe { &*(self.0.as_addr() as *mut CellWithShadowClassLayout) };
  }
}

impl ShadowInstance for Cell {
  #[inline(always)]
  fn class(&self) -> ShadowClass {
    assert!(self.get_header().has_shadow_class());
    return unsafe { self.object_layout().shadow_class };
  }
}

macro_rules! impl_bare_object {
  ($name:ident, $layout:ident<$body:ty>) => {
    impl_from_addr!($name, $layout);
    impl_into_addr!($name);
    impl_heap_object!($name);
    impl_repr_convertion!($name, $layout);
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
