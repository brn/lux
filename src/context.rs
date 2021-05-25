use super::heap::*;
use crate::def::*;
use crate::structs::{
  BareHeapLayout, Cell, HeapLayout, HeapObject, InternalArray, JsString, Name, Repr, ShadowClass, Shape, SymbolRegistry,
};
use once_cell::sync::Lazy as SyncLazy;
use once_cell::unsync::Lazy;
use std::alloc::{alloc, dealloc, Layout};
use std::mem::size_of;

#[derive(Copy, Clone)]
pub struct GlobalObjectsLayout {
  js_true: BareHeapLayout<Repr>,
  js_false: BareHeapLayout<Repr>,
  js_undefined: BareHeapLayout<Repr>,
  js_null: BareHeapLayout<Repr>,
  empty_internal_array: BareHeapLayout<InternalArray<Repr>>,
  empty_string: BareHeapLayout<JsString>,
  infinity_str: BareHeapLayout<JsString>,
  true_str: BareHeapLayout<JsString>,
  false_str: BareHeapLayout<JsString>,
  null_str: BareHeapLayout<JsString>,
  undefined_str: BareHeapLayout<JsString>,
  async_iterator_symbol_str: BareHeapLayout<Name>,
  has_instance_symbol_str: BareHeapLayout<Name>,
  is_concat_spreadable_symbol_str: BareHeapLayout<Name>,
  iterator_symbol_str: BareHeapLayout<Name>,
  match_symbol_str: BareHeapLayout<Name>,
  match_all_symbol_str: BareHeapLayout<Name>,
  replace_symbol_str: BareHeapLayout<Name>,
  search_symbol_str: BareHeapLayout<Name>,
  species_symbol_str: BareHeapLayout<Name>,
  split_symbol_str: BareHeapLayout<Name>,
  to_primitive_symbol_str: BareHeapLayout<Name>,
  to_string_tag_symbol_str: BareHeapLayout<Name>,
  unscopables_symbol_str: BareHeapLayout<Name>,
  empty_shadow_class: BareHeapLayout<ShadowClass>,
  symbol_registry: BareHeapLayout<SymbolRegistry>,
}

#[derive(Copy, Clone)]
pub struct GlobalObjects(HeapLayout<GlobalObjectsLayout>);
impl_object!(GlobalObjects, HeapLayout<GlobalObjectsLayout>);

impl GlobalObjects {
  const SIZE: usize = Cell::SIZE + size_of::<GlobalObjectsLayout>();

  pub fn new(context: &mut impl AllocationOnlyContext) -> GlobalObjects {
    let mut layout = HeapLayout::<GlobalObjectsLayout>::persist(context, GlobalObjects::SIZE, Shape::global_objects());
    layout.js_true.set(Repr::js_true());
    layout.js_false.set(Repr::js_false());
    layout.js_undefined.set(Repr::js_undefined());
    layout.js_null.set(Repr::js_null());
    layout.empty_internal_array.set(InternalArray::<Repr>::new(context, 0));
    let empty_array = InternalArray::<u16>::new(context, 0);
    let empty_string = JsString::new(context, empty_array);
    layout.empty_string.set(empty_string);
    layout.infinity_str.set(JsString::from_utf8(context, "infinity"));
    layout.true_str.set(JsString::from_utf8(context, "true"));
    layout.false_str.set(JsString::from_utf8(context, "false"));
    layout.undefined_str.set(JsString::from_utf8(context, "undefined"));
    layout.null_str.set(JsString::from_utf8(context, "null"));
    layout.empty_shadow_class.set(ShadowClass::empty(context));
    layout.symbol_registry.set(SymbolRegistry::persist(context));
    layout
      .async_iterator_symbol_str
      .set(Name::from_utf8(context, "Symbol.asyncIterator"));
    layout
      .has_instance_symbol_str
      .set(Name::from_utf8(context, "Symbol.hasInstance"));
    layout
      .is_concat_spreadable_symbol_str
      .set(Name::from_utf8(context, "Symbol.isConcatSpreadable"));
    layout
      .iterator_symbol_str
      .set(Name::from_utf8(context, "Symbol.iterator"));
    layout.match_symbol_str.set(Name::from_utf8(context, "Symbol.match"));
    layout
      .match_all_symbol_str
      .set(Name::from_utf8(context, "Symbol.matchAll"));
    layout
      .replace_symbol_str
      .set(Name::from_utf8(context, "Symbol.replace"));
    layout.search_symbol_str.set(Name::from_utf8(context, "Symbol.search"));
    layout
      .species_symbol_str
      .set(Name::from_utf8(context, "Symbol.species"));
    layout.split_symbol_str.set(Name::from_utf8(context, "Symbol.split"));
    layout
      .to_primitive_symbol_str
      .set(Name::from_utf8(context, "Symbol.toPrimitive"));
    layout
      .to_string_tag_symbol_str
      .set(Name::from_utf8(context, "Symbol.toStringTag"));
    layout
      .unscopables_symbol_str
      .set(Name::from_utf8(context, "Symbol.unscopables"));
    return GlobalObjects(layout);
  }
}

static mut HEAP: SyncLazy<Heap> = SyncLazy::new(|| {
  return Heap::new();
});

#[repr(C)]
#[derive(Copy, Clone)]
pub struct LuxContextLayout {
  globals: BareHeapLayout<GlobalObjects>,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct LuxContext(HeapLayout<LuxContextLayout>);
impl_object!(LuxContext, HeapLayout<LuxContextLayout>);

impl LuxContext {
  const SIZE: usize = Cell::SIZE + size_of::<LuxContextLayout>();
  pub fn new() -> LuxContext {
    let l = Layout::from_size_align(LuxContext::SIZE, ALIGNMENT).unwrap();
    let heap = unsafe { alloc(l) };
    let mut layout = HeapLayout::<LuxContextLayout>::new_into_heap(heap, LuxContext::SIZE, Shape::context());
    let mut c = LuxContext(layout);
    layout.globals.set(GlobalObjects::new(&mut c));
    return c;
  }

  pub unsafe fn destroy(&mut self) {
    let l = Layout::from_size_align(LuxContext::SIZE, ALIGNMENT).unwrap();
    dealloc(self.raw_heap(), l);
  }
}

pub trait AllocationOnlyContext {
  fn allocate(&mut self, size: usize) -> Addr;
  fn allocate_persist(&mut self, size: usize) -> Addr;
}

pub trait Context: AllocationOnlyContext {
  fn globals(&self) -> GlobalObjects;

  fn js_true(&self) -> Repr {
    return self.globals().js_true.handle();
  }

  fn js_false(&self) -> Repr {
    return self.globals().js_false.handle();
  }

  fn js_null(&self) -> Repr {
    return self.globals().js_null.handle();
  }

  fn js_undefined(&self) -> Repr {
    return self.globals().js_undefined.handle();
  }

  fn empty_internal_array<T: Copy>(&self) -> InternalArray<T> {
    let h = self.globals().empty_internal_array.handle();
    let p = &h as *const InternalArray<Repr> as *const u8;
    return unsafe { *(p as *const InternalArray<T>) };
  }

  fn empty_string(&self) -> JsString {
    return self.globals().empty_string.handle();
  }

  fn intinify_str(&self) -> JsString {
    return self.globals().infinity_str.handle();
  }

  fn true_str(&self) -> JsString {
    return self.globals().true_str.handle();
  }

  fn false_str(&self) -> JsString {
    return self.globals().false_str.handle();
  }

  fn null_str(&self) -> JsString {
    return self.globals().null_str.handle();
  }

  fn undefined_str(&self) -> JsString {
    return self.globals().undefined_str.handle();
  }

  fn empty_shadow_class(&self) -> ShadowClass {
    return self.globals().empty_shadow_class.handle();
  }

  fn symbol_registry(&self) -> SymbolRegistry {
    return self.globals().symbol_registry.handle();
  }

  fn async_iterator_symbol_str(&self) -> Name {
    return self.globals().async_iterator_symbol_str.handle();
  }
  fn has_instance_symbol_str(&self) -> Name {
    return self.globals().has_instance_symbol_str.handle();
  }
  fn is_concat_spreadable_symbol_str(&self) -> Name {
    return self.globals().is_concat_spreadable_symbol_str.handle();
  }
  fn iterator_symbol_str(&self) -> Name {
    return self.globals().iterator_symbol_str.handle();
  }
  fn match_symbol_str(&self) -> Name {
    return self.globals().match_symbol_str.handle();
  }
  fn match_all_symbol_str(&self) -> Name {
    return self.globals().match_all_symbol_str.handle();
  }
  fn replace_symbol_str(&self) -> Name {
    return self.globals().replace_symbol_str.handle();
  }
  fn search_symbol_str(&self) -> Name {
    return self.globals().search_symbol_str.handle();
  }
  fn species_symbol_str(&self) -> Name {
    return self.globals().species_symbol_str.handle();
  }
  fn split_symbol_str(&self) -> Name {
    return self.globals().split_symbol_str.handle();
  }
  fn to_primitive_symbol_str(&self) -> Name {
    return self.globals().to_primitive_symbol_str.handle();
  }
  fn to_string_tag_symbol_str(&self) -> Name {
    return self.globals().to_string_tag_symbol_str.handle();
  }
  fn unscopables_symbol_str(&self) -> Name {
    return self.globals().unscopables_symbol_str.handle();
  }
}

#[cfg(not(feature = "nogc"))]
impl AllocationOnlyContext for LuxContext {
  fn allocate(&mut self, size: usize) -> Addr {
    return unsafe { HEAP.allocate(size) };
  }

  fn allocate_persist(&mut self, size: usize) -> Addr {
    return unsafe { HEAP.allocate_persist(size) };
  }
}

#[cfg(feature = "nogc")]
impl AllocationOnlyContext for LuxContext {
  fn allocate(&mut self, size: usize) -> Addr {
    return unsafe { alloc(Layout::from_size_align(size, ALIGNMENT).unwrap()) };
  }

  fn allocate_persist(&mut self, size: usize) -> Addr {
    return unsafe { alloc(Layout::from_size_align(size, ALIGNMENT).unwrap()) };
  }
}

impl Context for LuxContext {
  fn globals(&self) -> GlobalObjects {
    return self.globals.handle();
  }
}

pub mod isolate {
  use super::*;

  thread_local!(
    pub static CONTEXT: Lazy<LuxContext> = Lazy::new(|| {
      return LuxContext::new();
    })
  );

  pub fn context() -> LuxContext {
    return CONTEXT.with(|m| **m);
  }
}
