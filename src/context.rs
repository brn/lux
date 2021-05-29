use super::heap::*;
use crate::def::*;
use crate::structs::{
  FlatString, HeapLayout, HeapObject, InternalArray, PropertyName, Repr, ShadowClass, Shape, SymbolRegistry,
};
use once_cell::sync::Lazy as SyncLazy;
use std::alloc::{alloc, dealloc, Layout};
use std::mem::size_of;

#[derive(Copy, Clone, Default)]
pub struct GlobalObjectsLayout {
  js_true: Repr,
  js_false: Repr,
  js_undefined: Repr,
  js_null: Repr,
  empty_internal_array: InternalArray<Repr>,
  empty_string: FlatString,
  infinity_str: FlatString,
  true_str: FlatString,
  false_str: FlatString,
  null_str: FlatString,
  undefined_str: FlatString,
  async_iterator_symbol_str: FlatString,
  has_instance_symbol_str: FlatString,
  is_concat_spreadable_symbol_str: FlatString,
  iterator_symbol_str: FlatString,
  match_symbol_str: FlatString,
  match_all_symbol_str: FlatString,
  replace_symbol_str: FlatString,
  search_symbol_str: FlatString,
  species_symbol_str: FlatString,
  split_symbol_str: FlatString,
  to_primitive_symbol_str: FlatString,
  to_string_tag_symbol_str: FlatString,
  unscopables_symbol_str: FlatString,
  empty_shadow_class: ShadowClass,
  symbol_registry: SymbolRegistry,
}

#[derive(Copy, Clone)]
pub struct GlobalObjects(HeapLayout<GlobalObjectsLayout>);
impl_object!(GlobalObjects, HeapLayout<GlobalObjectsLayout>);

impl GlobalObjects {
  const SIZE: usize = size_of::<GlobalObjectsLayout>();

  pub fn new(context: impl AllocationOnlyContext) -> GlobalObjects {
    let mut layout = HeapLayout::<GlobalObjectsLayout>::persist(context, GlobalObjects::SIZE, Shape::global_objects());
    // layout.js_true = Repr::js_true();
    // layout.js_false = Repr::js_false();
    // layout.js_undefined = Repr::js_undefined();
    // layout.js_null = Repr::js_null();
    // layout.empty_internal_array = InternalArray::<Repr>::new(context, 0);
    // let empty_array = InternalArray::<u16>::new(context, 0);
    // let empty_string = FlatString::new(context, empty_array);
    // layout.empty_string = empty_string;
    // layout.infinity_str = FlatString::from_utf8(context, "infinity");
    // layout.true_str = FlatString::from_utf8(context, "true");
    // layout.false_str = FlatString::from_utf8(context, "false");
    // layout.undefined_str = FlatString::from_utf8(context, "undefined");
    // layout.null_str = FlatString::from_utf8(context, "null");
    // layout.empty_shadow_class = ShadowClass::empty(context);
    // layout.symbol_registry = SymbolRegistry::persist(context);
    // layout.async_iterator_symbol_str = FlatString::from_utf8(context, "Symbol.asyncIterator");
    // layout.has_instance_symbol_str = FlatString::from_utf8(context, "Symbol.hasInstance");
    // layout.is_concat_spreadable_symbol_str = FlatString::from_utf8(context, "Symbol.isConcatSpreadable");
    // layout.iterator_symbol_str = FlatString::from_utf8(context, "Symbol.iterator");
    // layout.match_symbol_str = FlatString::from_utf8(context, "Symbol.match");
    // layout.match_all_symbol_str = FlatString::from_utf8(context, "Symbol.matchAll");
    // layout.replace_symbol_str = FlatString::from_utf8(context, "Symbol.replace");
    // layout.search_symbol_str = FlatString::from_utf8(context, "Symbol.search");
    // layout.species_symbol_str = FlatString::from_utf8(context, "Symbol.species");
    // layout.split_symbol_str = FlatString::from_utf8(context, "Symbol.split");
    // layout.to_primitive_symbol_str = FlatString::from_utf8(context, "Symbol.toPrimitive");
    // layout.to_string_tag_symbol_str = FlatString::from_utf8(context, "Symbol.toStringTag");
    // layout.unscopables_symbol_str = FlatString::from_utf8(context, "Symbol.unscopables");
    return GlobalObjects(layout);
  }
}

#[derive(Copy, Clone, Default)]
pub struct StaticNamesLayout {
  description: PropertyName,
}

#[derive(Copy, Clone)]
pub struct StaticNames(HeapLayout<StaticNamesLayout>);
impl_object!(StaticNames, HeapLayout<StaticNamesLayout>);

impl StaticNames {
  const SIZE: usize = size_of::<StaticNamesLayout>();
  pub fn new(context: impl AllocationOnlyContext) -> StaticNames {
    let mut layout = HeapLayout::<StaticNamesLayout>::persist(context, StaticNames::SIZE, Shape::static_names());
    //    layout.description = PropertyName::from_utf8_string(context, "description");
    return StaticNames(layout);
  }

  pub fn description(&self) -> PropertyName {
    return self.description;
  }
}

static mut HEAP: SyncLazy<Heap> = SyncLazy::new(|| {
  return Heap::new();
});

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct LuxContextLayout {
  globals: GlobalObjects,
  static_names: StaticNames,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct LuxContext(HeapLayout<LuxContextLayout>);
impl_object!(LuxContext, HeapLayout<LuxContextLayout>);

impl LuxContext {
  const SIZE: usize = size_of::<LuxContextLayout>();
  pub fn new() -> LuxContext {
    let l = Layout::from_size_align(LuxContext::SIZE, ALIGNMENT).unwrap();
    let heap = unsafe { alloc(l) };
    let mut layout = HeapLayout::<LuxContextLayout>::new_into_heap(heap, LuxContext::SIZE, Shape::context());
    let c = LuxContext(layout);
    layout.globals = GlobalObjects::new(c);
    layout.static_names = StaticNames::new(c);
    return c;
  }

  pub unsafe fn destroy(&mut self) {
    let l = Layout::from_size_align(LuxContext::SIZE, ALIGNMENT).unwrap();
    dealloc(self.raw_heap(), l);
  }
}

pub trait AllocationOnlyContext: Copy {
  fn allocate(&mut self, size: usize) -> Addr;
  fn allocate_persist(&mut self, size: usize) -> Addr;
}

pub trait Context: AllocationOnlyContext {
  fn globals(&self) -> GlobalObjects;
  fn static_names(&self) -> StaticNames;

  fn js_true(&self) -> Repr {
    return self.globals().js_true;
  }

  fn js_false(&self) -> Repr {
    return self.globals().js_false;
  }

  fn js_null(&self) -> Repr {
    return self.globals().js_null;
  }

  fn js_undefined(&self) -> Repr {
    return self.globals().js_undefined;
  }

  fn empty_internal_array<T: Copy>(&self) -> InternalArray<T> {
    let h = self.globals().empty_internal_array;
    let p = &h as *const InternalArray<Repr> as *const u8;
    return unsafe { *(p as *const InternalArray<T>) };
  }

  fn empty_string(&self) -> FlatString {
    return self.globals().empty_string;
  }

  fn intinify_str(&self) -> FlatString {
    return self.globals().infinity_str;
  }

  fn true_str(&self) -> FlatString {
    return self.globals().true_str;
  }

  fn false_str(&self) -> FlatString {
    return self.globals().false_str;
  }

  fn null_str(&self) -> FlatString {
    return self.globals().null_str;
  }

  fn undefined_str(&self) -> FlatString {
    return self.globals().undefined_str;
  }

  fn empty_shadow_class(&self) -> ShadowClass {
    return self.globals().empty_shadow_class;
  }

  fn symbol_registry(&self) -> SymbolRegistry {
    return self.globals().symbol_registry;
  }

  fn async_iterator_symbol_str(&self) -> FlatString {
    return self.globals().async_iterator_symbol_str;
  }
  fn has_instance_symbol_str(&self) -> FlatString {
    return self.globals().has_instance_symbol_str;
  }
  fn is_concat_spreadable_symbol_str(&self) -> FlatString {
    return self.globals().is_concat_spreadable_symbol_str;
  }
  fn iterator_symbol_str(&self) -> FlatString {
    return self.globals().iterator_symbol_str;
  }
  fn match_symbol_str(&self) -> FlatString {
    return self.globals().match_symbol_str;
  }
  fn match_all_symbol_str(&self) -> FlatString {
    return self.globals().match_all_symbol_str;
  }
  fn replace_symbol_str(&self) -> FlatString {
    return self.globals().replace_symbol_str;
  }
  fn search_symbol_str(&self) -> FlatString {
    return self.globals().search_symbol_str;
  }
  fn species_symbol_str(&self) -> FlatString {
    return self.globals().species_symbol_str;
  }
  fn split_symbol_str(&self) -> FlatString {
    return self.globals().split_symbol_str;
  }
  fn to_primitive_symbol_str(&self) -> FlatString {
    return self.globals().to_primitive_symbol_str;
  }
  fn to_string_tag_symbol_str(&self) -> FlatString {
    return self.globals().to_string_tag_symbol_str;
  }
  fn unscopables_symbol_str(&self) -> FlatString {
    return self.globals().unscopables_symbol_str;
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
    return self.globals;
  }

  fn static_names(&self) -> StaticNames {
    return self.static_names;
  }
}
