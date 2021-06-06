use super::heap::*;
use crate::def::*;
use crate::structs::{
  BareHeapLayout, Builtins, FlatString, HeapLayout, HeapObject, InternalArray, ObjectRecord, ObjectRecords,
  PropertyName, Repr, Shape, SymbolRegistry,
};
use once_cell::sync::Lazy as SyncLazy;
use property::Property;
use std::alloc::{alloc, dealloc, Layout};
use std::mem::size_of;

#[derive(Copy, Clone, Default, Property)]
pub struct GlobalObjectsLayout {
  #[property(get(type = "copy"))]
  js_true: Repr,

  #[property(get(type = "copy"))]
  js_false: Repr,

  #[property(get(type = "copy"))]
  js_undefined: Repr,

  #[property(get(type = "copy"))]
  js_null: Repr,

  empty_internal_array: InternalArray<Repr>,

  #[property(get(type = "copy"))]
  empty_string: FlatString,

  #[property(get(type = "copy"))]
  infinity_str: FlatString,

  #[property(get(type = "copy"))]
  true_str: FlatString,

  #[property(get(type = "copy"))]
  false_str: FlatString,

  #[property(get(type = "copy"))]
  null_str: FlatString,

  #[property(get(type = "copy"))]
  undefined_str: FlatString,

  #[property(get(type = "copy"))]
  async_iterator_symbol_str: FlatString,

  #[property(get(type = "copy"))]
  has_instance_symbol_str: FlatString,

  #[property(get(type = "copy"))]
  is_concat_spreadable_symbol_str: FlatString,

  #[property(get(type = "copy"))]
  iterator_symbol_str: FlatString,

  #[property(get(type = "copy"))]
  match_symbol_str: FlatString,

  #[property(get(type = "copy"))]
  match_all_symbol_str: FlatString,

  #[property(get(type = "copy"))]
  replace_symbol_str: FlatString,

  #[property(get(type = "copy"))]
  search_symbol_str: FlatString,

  #[property(get(type = "copy"))]
  species_symbol_str: FlatString,

  #[property(get(type = "copy"))]
  split_symbol_str: FlatString,

  #[property(get(type = "copy"))]
  to_primitive_symbol_str: FlatString,

  #[property(get(type = "copy"))]
  to_string_tag_symbol_str: FlatString,

  #[property(get(type = "copy"))]
  unscopables_symbol_str: FlatString,

  #[property(get(type = "copy"))]
  to_string_to_string_str: FlatString,

  #[property(get(type = "copy"))]
  symbol_to_string_str: FlatString,
}

#[derive(Copy, Clone)]
pub struct GlobalObjects(HeapLayout<GlobalObjectsLayout>);
impl_object!(GlobalObjects, HeapLayout<GlobalObjectsLayout>);

impl GlobalObjects {
  const SIZE: usize = size_of::<GlobalObjectsLayout>();

  pub fn new(context: impl ObjectRecordsInitializedContext) -> GlobalObjects {
    let global_objects_record = ObjectRecord::new(context, GlobalObjects::SIZE as u32, Shape::global_objects());
    let mut layout = HeapLayout::<GlobalObjectsLayout>::persist(context, global_objects_record);
    layout.js_true = Repr::js_true();
    layout.js_false = Repr::js_false();
    layout.js_undefined = Repr::js_undefined();
    layout.js_null = Repr::js_null();
    layout.empty_internal_array = InternalArray::<Repr>::new(context, 0);
    let empty_array = InternalArray::<u16>::new(context, 0);
    let empty_string = FlatString::new(context, empty_array);
    layout.empty_string = empty_string;
    layout.infinity_str = FlatString::from_utf8(context, "infinity");
    layout.true_str = FlatString::from_utf8(context, "true");
    layout.false_str = FlatString::from_utf8(context, "false");
    layout.undefined_str = FlatString::from_utf8(context, "undefined");
    layout.null_str = FlatString::from_utf8(context, "null");
    layout.async_iterator_symbol_str = FlatString::from_utf8(context, "Symbol.asyncIterator");
    layout.has_instance_symbol_str = FlatString::from_utf8(context, "Symbol.hasInstance");
    layout.is_concat_spreadable_symbol_str = FlatString::from_utf8(context, "Symbol.isConcatSpreadable");
    layout.iterator_symbol_str = FlatString::from_utf8(context, "Symbol.iterator");
    layout.match_symbol_str = FlatString::from_utf8(context, "Symbol.match");
    layout.match_all_symbol_str = FlatString::from_utf8(context, "Symbol.matchAll");
    layout.replace_symbol_str = FlatString::from_utf8(context, "Symbol.replace");
    layout.search_symbol_str = FlatString::from_utf8(context, "Symbol.search");
    layout.species_symbol_str = FlatString::from_utf8(context, "Symbol.species");
    layout.split_symbol_str = FlatString::from_utf8(context, "Symbol.split");
    layout.to_primitive_symbol_str = FlatString::from_utf8(context, "Symbol.toPrimitive");
    layout.to_string_tag_symbol_str = FlatString::from_utf8(context, "Symbol.toStringTag");
    layout.unscopables_symbol_str = FlatString::from_utf8(context, "Symbol.unscopables");
    layout.to_string_to_string_str = FlatString::from_utf8(context, "function toString () { [native code] }");
    layout.symbol_to_string_str = FlatString::from_utf8(context, "function Symbol () { [native code] }");
    return GlobalObjects(layout);
  }

  pub fn empty_internal_array<T: Copy>(&self) -> InternalArray<T> {
    return InternalArray::<T>::from(self.empty_internal_array.raw_heap());
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
  pub fn new(context: impl ObjectRecordsInitializedContext) -> StaticNames {
    let static_names_record = ObjectRecord::new(context, StaticNames::SIZE as u32, Shape::static_names());
    let mut layout = HeapLayout::<StaticNamesLayout>::persist(context, static_names_record);
    layout.description = PropertyName::from_utf8_string(context, "description");
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
  symbol_registry: SymbolRegistry,
  static_names: StaticNames,
  builtins: Builtins,
  object_records: ObjectRecords,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct LuxContext(BareHeapLayout<LuxContextLayout>);
impl_object!(LuxContext, BareHeapLayout<LuxContextLayout>);

impl LuxContext {
  const SIZE: usize = size_of::<LuxContextLayout>();
  #[cfg(feature = "nogc")]
  pub fn new() -> LuxContext {
    let c = LuxContext::new_until_js_object_records();
    let mut layout = c.0;
    layout.static_names = StaticNames::new(c);
    layout.globals = GlobalObjects::new(c);
    layout.builtins = Builtins::new(c);
    layout.symbol_registry = SymbolRegistry::persist(c);
    layout.symbol_registry.init_well_known_symbols(c);
    return c;
  }

  pub fn new_only_allocator() -> LuxContext {
    let l = Layout::from_size_align(LuxContext::SIZE, ALIGNMENT).unwrap();
    let heap = unsafe { alloc(l) };
    let layout = BareHeapLayout::<LuxContextLayout>::wrap(heap);
    let c = LuxContext(layout);
    return c;
  }

  pub fn new_until_internal_object_records() -> LuxContext {
    let mut c = LuxContext::new_only_allocator();
    let mut records = ObjectRecords::new(c);
    c.object_records = records;
    records.initialize_internal_structs(c);
    return c;
  }

  pub fn new_until_js_object_records() -> LuxContext {
    let c = LuxContext::new_until_internal_object_records();
    let mut records = c.object_records;
    records.initialize_js_objects(c);
    return c;
  }

  pub unsafe fn destroy(&mut self) {
    let l = Layout::from_size_align(LuxContext::SIZE, ALIGNMENT).unwrap();
    dealloc(self.raw_heap(), l);
  }
}

pub trait AllocationOnlyContext: HeapObject + Copy {
  fn allocate(&mut self, size: usize) -> Addr;
  fn allocate_persist(&mut self, size: usize) -> Addr;
}

pub trait ObjectRecordsInitializedContext: AllocationOnlyContext {
  fn object_records(&self) -> ObjectRecords;
}

pub trait Context: ObjectRecordsInitializedContext {
  fn globals(&self) -> GlobalObjects;

  fn static_names(&self) -> StaticNames;

  fn from_allocation_only_context(context: impl AllocationOnlyContext) -> LuxContext {
    return LuxContext::from(context.raw_heap());
  }

  fn symbol_registry(&self) -> SymbolRegistry;
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

impl ObjectRecordsInitializedContext for LuxContext {
  fn object_records(&self) -> ObjectRecords {
    return self.object_records;
  }
}

impl Context for LuxContext {
  fn globals(&self) -> GlobalObjects {
    return self.globals;
  }

  fn static_names(&self) -> StaticNames {
    return self.static_names;
  }

  fn symbol_registry(&self) -> SymbolRegistry {
    return self.symbol_registry;
  }
}
