use super::super::cell::*;
use super::super::hash_map::{HashMap, PredefinedHash};
use super::super::object_record::{ObjectRecord, ObjectSkin, OwnPropertySearchHint, PropertyMode};
use super::super::repr::Repr;
use super::super::string::FlatString;
use super::property::PropertyName;
use crate::context::{AllocationOnlyContext, Context, LuxContext, ObjectRecordsInitializedContext};
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use std::mem::size_of;

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct SymbolRegistryLayout {
  map: HashMap<PropertyName, JsSymbol>,
  symbol_async_iterator: JsSymbol,
  symbol_has_instance: JsSymbol,
  symbol_is_concat_spreadable: JsSymbol,
  symbol_iterator: JsSymbol,
  symbol_match: JsSymbol,
  symbol_match_all: JsSymbol,
  symbol_replace: JsSymbol,
  symbol_search: JsSymbol,
  symbol_species: JsSymbol,
  symbol_split: JsSymbol,
  symbol_to_primitive: JsSymbol,
  symbol_to_string_tag: JsSymbol,
  symbol_unscopables: JsSymbol,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct SymbolRegistry(HeapLayout<SymbolRegistryLayout>);
impl_object!(SymbolRegistry, HeapLayout<SymbolRegistryLayout>);

impl SymbolRegistry {
  pub const SIZE: usize = size_of::<SymbolRegistryLayout>();

  pub fn persist(context: impl ObjectRecordsInitializedContext) -> SymbolRegistry {
    let mut layout =
      HeapLayout::<SymbolRegistryLayout>::persist(context, context.object_records().symbol_registry_record());
    layout.map = HashMap::<PropertyName, JsSymbol>::new(context);
    return SymbolRegistry(layout);
  }

  pub fn register(&mut self, context: impl Context, symbol: JsSymbol) {
    let desc = PropertyName::from(symbol.description(context));
    match self.map.find(desc) {
      None => {
        self.map.insert(context, desc, symbol);
      }
      _ => {}
    }
  }

  pub fn find(&self, desc: FlatString) -> Option<JsSymbol> {
    return self.map.find(PropertyName::from(desc));
  }

  pub fn get_wellknown_symbol(&self, symbol_type: WellKnownSymbolType) -> JsSymbol {
    match symbol_type {
      WellKnownSymbolType::AsyncIterator => return self.symbol_async_iterator,
      WellKnownSymbolType::HasInstance => return self.symbol_has_instance,
      WellKnownSymbolType::IsConcatSpreadable => return self.symbol_is_concat_spreadable,
      WellKnownSymbolType::Iterator => return self.symbol_iterator,
      WellKnownSymbolType::Match => return self.symbol_match,
      WellKnownSymbolType::MatchAll => return self.symbol_match_all,
      WellKnownSymbolType::Replace => return self.symbol_replace,
      WellKnownSymbolType::Search => return self.symbol_search,
      WellKnownSymbolType::Species => return self.symbol_species,
      WellKnownSymbolType::Split => return self.symbol_split,
      WellKnownSymbolType::ToPrimitive => return self.symbol_to_primitive,
      WellKnownSymbolType::ToStringTag => return self.symbol_to_string_tag,
      WellKnownSymbolType::Unscopables => return self.symbol_unscopables,
    }
  }

  pub fn init_well_known_symbols(&mut self, context: impl Context) {
    self.symbol_async_iterator = JsSymbol::new_wellknown(context, WellKnownSymbolType::AsyncIterator);
    self.symbol_has_instance = JsSymbol::new_wellknown(context, WellKnownSymbolType::HasInstance);
    self.symbol_is_concat_spreadable = JsSymbol::new_wellknown(context, WellKnownSymbolType::IsConcatSpreadable);
    self.symbol_iterator = JsSymbol::new_wellknown(context, WellKnownSymbolType::Iterator);
    self.symbol_match = JsSymbol::new_wellknown(context, WellKnownSymbolType::Match);
    self.symbol_match_all = JsSymbol::new_wellknown(context, WellKnownSymbolType::MatchAll);
    self.symbol_replace = JsSymbol::new_wellknown(context, WellKnownSymbolType::Replace);
    self.symbol_search = JsSymbol::new_wellknown(context, WellKnownSymbolType::Search);
    self.symbol_species = JsSymbol::new_wellknown(context, WellKnownSymbolType::Species);
    self.symbol_split = JsSymbol::new_wellknown(context, WellKnownSymbolType::Split);
    self.symbol_to_primitive = JsSymbol::new_wellknown(context, WellKnownSymbolType::ToPrimitive);
    self.symbol_to_string_tag = JsSymbol::new_wellknown(context, WellKnownSymbolType::ToStringTag);
    self.symbol_unscopables = JsSymbol::new_wellknown(context, WellKnownSymbolType::Unscopables);
  }
}

#[derive(Copy, Clone, FromPrimitive)]
pub enum WellKnownSymbolType {
  AsyncIterator = 2,
  HasInstance = 6,
  IsConcatSpreadable = 14,
  Iterator = 30,
  Match = 64,
  MatchAll = 126,
  Replace = 254,
  Search = 510,
  Species = 1022,
  Split = 2046,
  ToPrimitive = 4094,
  ToStringTag = 8190,
  Unscopables = 16382,
}

#[derive(Copy, Clone, Default)]
pub struct JsSymbolLayout {
  description_index: isize,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct JsSymbol(HeapLayout<JsSymbolLayout>);
impl_object!(JsSymbol, HeapLayout<JsSymbolLayout>);

impl JsSymbol {
  pub const SIZE: usize = std::mem::size_of::<JsSymbolLayout>();
  const IS_WELLKNOWN_BIT: usize = 1;
  const IS_OBJECT_BIT: usize = 2;

  pub fn new(context: impl Context, mut desc: FlatString) -> JsSymbol {
    desc.prepare_hash(context);
    let mut layout = HeapLayout::<JsSymbolLayout>::new_object(context, context.object_records().symbol_record());
    let result = layout.full_record_unchecked().define_own_property(
      context,
      new_property!(context, value: context.static_names().description(), desc.into()),
    );
    if result.usable_as_cache() {
      layout.description_index = result.index();
    } else {
      layout.description_index = -1;
    }
    return JsSymbol(layout);
  }

  pub fn new_primitive(context: impl Context, desc: FlatString) -> JsSymbol {
    let mut layout = HeapLayout::<JsSymbolLayout>::new_object(context, context.object_records().symbol_record());
    let result = layout.full_record_unchecked().define_own_property(
      context,
      new_property!(context, value: context.static_names().description(), desc.into()),
    );
    if result.usable_as_cache() {
      layout.description_index = result.index();
    } else {
      layout.description_index = -1;
    }
    return JsSymbol(layout);
  }

  pub fn description(&self, context: impl Context) -> FlatString {
    match self
      .full_record_unchecked()
      .get_own_property(OwnPropertySearchHint::new_with(
        context.static_names().description(),
        self.description_index,
        PropertyMode::FAST,
      )) {
      Some(result) => {
        if !result.descriptor().value().is_boxed() {
          match WellKnownSymbolType::from_u64(u64::from(result.descriptor().value())) {
            Some(WellKnownSymbolType::AsyncIterator) => return context.globals().async_iterator_symbol_str(),
            Some(WellKnownSymbolType::HasInstance) => return context.globals().has_instance_symbol_str(),
            Some(WellKnownSymbolType::IsConcatSpreadable) => {
              return context.globals().is_concat_spreadable_symbol_str()
            }
            Some(WellKnownSymbolType::Iterator) => return context.globals().iterator_symbol_str(),
            Some(WellKnownSymbolType::Match) => return context.globals().match_symbol_str(),
            Some(WellKnownSymbolType::MatchAll) => return context.globals().match_all_symbol_str(),
            Some(WellKnownSymbolType::Replace) => return context.globals().replace_symbol_str(),
            Some(WellKnownSymbolType::Search) => return context.globals().search_symbol_str(),
            Some(WellKnownSymbolType::Species) => return context.globals().species_symbol_str(),
            Some(WellKnownSymbolType::Split) => return context.globals().split_symbol_str(),
            Some(WellKnownSymbolType::ToPrimitive) => return context.globals().to_primitive_symbol_str(),
            Some(WellKnownSymbolType::ToStringTag) => return context.globals().to_string_tag_symbol_str(),
            Some(WellKnownSymbolType::Unscopables) => return context.globals().unscopables_symbol_str(),
            _ => unreachable!(),
          }
        }
        return FlatString::from(result.descriptor().value());
      }
      None => unreachable!(),
    };
    unreachable!();
  }

  pub fn wellknown_symbol_type(&self, context: impl Context) -> Option<WellKnownSymbolType> {
    if self.is_well_known_symbol() {
      match self
        .full_record_unchecked()
        .get_own_property(OwnPropertySearchHint::new_with(
          context.static_names().description(),
          self.description_index,
          PropertyMode::FAST,
        )) {
        Some(result) => {
          if !result.descriptor().value().is_boxed() {
            return WellKnownSymbolType::from_u64(u64::from(result.descriptor().value()));
          }
        }
        None => unreachable!(),
      };
    }
    return None;
  }

  pub fn is_well_known_symbol(&self) -> bool {
    return HeapObject::get_data_field(self, JsSymbol::IS_WELLKNOWN_BIT);
  }

  pub fn is_object(this: Repr) -> bool {
    debug_assert!(Cell::from(this).shape().is_symbol());
    let s = JsSymbol::from(this);
    return HeapObject::get_data_field(&s, JsSymbol::IS_OBJECT_BIT);
  }

  pub fn to_object(this: Repr) -> JsSymbol {
    debug_assert!(Cell::from(this).shape().is_symbol());
    let mut s = JsSymbol::from(this);
    HeapObject::set_data_field(&mut s, JsSymbol::IS_OBJECT_BIT);
    return JsSymbol::from(this);
  }

  pub fn to_primitive(this: Repr) -> PrimitiveSymbol {
    debug_assert!(Cell::from(this).shape().is_symbol());
    let mut s = JsSymbol::from(this);
    HeapObject::unset_data_field(&mut s, JsSymbol::IS_OBJECT_BIT);
    return PrimitiveSymbol::from(this);
  }

  fn new_wellknown(context: impl Context, symbol_type: WellKnownSymbolType) -> JsSymbol {
    let layout = HeapLayout::<JsSymbolLayout>::persist_object(context, context.object_records().symbol_record());
    layout.full_record_unchecked().define_own_property(
      context,
      new_property!(
        context,
        value: context.static_names().description(),
        Repr::from(symbol_type as u64)
      ),
    );
    let mut symbol = JsSymbol(layout);
    HeapObject::set_data_field(&mut symbol, JsSymbol::IS_OBJECT_BIT);
    return symbol;
  }
}

impl_object_record!(JsSymbol);

impl PredefinedHash for JsSymbol {
  fn prepare_hash(&mut self, context: impl AllocationOnlyContext) {
    let mut desc = self.description(LuxContext::from_allocation_only_context(context));
    desc.prepare_hash(context);
    self.full_record_unchecked().set_hash(desc.predefined_hash());
  }

  fn predefined_hash(&self) -> u64 {
    return self.full_record_unchecked().hash();
  }
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct PrimitiveSymbol(HeapLayout<JsSymbolLayout>);
impl_object!(PrimitiveSymbol, HeapLayout<JsSymbolLayout>);

impl PrimitiveSymbol {
  pub fn new(context: impl Context, desc: FlatString) -> PrimitiveSymbol {
    let symbol = JsSymbol::new_primitive(context, desc);
    return PrimitiveSymbol(symbol.0);
  }
}

#[cfg(test)]
mod symbol_test {
  use super::*;
  use crate::context::LuxContext;

  #[test]
  fn wellknown_symbols_test() {
    let compare = |symbol_type: WellKnownSymbolType| {
      let c = LuxContext::new();
      let symbol = c.symbol_registry().get_wellknown_symbol(symbol_type);
      let b = c.symbol_registry().get_wellknown_symbol(symbol_type);
      let desc = b.description(c);
      assert!(symbol.description(c) == desc);
      assert!(symbol.is_same_heap_object(b));
    };
    compare(WellKnownSymbolType::AsyncIterator);
    compare(WellKnownSymbolType::HasInstance);
    compare(WellKnownSymbolType::IsConcatSpreadable);
    compare(WellKnownSymbolType::Iterator);
    compare(WellKnownSymbolType::Match);
    compare(WellKnownSymbolType::MatchAll);
    compare(WellKnownSymbolType::Replace);
    compare(WellKnownSymbolType::Search);
    compare(WellKnownSymbolType::Species);
    compare(WellKnownSymbolType::Split);
    compare(WellKnownSymbolType::ToPrimitive);
  }

  #[test]
  fn custom_symbols_test() {
    let compare = |name| {
      let c = LuxContext::new();
      let name = FlatString::from_utf8(c, name);
      let symbol = JsSymbol::new(c, name);
      c.symbol_registry().register(c, symbol);
      let desc = symbol.description(c);
      let b = c.symbol_registry().find(desc);
      assert!(b.is_some());
      let s = b.unwrap().description(c);
      assert!(symbol.description(c) == s);
      assert!(symbol.is_same_heap_object(b.unwrap()));
    };
    compare("@@test");
    compare("@@test-value");
    compare("Symbol.asyncIterator");
  }
}
