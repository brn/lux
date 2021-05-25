use super::super::cell::*;
use super::super::hash_map::HashMap;
use super::super::js_object::Name;
use super::super::shape::Shape;
use crate::context::{AllocationOnlyContext, Context};
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use std::mem::size_of;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct SymbolRegistryLayout {
  map: BareHeapLayout<HashMap<Name, JsSymbol>>,
  symbol_async_iterator: BareHeapLayout<JsSymbol>,
  symbol_has_instance: BareHeapLayout<JsSymbol>,
  symbol_is_concat_spreadable: BareHeapLayout<JsSymbol>,
  symbol_iterator: BareHeapLayout<JsSymbol>,
  symbol_match: BareHeapLayout<JsSymbol>,
  symbol_match_all: BareHeapLayout<JsSymbol>,
  symbol_replace: BareHeapLayout<JsSymbol>,
  symbol_search: BareHeapLayout<JsSymbol>,
  symbol_species: BareHeapLayout<JsSymbol>,
  symbol_split: BareHeapLayout<JsSymbol>,
  symbol_to_primitive: BareHeapLayout<JsSymbol>,
  symbol_to_string_tag: BareHeapLayout<JsSymbol>,
  symbol_unscopables: BareHeapLayout<JsSymbol>,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct SymbolRegistry(HeapLayout<SymbolRegistryLayout>);
impl_object!(SymbolRegistry, HeapLayout<SymbolRegistryLayout>);

impl SymbolRegistry {
  const SIZE: usize = Cell::SIZE + size_of::<SymbolRegistryLayout>();
  const TABLE_DEFAULT_CAPACITY: usize = 10;
  pub fn persist(context: &mut impl AllocationOnlyContext) -> SymbolRegistry {
    let mut layout =
      HeapLayout::<SymbolRegistryLayout>::persist(context, SymbolRegistry::SIZE, Shape::symbol_registry());
    layout.map.set(HashMap::<Name, JsSymbol>::new(context));

    SymbolRegistry::init_well_known_symbols(context, layout);

    return SymbolRegistry(layout);
  }

  pub fn register(&self, context: &mut impl Context, symbol: JsSymbol) {
    let desc = symbol.description(context);
    match self.map.handle().find(context, desc) {
      None => {
        self.map.handle().insert(context, desc, symbol);
      }
      _ => {}
    }
  }

  pub fn find(&self, context: &mut impl Context, name: Name) -> Option<JsSymbol> {
    return self.map.handle().find(context, name);
  }

  pub fn get_wellknown_symbol(&self, symbol_type: WellKnownSymbolType) -> JsSymbol {
    match symbol_type {
      WellKnownSymbolType::AsyncIterator => return self.symbol_async_iterator.handle(),
      WellKnownSymbolType::HasInstance => return self.symbol_has_instance.handle(),
      WellKnownSymbolType::IsConcatSpreadable => return self.symbol_is_concat_spreadable.handle(),
      WellKnownSymbolType::Iterator => return self.symbol_iterator.handle(),
      WellKnownSymbolType::Match => return self.symbol_match.handle(),
      WellKnownSymbolType::MatchAll => return self.symbol_match_all.handle(),
      WellKnownSymbolType::Replace => return self.symbol_replace.handle(),
      WellKnownSymbolType::Search => return self.symbol_search.handle(),
      WellKnownSymbolType::Species => return self.symbol_species.handle(),
      WellKnownSymbolType::Split => return self.symbol_split.handle(),
      WellKnownSymbolType::ToPrimitive => return self.symbol_to_primitive.handle(),
      WellKnownSymbolType::ToStringTag => return self.symbol_to_string_tag.handle(),
      WellKnownSymbolType::Unscopables => return self.symbol_unscopables.handle(),
    }
  }

  fn init_well_known_symbols(context: &mut impl AllocationOnlyContext, mut layout: HeapLayout<SymbolRegistryLayout>) {
    layout
      .symbol_async_iterator
      .set(JsSymbol::new_wellknown(context, WellKnownSymbolType::AsyncIterator));
    layout
      .symbol_has_instance
      .set(JsSymbol::new_wellknown(context, WellKnownSymbolType::HasInstance));
    layout.symbol_is_concat_spreadable.set(JsSymbol::new_wellknown(
      context,
      WellKnownSymbolType::IsConcatSpreadable,
    ));
    layout
      .symbol_iterator
      .set(JsSymbol::new_wellknown(context, WellKnownSymbolType::Iterator));
    layout
      .symbol_match
      .set(JsSymbol::new_wellknown(context, WellKnownSymbolType::Match));
    layout
      .symbol_match_all
      .set(JsSymbol::new_wellknown(context, WellKnownSymbolType::MatchAll));
    layout
      .symbol_replace
      .set(JsSymbol::new_wellknown(context, WellKnownSymbolType::Replace));
    layout
      .symbol_search
      .set(JsSymbol::new_wellknown(context, WellKnownSymbolType::Search));
    layout
      .symbol_species
      .set(JsSymbol::new_wellknown(context, WellKnownSymbolType::Species));
    layout
      .symbol_split
      .set(JsSymbol::new_wellknown(context, WellKnownSymbolType::Split));
    layout
      .symbol_to_primitive
      .set(JsSymbol::new_wellknown(context, WellKnownSymbolType::ToPrimitive));
    layout
      .symbol_to_string_tag
      .set(JsSymbol::new_wellknown(context, WellKnownSymbolType::ToStringTag));
    layout
      .symbol_unscopables
      .set(JsSymbol::new_wellknown(context, WellKnownSymbolType::Unscopables));
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

#[repr(C)]
#[derive(Copy, Clone)]
pub struct JsSymbolLayout {
  description: BareHeapLayout<Name>,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct JsSymbol(HeapLayout<JsSymbolLayout>);
impl_object!(JsSymbol, HeapLayout<JsSymbolLayout>);

impl JsSymbol {
  const SIZE: usize = Cell::SIZE + size_of::<JsSymbolLayout>();

  pub fn new(context: &mut impl Context, desc: Name) -> JsSymbol {
    let mut layout = HeapLayout::<JsSymbolLayout>::new(context, JsSymbol::SIZE, Shape::symbol());
    layout.description.set(desc);
    let symbol = JsSymbol(layout);
    return symbol;
  }

  pub fn description(&self, context: &mut impl Context) -> Name {
    if self.is_well_known_symbol() {
      match WellKnownSymbolType::from_u64(self.description.as_value()) {
        Some(WellKnownSymbolType::AsyncIterator) => return context.async_iterator_symbol_str(),
        Some(WellKnownSymbolType::HasInstance) => return context.has_instance_symbol_str(),
        Some(WellKnownSymbolType::IsConcatSpreadable) => return context.is_concat_spreadable_symbol_str(),
        Some(WellKnownSymbolType::Iterator) => return context.iterator_symbol_str(),
        Some(WellKnownSymbolType::Match) => return context.match_symbol_str(),
        Some(WellKnownSymbolType::MatchAll) => return context.match_all_symbol_str(),
        Some(WellKnownSymbolType::Replace) => return context.replace_symbol_str(),
        Some(WellKnownSymbolType::Search) => return context.search_symbol_str(),
        Some(WellKnownSymbolType::Species) => return context.species_symbol_str(),
        Some(WellKnownSymbolType::Split) => return context.split_symbol_str(),
        Some(WellKnownSymbolType::ToPrimitive) => return context.to_primitive_symbol_str(),
        Some(WellKnownSymbolType::ToStringTag) => return context.to_string_tag_symbol_str(),
        Some(WellKnownSymbolType::Unscopables) => return context.unscopables_symbol_str(),
        _ => unreachable!(),
      }
    }
    return self.description.handle();
  }

  pub fn wellknown_symbol_type(&self) -> Option<WellKnownSymbolType> {
    if self.is_well_known_symbol() {
      return WellKnownSymbolType::from_u64(self.description.as_value());
    }
    return None;
  }

  pub fn is_well_known_symbol(&self) -> bool {
    return self.description.get_flag();
  }

  fn new_wellknown(context: &mut impl AllocationOnlyContext, symbol_type: WellKnownSymbolType) -> JsSymbol {
    let mut layout = HeapLayout::<JsSymbolLayout>::persist(context, JsSymbol::SIZE, Shape::symbol());
    layout.description.set_value(symbol_type as u64);
    layout.description.set_flag();
    let symbol = JsSymbol(layout);
    return symbol;
  }
}

#[cfg(test)]
mod symbol_test {
  use super::*;
  use crate::context::isolate;

  #[test]
  fn wellknown_symbols_test() {
    let compare = |symbol_type: WellKnownSymbolType| {
      let mut c = isolate::context();
      let symbol = c.symbol_registry().get_wellknown_symbol(symbol_type);
      let b = c.symbol_registry().get_wellknown_symbol(symbol_type);
      let desc = b.description(&mut c);
      assert!(symbol.description(&mut c).equals(&mut c, desc));
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
      let mut c = isolate::context();
      let name = Name::from_utf8(&mut c, name);
      let symbol = JsSymbol::new(&mut c, name);
      c.symbol_registry().register(&mut c, symbol);
      let desc = symbol.description(&mut c);
      let b = c.symbol_registry().find(&mut c, desc);
      assert!(b.is_some());
      let s = b.unwrap().description(&mut c);
      assert!(symbol.description(&mut c).equals(&mut c, s));
      assert!(symbol.is_same_heap_object(b.unwrap()));
    };
    compare("@@test");
    compare("@@test-value");
    compare("Symbol.asyncIterator");
  }
}
