use super::cell::*;
use super::hash_map::HashMap;
use super::hash_map::PredefinedHash;
use super::internal_array::InternalArray;
use super::object::{FastOwnProperties, Header, JsObject, JsReceiver, Property, PropertyDescriptor, PropertyName};
use super::repr::Repr;
use super::shape::{Shape, ShapeTag};
use crate::context::{AllocationOnlyContext, Context, LuxContext, ObjectRecordsInitializedContext};
use crate::def::*;
use crate::utility::{BitOperator, MaskedBitsetMut};
use num_derive::FromPrimitive;
use property::Property as Prop;
use std::convert::TryFrom;
use std::mem::size_of;

pub type PropertyDescriptorTable = HashMap<PropertyName, PropertyDescriptor>;
pub type PropertyDescriptorArray = InternalArray<Property>;

#[repr(C)]
#[derive(Copy, Clone, Default, Prop)]
pub struct TransitionRecord {
  #[property(get(type = "copy"))]
  property: Property,

  #[property(get(type = "copy"))]
  next_record: FullObjectRecord,
}

impl TransitionRecord {
  pub fn new(property: Property, next_record: FullObjectRecord) -> TransitionRecord {
    return TransitionRecord { property, next_record };
  }
}

pub type TransitionArray = InternalArray<TransitionRecord>;

#[derive(Copy, Clone, Property)]
pub struct OwnPropertySearchHint {
  #[property(get(type = "copy"))]
  key: PropertyName,

  #[property(get(type = "copy"))]
  index: isize,

  #[property(get(type = "copy"))]
  mode: PropertyMode,
}

impl OwnPropertySearchHint {
  pub fn new(key: PropertyName) -> OwnPropertySearchHint {
    return OwnPropertySearchHint {
      key,
      index: -1,
      mode: PropertyMode::Element,
    };
  }
  pub fn new_with(key: PropertyName, index: isize, mode: PropertyMode) -> OwnPropertySearchHint {
    return OwnPropertySearchHint { key, index, mode };
  }
}

impl From<OwnPropertyDescriptorSearchResult> for OwnPropertySearchHint {
  fn from(a: OwnPropertyDescriptorSearchResult) -> OwnPropertySearchHint {
    return OwnPropertySearchHint::new_with(a.name(), a.index(), a.property_mode());
  }
}

#[derive(Copy, Clone, Property)]
pub struct PropertySearchHint {
  #[property(get(type = "copy"))]
  own_property_search_hint: OwnPropertySearchHint,

  #[property(get(type = "copy"))]
  full_object_record: FullObjectRecord,
}

impl PropertySearchHint {
  pub fn new(key: PropertyName) -> PropertySearchHint {
    return PropertySearchHint {
      own_property_search_hint: OwnPropertySearchHint::new(key),
      full_object_record: FullObjectRecord::from(Repr::invalid()),
    };
  }

  pub fn new_with(key: PropertyName, index: isize, mode: PropertyMode, full_object_record: FullObjectRecord) -> PropertySearchHint {
    return PropertySearchHint {
      own_property_search_hint: OwnPropertySearchHint::new_with(key, index, mode),
      full_object_record,
    };
  }
}

#[derive(Copy, Clone, FromPrimitive, PartialEq)]
pub enum PropertyMode {
  Fast = 0,
  External,
  Element,
  SlowElement,
  Slow,
  Invalid,
}
impl From<PropertyMode> for usize {
  fn from(a: PropertyMode) -> usize {
    return a as usize;
  }
}

#[derive(Copy, Clone, Property)]
pub struct OwnPropertyDescriptorSearchResult {
  #[property(get(type = "copy"))]
  name: PropertyName,

  #[property(get(type = "copy"))]
  descriptor: PropertyDescriptor,

  #[property(get(type = "copy"))]
  property_mode: PropertyMode,

  #[property(get(type = "copy"))]
  index: isize,
}

impl OwnPropertyDescriptorSearchResult {
  pub fn new(
    name: PropertyName,
    descriptor: PropertyDescriptor,
    property_mode: PropertyMode,
    index: isize,
  ) -> OwnPropertyDescriptorSearchResult {
    return OwnPropertyDescriptorSearchResult {
      name,
      descriptor,
      property_mode,
      index,
    };
  }

  pub fn usable_as_cache(&self) -> bool {
    return self.property_mode == PropertyMode::Fast && self.index >= 0;
  }
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct PropertyStorageCoercible(Repr);
impl Default for PropertyStorageCoercible {
  fn default() -> PropertyStorageCoercible {
    return PropertyStorageCoercible(Repr::invalid());
  }
}

impl PropertyStorageCoercible {
  pub fn new(repr: Repr) -> PropertyStorageCoercible {
    let cell = Cell::from(repr);
    assert!(cell.shape().tag() == ShapeTag::InternalArray || cell.shape().tag() == ShapeTag::HashMap);
    return PropertyStorageCoercible(repr);
  }

  pub fn into_storage(&self) -> Result<PropertyDescriptorArray, PropertyDescriptorTable> {
    match Cell::from(self.0).shape().tag() {
      ShapeTag::InternalArray => return Ok(InternalArray::<Property>::from(self.0)),
      ShapeTag::HashMap => return Err(HashMap::<PropertyName, PropertyDescriptor>::from(self.0)),
      _ => unreachable!(),
    }
  }

  pub fn is_null(&self) -> bool {
    return self.0.is_invalid();
  }

  fn len(&self) -> usize {
    if self.is_null() {
      return 0;
    }
    return match self.into_storage() {
      Ok(a) => a.len(),
      Err(h) => h.len(),
    };
  }
}

impl std::fmt::Debug for PropertyStorageCoercible {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return write!(
      f,
      "{}",
      if self.is_null() {
        String::from("null")
      } else {
        match self.into_storage() {
          Ok(array) => format!("{:?}", array),
          Err(hash_map) => format!("{:?}", hash_map),
        }
      }
    );
  }
}

#[derive(Copy, Clone, Prop)]
pub struct PropertyDescriptorSearchResult {
  #[property(get(type = "copy"))]
  own_property_descriptor_search_result: OwnPropertyDescriptorSearchResult,

  #[property(get(type = "copy"))]
  full_object_record: FullObjectRecord,
}

impl PropertyDescriptorSearchResult {
  pub fn new(
    own_property_descriptor_search_result: &OwnPropertyDescriptorSearchResult,
    full_object_record: FullObjectRecord,
  ) -> PropertyDescriptorSearchResult {
    return PropertyDescriptorSearchResult {
      own_property_descriptor_search_result: *own_property_descriptor_search_result,
      full_object_record,
    };
  }
}

pub struct PropertyLocation {
  target_property: Option<PropertyName>,
  current: PropertyMode,
  index: usize,
  record: FullObjectRecord,
  receiver: JsReceiver,
}

pub struct OwnPropertyIterationResult {
  own_property_descriptor_search_result: OwnPropertyDescriptorSearchResult,
  receiver: JsReceiver,
  record: FullObjectRecord,
  descriptor_array: PropertyDescriptorArray,
  descriptor_table: PropertyDescriptorTable,
  fast_properties: Option<FastOwnProperties>,
}

impl OwnPropertyIterationResult {
  pub fn new_fast(
    name: PropertyName,
    desc: PropertyDescriptor,
    index: usize,
    location: &PropertyLocation,
    fast_properties: FastOwnProperties,
  ) -> OwnPropertyIterationResult {
    return OwnPropertyIterationResult {
      own_property_descriptor_search_result: OwnPropertyDescriptorSearchResult::new(name, desc, PropertyMode::Fast, index as isize),
      record: location.record,
      receiver: location.receiver,
      descriptor_array: PropertyDescriptorArray::from(Repr::invalid()),
      descriptor_table: PropertyDescriptorTable::from(Repr::invalid()),
      fast_properties: Some(fast_properties),
    };
  }

  pub fn new_external_array(
    name: PropertyName,
    desc: PropertyDescriptor,
    index: usize,
    location: &PropertyLocation,
    descriptor_array: PropertyDescriptorArray,
  ) -> OwnPropertyIterationResult {
    return OwnPropertyIterationResult {
      own_property_descriptor_search_result: OwnPropertyDescriptorSearchResult::new(name, desc, PropertyMode::External, index as isize),
      record: location.record,
      receiver: location.receiver,
      descriptor_array,
      descriptor_table: PropertyDescriptorTable::from(Repr::invalid()),
      fast_properties: None,
    };
  }

  pub fn new_external_map(
    name: PropertyName,
    desc: PropertyDescriptor,
    index: usize,
    location: &PropertyLocation,
    descriptor_table: PropertyDescriptorTable,
  ) -> OwnPropertyIterationResult {
    return OwnPropertyIterationResult {
      own_property_descriptor_search_result: OwnPropertyDescriptorSearchResult::new(name, desc, PropertyMode::Slow, index as isize),
      record: location.record,
      receiver: location.receiver,
      descriptor_array: PropertyDescriptorArray::from(Repr::invalid()),
      descriptor_table,
      fast_properties: None,
    };
  }

  pub fn new_elements_array(
    name: PropertyName,
    desc: PropertyDescriptor,
    index: usize,
    location: &PropertyLocation,
    descriptor_array: PropertyDescriptorArray,
  ) -> OwnPropertyIterationResult {
    return OwnPropertyIterationResult {
      own_property_descriptor_search_result: OwnPropertyDescriptorSearchResult::new(name, desc, PropertyMode::Element, index as isize),
      record: location.record,
      receiver: location.receiver,
      descriptor_array,
      descriptor_table: PropertyDescriptorTable::from(Repr::invalid()),
      fast_properties: None,
    };
  }

  pub fn new_elements_map(
    name: PropertyName,
    desc: PropertyDescriptor,
    index: usize,
    location: &PropertyLocation,
    descriptor_table: PropertyDescriptorTable,
  ) -> OwnPropertyIterationResult {
    return OwnPropertyIterationResult {
      own_property_descriptor_search_result: OwnPropertyDescriptorSearchResult::new(name, desc, PropertyMode::SlowElement, index as isize),
      record: location.record,
      receiver: location.receiver,
      descriptor_array: PropertyDescriptorArray::from(Repr::invalid()),
      descriptor_table,
      fast_properties: None,
    };
  }

  pub fn update(&mut self, property: Property) {
    match self.own_property_descriptor_search_result.property_mode {
      PropertyMode::Fast => {
        *self
          .fast_properties
          .unwrap()
          .offset_mut(self.own_property_descriptor_search_result.index) = property;
      }
      PropertyMode::External | PropertyMode::Element => {
        self
          .descriptor_array
          .write(self.own_property_descriptor_search_result.index as usize, property);
      }
      PropertyMode::Slow | PropertyMode::SlowElement => {
        self.descriptor_table.update_entry_from_index(
          self.own_property_descriptor_search_result.index as usize,
          property.name(),
          property.desc(),
        );
      }
      _ => unreachable!(),
    }
  }
}

pub struct OwnPropertyIterator(Option<PropertyLocation>);
impl Iterator for OwnPropertyIterator {
  type Item = OwnPropertyIterationResult;

  fn next(&mut self) -> Option<Self::Item> {
    loop {
      match self.property_location_mut().current {
        PropertyMode::Element | PropertyMode::SlowElement => match self.search_element_properties() {
          Some(result) => return Some(result),
          _ => {
            let loc = self.property_location_mut();
            loc.index = 0;
            loc.current = PropertyMode::Fast;
          }
        },
        PropertyMode::Fast => match self.search_fast_own_properties() {
          Some(result) => return Some(result),
          _ => {
            let loc = self.property_location_mut();
            loc.index = 0;
            loc.current = PropertyMode::External;
          }
        },
        PropertyMode::External | PropertyMode::Slow => match self.search_external_properties() {
          Some(result) => return Some(result),
          None => return None,
        },
        _ => return None,
      }
    }
  }
}

impl OwnPropertyIterator {
  fn property_location_mut(&mut self) -> &mut PropertyLocation {
    assert!(self.0.is_some());
    let loc = self.0.as_mut().unwrap();
    assert!(loc.current != PropertyMode::Invalid);
    return loc;
  }

  fn search_element_properties(&mut self) -> Option<OwnPropertyIterationResult> {
    let loc = self.property_location_mut();
    if loc.record.elements.is_null() {
      return None;
    }
    match loc.record.elements.into_storage() {
      Ok(array) => {
        if loc.index == array.len() {
          return None;
        }
        if loc.target_property.is_some() && loc.target_property.unwrap() != array[loc.index].name() {
          return None;
        }
        let p = array[loc.index];
        loc.index += 1;
        return Some(OwnPropertyIterationResult::new_elements_array(
          p.name(),
          p.desc(),
          loc.index - 1,
          loc,
          array,
        ));
      }
      Err(hash_map) => match hash_map.entry_from_index(loc.index) {
        Some((key, value)) => {
          if loc.target_property.is_some() && loc.target_property.unwrap() != key {
            return None;
          }
          loc.index += 1;
          return Some(OwnPropertyIterationResult::new_elements_map(
            key,
            value,
            loc.index - 1,
            loc,
            hash_map,
          ));
        }
        _ => {
          return None;
        }
      },
    }
  }

  fn search_fast_own_properties(&mut self) -> Option<OwnPropertyIterationResult> {
    let loc = self.property_location_mut();
    if loc.index == loc.record.fast_properties_len() {
      return None;
    }
    match FullObjectRecord::fast_own_properties(loc.record, loc.receiver) {
      Ok(fast) => {
        let property = fast.offset_ref(loc.index as isize);
        if loc.target_property.is_some() && loc.target_property.unwrap() != property.name() {
          return None;
        }
        let ret = OwnPropertyIterationResult::new_fast(property.name(), property.desc(), loc.index, loc, fast);
        loc.index += 1;
        return Some(ret);
      }
      _ => return None,
    }
  }

  fn search_external_properties(&mut self) -> Option<OwnPropertyIterationResult> {
    let loc = self.property_location_mut();
    if loc.record.properties.is_null() {
      return None;
    }
    match loc.record.properties.into_storage() {
      Ok(array) => {
        if loc.index == array.len() {
          loc.index = 0;
          loc.current = PropertyMode::Invalid;
          return None;
        }
        let property = array[loc.index];
        if loc.target_property.is_some() && loc.target_property.unwrap() != property.name() {
          return None;
        }
        let ret = Some(OwnPropertyIterationResult::new_external_array(
          property.name(),
          property.desc(),
          loc.index,
          loc,
          array,
        ));
        loc.index += 1;
        return ret;
      }
      Err(hash_map) => match hash_map.entry_from_index(loc.index) {
        Some((key, value)) => {
          if loc.target_property.is_some() && loc.target_property.unwrap() != key {
            return None;
          }
          let ret = Some(OwnPropertyIterationResult::new_external_map(key, value, loc.index, loc, hash_map));
          loc.index += 1;
          return ret;
        }
        _ => {
          loc.index = 0;
          loc.current = PropertyMode::Invalid;
          return None;
        }
      },
    }
  }
}

const LIGHT_LAYOUT: usize = 1;
const EMBED_PROPERTY: usize = 2;
const NOT_EXTENSIBLE_INDEX: usize = 3;

const LIGHT_SIZE: u32 = size_of::<LightObjectRecordLayout>() as u32;
const FULL_SIZE: u32 = size_of::<ObjectRecordLayout>() as u32;

pub trait ObjectMetaData {
  fn header(&self) -> &Header;
  fn header_mut(&mut self) -> &mut Header;
}
macro_rules! _impl_obj_metadata {
  ($name:tt) => {
    impl ObjectMetaData for $name {
      fn header(&self) -> &Header {
        return &self.header;
      }

      fn header_mut(&mut self) -> &mut Header {
        return &mut self.header;
      }
    }
  };
}

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct LightObjectRecordLayout {
  header: Header,
}
_impl_obj_metadata!(LightObjectRecordLayout);

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct ObjectRecordLayout {
  header: Header,
  parent: FullObjectRecord,
  prototype: JsObject,
  transitions: TransitionArray,
  properties: PropertyStorageCoercible,
  elements: PropertyStorageCoercible,
  hash: u64,
}
_impl_obj_metadata!(ObjectRecordLayout);

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct FastPropertiesLayout {
  fast_property_offset: u32,
  fast_property_capacity: u32,
  fast_property_len: u32,
  _pad: u32,
}

#[repr(C)]
pub struct ObjectRecord(BareHeapLayout<LightObjectRecordLayout>);
impl_bare_object!(ObjectRecord, BareHeapLayout<LightObjectRecordLayout>);

impl<T: Copy + ObjectMetaData> BareHeapLayout<T> {
  pub fn size(&self) -> u32 {
    return self.header().size();
  }

  pub fn size_contains_object_record(&self) -> u32 {
    return self.size() + HEAP_LAYOUT_SIZE;
  }

  pub fn shape_tag(&self) -> ShapeTag {
    return self.header().shape().tag();
  }

  pub fn data_field(&mut self) -> MaskedBitsetMut<u64> {
    return self.header_mut().data_offset(NOT_EXTENSIBLE_INDEX);
  }

  pub fn is_light_layout(&self) -> bool {
    return self.header().data().get(LIGHT_LAYOUT);
  }

  pub fn shape(&self) -> Shape {
    return self.header().shape();
  }
}

impl ObjectRecord {
  pub const SIZE: usize = LIGHT_SIZE as usize;
  pub fn new(mut context: impl AllocationOnlyContext, size: u32, shape: Shape) -> ObjectRecord {
    return ObjectRecord::new_into_heap(context.allocate(LIGHT_SIZE as usize), size, shape, true);
  }

  pub fn new_into_heap(heap: Addr, size: u32, shape: Shape, is_light_layout: bool) -> ObjectRecord {
    let base_layout = BareHeapLayout::<LightObjectRecordLayout>::wrap(heap);
    let mut object_record = ObjectRecord(base_layout);
    object_record.header.init();
    object_record.header.set_size(size);
    object_record.header.set_shape(shape);
    if is_light_layout {
      object_record.header.data_mut().set(LIGHT_LAYOUT);
    }
    return object_record;
  }

  pub fn copy_with_size(&self, mut context: impl AllocationOnlyContext, size: usize) -> ObjectRecord {
    let record_size = if self.is_light_layout() { LIGHT_SIZE } else { FULL_SIZE };
    let mut layout = BareHeapLayout::<LightObjectRecordLayout>::wrap(context.allocate(record_size as usize));
    unsafe { std::ptr::copy_nonoverlapping(self.raw_heap(), layout.as_addr(), record_size as usize) };
    layout.header.set_size(size as u32);
    return ObjectRecord(layout);
  }
}

impl std::fmt::Debug for ObjectRecord {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return write!(
      f,
      "ObjectRecord {{
{:?}
}}",
      self.header
    );
  }
}

const FULL_OBJECT_RECORD_LAYOUT_SIZE: usize = size_of::<ObjectRecordLayout>();
const EMBEDDABLE_LAYOUT_SIZE: usize = size_of::<FastPropertiesLayout>();

#[repr(C)]
pub struct FullObjectRecord(BareHeapLayout<ObjectRecordLayout>);
impl_bare_object!(FullObjectRecord, BareHeapLayout<ObjectRecordLayout>);

impl FullObjectRecord {
  const EMBEDDED_PROPERTY_SIZE: usize = 7;
  pub fn new(mut context: impl Context, size: u32, shape: Shape, fast_property_len: u32) -> FullObjectRecord {
    let mut object_record = ObjectRecord::new_into_heap(
      context.allocate(FULL_OBJECT_RECORD_LAYOUT_SIZE + if fast_property_len == 0 { 0 } else { EMBEDDABLE_LAYOUT_SIZE }),
      size
        + if fast_property_len > 0 {
          fast_property_len * HEAP_LAYOUT_SIZE
        } else {
          0
        },
      shape,
      false,
    );
    let mut full_object_record = FullObjectRecord::try_from(object_record).unwrap();
    if fast_property_len > 0 {
      object_record.header.data_mut().set(EMBED_PROPERTY);
      let mut layout = FullObjectRecord::get_fast_properties_layout(full_object_record).unwrap();
      layout.fast_property_offset = size;
      layout.fast_property_capacity = fast_property_len;
      layout.fast_property_len = 0;
    }
    full_object_record.transitions = TransitionArray::default();
    full_object_record.properties = PropertyStorageCoercible::default();
    full_object_record.elements = PropertyStorageCoercible::default();
    full_object_record.prototype = JsObject::default();
    full_object_record.parent = FullObjectRecord::default();
    full_object_record.hash = 0;
    return full_object_record;
  }

  #[inline]
  pub fn copy_fast(mut context: impl ObjectRecordsInitializedContext, old_record: FullObjectRecord) -> FullObjectRecord {
    let record_size = FULL_OBJECT_RECORD_LAYOUT_SIZE
      + if old_record.fast_properties_capacity() == 0 {
        0
      } else {
        EMBEDDABLE_LAYOUT_SIZE
      };
    let new_record = ObjectRecord::new_into_heap(context.allocate(record_size), old_record.size(), old_record.shape(), false);
    unsafe { std::ptr::copy_nonoverlapping(old_record.raw_heap(), new_record.raw_heap(), record_size) };
    return FullObjectRecord::try_from(new_record).unwrap();
  }

  #[inline]
  pub fn copy_external_array(
    context: impl ObjectRecordsInitializedContext,
    old_record: FullObjectRecord,
    array: PropertyDescriptorArray,
  ) -> FullObjectRecord {
    let mut full_record = Self::copy_fast(context, old_record);
    let new_cap = if array.capacity() > array.len() {
      array.capacity()
    } else {
      array.capacity() * 2
    };
    let new_array = InternalArray::<Property>::copy_construct(context, new_cap, array.len(), array.data());
    full_record.properties = PropertyStorageCoercible::new(new_array.into());
    return full_record;
  }

  #[inline]
  pub fn copy_external_hashmap(
    context: impl ObjectRecordsInitializedContext,
    old_record: FullObjectRecord,
    hash_map: PropertyDescriptorTable,
  ) -> FullObjectRecord {
    let mut full_record = Self::copy_fast(context, old_record);
    let new_hash_map = HashMap::<PropertyName, PropertyDescriptor>::copy_construct(context, hash_map);
    full_record.properties = PropertyStorageCoercible::new(new_hash_map.into());
    return full_record;
  }

  #[inline]
  pub fn copy_elements_array(
    context: impl ObjectRecordsInitializedContext,
    old_record: FullObjectRecord,
    array: PropertyDescriptorArray,
  ) -> FullObjectRecord {
    let mut full_record = Self::copy_fast(context, old_record);
    let new_cap = if array.capacity() > array.len() {
      array.capacity()
    } else {
      array.capacity() * 2
    };
    let new_array = InternalArray::<Property>::copy_construct(context, new_cap, array.len(), array.data());
    full_record.elements = PropertyStorageCoercible::new(new_array.into());
    return full_record;
  }

  #[inline]
  pub fn copy_elements_hashmap(
    context: impl ObjectRecordsInitializedContext,
    old_record: FullObjectRecord,
    hash_map: PropertyDescriptorTable,
  ) -> FullObjectRecord {
    let mut full_record = Self::copy_fast(context, old_record);
    let new_hash_map = HashMap::<PropertyName, PropertyDescriptor>::copy_construct(context, hash_map);
    full_record.elements = PropertyStorageCoercible::new(new_hash_map.into());
    return full_record;
  }

  #[inline]
  pub fn hash(&self) -> u64 {
    return self.hash;
  }

  #[inline]
  pub fn set_hash(&mut self, hash: u64) {
    self.hash = hash;
  }

  #[inline(always)]
  pub fn fast_properties_capacity(&self) -> usize {
    return match FullObjectRecord::get_fast_properties_layout(*self) {
      Ok(layout) => layout.fast_property_capacity as usize,
      _ => 0,
    };
  }

  #[inline(always)]
  pub fn fast_properties_len(&self) -> usize {
    return match FullObjectRecord::get_fast_properties_layout(*self) {
      Ok(layout) => layout.fast_property_len as usize,
      _ => 0,
    };
  }

  #[inline(always)]
  pub fn has_fast_property_space(&self) -> bool {
    return self.fast_properties_capacity() > self.fast_properties_len();
  }

  #[inline(always)]
  pub fn is_enable_fast_mode(&self) -> bool {
    return self.header.data().get(EMBED_PROPERTY);
  }

  #[inline(always)]
  pub fn is_fast_mode(&self) -> bool {
    let mut or = ObjectRecord::from(*self);
    let field = or.data_field();
    return !field.get(PropertyMode::External.into())
      && !field.get(PropertyMode::Slow.into())
      && !field.get(PropertyMode::Element.into())
      && !field.get(PropertyMode::SlowElement.into());
  }

  #[inline(always)]
  pub fn is_external_field_mode(&self) -> bool {
    let mut or = ObjectRecord::from(*self);
    let field = or.data_field();
    return field.get(PropertyMode::External.into());
  }

  #[inline(always)]
  pub fn set_external_field_mode(&self) {
    ObjectRecord::from(*self).data_field().set(PropertyMode::External.into());
  }

  #[inline(always)]
  pub fn set_slow_external_field_mode(&self) {
    ObjectRecord::from(*self).data_field().set(PropertyMode::Slow.into());
  }

  #[inline(always)]
  pub fn is_element_mode(&self) -> bool {
    let mut or = ObjectRecord::from(*self);
    let field = or.data_field();
    return field.get(PropertyMode::Element.into()) || field.get(PropertyMode::SlowElement.into());
  }

  #[inline(always)]
  pub fn is_slow_element_mode(&self) -> bool {
    let mut or = ObjectRecord::from(*self);
    let field = or.data_field();
    return field.get(PropertyMode::SlowElement.into());
  }

  #[inline(always)]
  pub fn set_element_mode(&self) {
    ObjectRecord::from(*self).data_field().set(PropertyMode::Element.into());
  }

  #[inline(always)]
  pub fn set_slow_element_mode(&self) {
    ObjectRecord::from(*self).data_field().set(PropertyMode::SlowElement.into());
  }

  #[inline(always)]
  pub fn is_slow_mode(&self) -> bool {
    let mut or = ObjectRecord::from(*self);
    let field = or.data_field();
    return field.get(PropertyMode::Slow.into());
  }

  #[inline(always)]
  pub fn own_properties_len(&self) -> usize {
    return self.fast_properties_len() + self.properties.len() + self.elements.len();
  }

  #[inline(always)]
  pub fn is_transited(&self) -> bool {
    return !self.transitions.is_null();
  }

  #[inline(always)]
  pub fn traistion(&mut self, context: impl ObjectRecordsInitializedContext, property: Property) -> FullObjectRecord {
    return FullObjectRecordTraistion(*self).transition(context, property);
  }

  #[inline(always)]
  pub fn find_transition(&mut self, property: Property) -> Option<FullObjectRecord> {
    if self.transitions.is_null() {
      return None;
    }

    for record in self.transitions.into_iter() {
      if record.property() == property {
        return Some(record.next_record());
      }
    }
    return None;
  }

  #[inline(always)]
  pub fn transition_with_record(
    &mut self,
    context: impl ObjectRecordsInitializedContext,
    property: Property,
    mut object_record: FullObjectRecord,
  ) -> FullObjectRecord {
    if self.transitions.is_null() {
      self.transitions = TransitionArray::new(context, 10);
    }
    self.transitions = self.transitions.push_safe(context, TransitionRecord::new(property, object_record));
    object_record.parent = *self;
    return object_record;
  }

  #[inline(always)]
  pub fn prototype(&self) -> Option<JsObject> {
    return if !self.prototype.is_null() { Some(self.prototype) } else { None };
  }

  #[inline(always)]
  pub fn set_prototype(&mut self, proto: JsObject) -> bool {
    self.prototype = proto;
    return true;
  }

  #[inline(always)]
  pub fn is_extensible(&self) -> bool {
    return !self.header.data().get(NOT_EXTENSIBLE_INDEX);
  }

  #[inline(always)]
  pub fn prevent_extensions(&mut self) -> bool {
    self.header.data().set(NOT_EXTENSIBLE_INDEX);
    return true;
  }

  #[inline]
  pub fn define_own_property(
    this: Self,
    context: impl ObjectRecordsInitializedContext,
    receiver: JsReceiver,
    property: Property,
    should_transition: bool,
  ) -> OwnPropertyDescriptorSearchResult {
    if property.name().is_number() {
      return match FullObjectRecord::update_property_if_exists(this, receiver, property, PropertyMode::Element) {
        Some(r) => r,
        _ => Self::define_element_property(this, context, receiver, property, should_transition),
      };
    }
    if this.is_fast_mode() && this.is_enable_fast_mode() {
      return match FullObjectRecord::update_property_if_exists(this, receiver, property, PropertyMode::Fast) {
        Some(r) => r,
        _ => Self::define_fast_property(this, context, receiver, property, should_transition),
      };
    }

    return match FullObjectRecord::update_property_if_exists(this, receiver, property, PropertyMode::External) {
      Some(r) => r,
      _ => Self::define_external_property(this, context, receiver, property, should_transition),
    };
  }

  #[inline(always)]
  pub fn define_own_properties(
    this: Self,
    context: impl ObjectRecordsInitializedContext,
    receiver: JsReceiver,
    properties: PropertyDescriptorArray,
  ) {
    for p in properties.into_iter() {
      Self::define_own_property(this, context, receiver, p, false);
    }
  }

  pub fn has_property(this: Self, receiver: JsReceiver, hint: PropertySearchHint) -> Option<PropertyDescriptorSearchResult> {
    if hint.full_object_record().is_null() {
      let mut maybe_full_object_record = this.prototype();
      while maybe_full_object_record.is_some() {
        let full_object_record = maybe_full_object_record.unwrap().full_record_unchecked();
        let result = Self::get_own_property(full_object_record, receiver, hint.own_property_search_hint());
        if result.is_some() {
          return Some(PropertyDescriptorSearchResult::new(&result.unwrap(), full_object_record));
        }
        maybe_full_object_record = full_object_record.prototype();
      }
      return None;
    }
    match Self::get_own_property(hint.full_object_record, receiver, hint.own_property_search_hint()) {
      Some(result) => {
        return Some(PropertyDescriptorSearchResult::new(&result, hint.full_object_record()));
      }
      _ => return None,
    }
  }

  pub fn get_own_property(this: Self, receiver: JsReceiver, hint: OwnPropertySearchHint) -> Option<OwnPropertyDescriptorSearchResult> {
    let mut iterator = FullObjectRecord::init_own_property_iterator_with_hint(this, receiver, hint);
    if hint.index() >= 0 {
      return match iterator.next() {
        Some(r) => Some(r.own_property_descriptor_search_result),
        None => None,
      };
    }
    for p in iterator {
      if p.own_property_descriptor_search_result.name() == hint.key() {
        return Some(p.own_property_descriptor_search_result);
      }
    }
    return None;
  }

  pub fn own_property_keys(this: Self, context: impl ObjectRecordsInitializedContext, receiver: JsReceiver) -> InternalArray<PropertyName> {
    let iterator = FullObjectRecord::init_own_property_iterator(this, receiver);
    let estimated_len = this.own_properties_len() as usize;
    let mut array = InternalArray::<PropertyName>::new(context, if estimated_len == 0 { 10 } else { estimated_len });
    for r in iterator {
      array = array.push_safe(context, r.own_property_descriptor_search_result.name());
    }
    return array;
  }

  #[inline(always)]
  fn init_own_property_iterator(this: Self, receiver: JsReceiver) -> OwnPropertyIterator {
    return OwnPropertyIterator(Some(PropertyLocation {
      target_property: None,
      index: 0,
      current: PropertyMode::Fast,
      record: this,
      receiver,
    }));
  }

  #[inline(always)]
  fn init_own_property_iterator_with_hint(this: Self, receiver: JsReceiver, hint: OwnPropertySearchHint) -> OwnPropertyIterator {
    return OwnPropertyIterator(Some(PropertyLocation {
      target_property: None,
      index: if hint.index() < 0 { 0 } else { hint.index() as usize },
      current: hint.mode(),
      record: this,
      receiver,
    }));
  }

  fn define_fast_property(
    mut this: Self,
    context: impl ObjectRecordsInitializedContext,
    receiver: JsReceiver,
    property: Property,
    should_transition: bool,
  ) -> OwnPropertyDescriptorSearchResult {
    if should_transition {
      this = FullObjectRecordTraistion(this).transition(context, property);
      receiver.object().set_record(this.into());
    }
    let mut layout = Self::get_fast_properties_layout_unchecked(this);
    if layout.fast_property_capacity <= layout.fast_property_len {
      this.set_external_field_mode();
      return Self::define_external_property(this, context, receiver, property, false);
    }
    let mut fast_own_properties = FullObjectRecord::fast_own_properties_unchecked(this, receiver);
    fast_own_properties.add_property(layout.fast_property_capacity, layout.fast_property_len, property);
    layout.fast_property_len += 1;
    return OwnPropertyDescriptorSearchResult::new(
      property.name(),
      property.desc(),
      PropertyMode::Fast,
      (layout.fast_property_len - 1) as isize,
    );
  }

  fn define_element_property(
    mut this: Self,
    context: impl ObjectRecordsInitializedContext,
    receiver: JsReceiver,
    property: Property,
    should_transition: bool,
  ) -> OwnPropertyDescriptorSearchResult {
    if should_transition {
      this = FullObjectRecordTraistion(this).transition(context, property);
      receiver.object().set_record(this.into());
    }
    if !this.elements.is_null() {
      this.elements = PropertyStorageCoercible::new(PropertyDescriptorArray::new(context, 7).into());
    }
    let index = property.name().to_number_unchecked() as u32;
    if this.elements.is_null() {
      if index > 7 {
        this.set_slow_element_mode();
        this.elements = PropertyStorageCoercible::new(HashMap::<PropertyName, PropertyDescriptor>::new(context).into());
      } else {
        this.set_element_mode();
        this.elements = PropertyStorageCoercible::new(InternalArray::<Property>::new(context, 7).into());
      }
    }

    let mut property_mode = PropertyMode::Element;
    let storage_index = match this.elements.into_storage() {
      Ok(mut array) => {
        if (index as usize) < array.len() {
          this.elements = PropertyStorageCoercible::new(array.push_safe(context, property).into());
          index
        } else {
          this.set_slow_element_mode();
          let mut hash = HashMap::<PropertyName, PropertyDescriptor>::new(context);
          for p in array.into_iter() {
            hash.insert(context, p.name(), p.desc());
          }
          let storage_index = hash.insert(context, property.name(), property.desc()) as u32;
          this.elements = PropertyStorageCoercible(hash.into());
          storage_index
        }
      }
      Err(mut hash_map) => {
        property_mode = PropertyMode::SlowElement;
        hash_map.insert(context, property.name(), property.desc()) as u32
      }
    };

    if should_transition {
      receiver
        .object()
        .set_record(FullObjectRecordTraistion(this).transition(context, property).into());
    }

    return OwnPropertyDescriptorSearchResult::new(property.name(), property.desc(), property_mode, storage_index as isize);
  }

  #[inline(always)]
  fn define_external_property(
    mut this: Self,
    context: impl ObjectRecordsInitializedContext,
    receiver: JsReceiver,
    property: Property,
    should_transition: bool,
  ) -> OwnPropertyDescriptorSearchResult {
    if should_transition {
      this = FullObjectRecordTraistion(this).transition(context, property);
      receiver.object().set_record(this.into());
    }
    if this.properties.is_null() {
      this.properties = PropertyStorageCoercible::new(PropertyDescriptorArray::new(context, 10).into());
    }
    return match this.properties.into_storage() {
      Ok(array) => {
        if array.is_full() {
          FullObjectRecord::define_external_hash_map_proeprty(this, context, property, true)
        } else {
          FullObjectRecord::define_external_array_proeprty(this, context, property)
        }
      }
      Err(_) => return FullObjectRecord::define_external_hash_map_proeprty(this, context, property, false),
    };
  }

  fn define_external_array_proeprty(
    mut this: Self,
    context: impl ObjectRecordsInitializedContext,
    property: Property,
  ) -> OwnPropertyDescriptorSearchResult {
    let mut array = this.properties.into_storage().unwrap();
    if array.is_full() {
      array = InternalArray::expand_and_copy(context, array);
      this.properties = PropertyStorageCoercible::new(array.into());
    }
    let offset = array.len();
    array.push(property);

    return OwnPropertyDescriptorSearchResult::new(property.name(), property.desc(), PropertyMode::External, offset as isize);
  }

  #[inline(always)]
  fn define_external_hash_map_proeprty(
    mut this: Self,
    context: impl ObjectRecordsInitializedContext,
    property: Property,
    transition_from_array: bool,
  ) -> OwnPropertyDescriptorSearchResult {
    if transition_from_array {
      this.set_slow_external_field_mode();
      let table = PropertyDescriptorTable::new(context);
      this.properties = PropertyStorageCoercible::new(table.into());
    }
    let mut hash_map = this.properties.into_storage().unwrap_err();
    property.name().prepare_hash(context);
    let index = hash_map.insert(context, property.name(), property.desc());
    return OwnPropertyDescriptorSearchResult::new(property.name(), property.desc(), PropertyMode::Slow, index as isize);
  }

  #[inline(always)]
  fn update_property_if_exists(
    this: Self,
    receiver: JsReceiver,
    property: Property,
    mode: PropertyMode,
  ) -> Option<OwnPropertyDescriptorSearchResult> {
    let iter =
      FullObjectRecord::init_own_property_iterator_with_hint(this, receiver, OwnPropertySearchHint::new_with(property.name(), -1, mode));
    for mut p in iter {
      if p.own_property_descriptor_search_result.name() == property.name()
        && p.own_property_descriptor_search_result.descriptor() == property.desc()
      {
        p.update(property);
        return Some(p.own_property_descriptor_search_result);
      }
    }
    return None;
  }

  #[inline(always)]
  fn get_embedded_properties_head_addr(this: Self, receiver: JsReceiver) -> Addr {
    return unsafe {
      Cell::from(receiver.object())
        .get_body()
        .offset(Self::get_fast_properties_layout_unchecked(this).fast_property_offset as isize)
    };
  }

  #[inline(always)]
  fn fast_own_properties(this: Self, receiver: JsReceiver) -> Result<FastOwnProperties, ()> {
    if this.is_enable_fast_mode() {
      return Ok(FastOwnProperties::from(FullObjectRecord::get_embedded_properties_head_addr(
        this, receiver,
      )));
    }
    return Err(());
  }

  #[inline(always)]
  fn fast_own_properties_unchecked(this: Self, receiver: JsReceiver) -> FastOwnProperties {
    return FastOwnProperties::from(FullObjectRecord::get_embedded_properties_head_addr(this, receiver));
  }

  #[inline(always)]
  fn get_fast_properties_layout(this: FullObjectRecord) -> Result<BareHeapLayout<FastPropertiesLayout>, ()> {
    if this.is_enable_fast_mode() {
      return Ok(FullObjectRecord::get_fast_properties_layout_unchecked(this));
    }
    Err(())
  }

  #[inline(always)]
  fn get_fast_properties_layout_unchecked(this: FullObjectRecord) -> BareHeapLayout<FastPropertiesLayout> {
    return BareHeapLayout::<FastPropertiesLayout>::wrap(unsafe { this.raw_heap().offset(FULL_OBJECT_RECORD_LAYOUT_SIZE as isize) });
  }

  fn short_description(&self) -> String {
    return format!("FullObjectRecord {{ {:?} }}", self.header.shape());
  }
}

impl std::fmt::Debug for FullObjectRecord {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    if ObjectRecord::from(*self).is_light_layout() {
      return write!(f, "{:?}", ObjectRecord::from(*self));
    }
    return write!(
      f,
      "FullObjectRecord {{
{:?},
  parent: {},
  prototype: {},
  transitions: {},
  properties: {:?},
  elements: {:?},
  hash: {},
  mode: {},
  fast_property_cap: {},
  fast_property_len: {}
}}",
      self.header,
      if self.parent.is_null() {
        String::from("null")
      } else {
        self.parent.short_description()
      },
      if self.prototype.is_null() {
        String::from("null")
      } else {
        format!("{:?}", self.prototype.record().shape())
      },
      if self.transitions.is_null() { 0 } else { self.transitions.len() },
      self.properties,
      self.elements,
      self.hash,
      if self.is_fast_mode() {
        "Fast"
      } else if self.is_external_field_mode() {
        "External Properties"
      } else if self.is_slow_mode() {
        "External Table"
      } else if self.is_element_mode() {
        "Elements"
      } else {
        "Element Table"
      },
      self.fast_properties_capacity(),
      self.fast_properties_len(),
    );
  }
}

#[repr(C)]
pub struct FullObjectRecordTraversal(BareHeapLayout<ObjectRecordLayout>);
impl_object!(FullObjectRecordTraversal, BareHeapLayout<ObjectRecordLayout>);

impl FullObjectRecordTraversal {
  #[inline]
  pub fn search_root_record(&self) -> FullObjectRecord {
    let mut parent = self.parent;
    while !parent.is_null() {
      parent = parent.parent;
    }
    return parent;
  }

  #[inline]
  pub fn search(&self, property: Property) -> Option<FullObjectRecord> {
    if self.transitions.is_null() {
      let transitions = self.transitions;
      for record in transitions {
        if record.property().name() == property.name() && record.property().desc() == property.desc() {
          return Some(record.next_record());
        }
      }
    }
    return None;
  }
}

impl From<FullObjectRecord> for FullObjectRecordTraversal {
  fn from(a: FullObjectRecord) -> FullObjectRecordTraversal {
    return FullObjectRecordTraversal(a.0);
  }
}

impl From<FullObjectRecordTraversal> for FullObjectRecord {
  fn from(a: FullObjectRecordTraversal) -> FullObjectRecord {
    return FullObjectRecord(a.0);
  }
}

impl From<FullObjectRecord> for ObjectRecord {
  fn from(a: FullObjectRecord) -> ObjectRecord {
    return ObjectRecord(BareHeapLayout::<LightObjectRecordLayout>::wrap(a.0.as_addr()));
  }
}

impl TryFrom<ObjectRecord> for FullObjectRecord {
  type Error = ();
  fn try_from(record: ObjectRecord) -> Result<FullObjectRecord, ()> {
    return if !record.is_light_layout() {
      Ok(FullObjectRecord(BareHeapLayout::<ObjectRecordLayout>::wrap(record.0.as_addr())))
    } else {
      Err(())
    };
  }
}

pub struct FullObjectRecordTraistion(FullObjectRecord);

impl FullObjectRecordTraistion {
  pub fn transition(&mut self, context: impl ObjectRecordsInitializedContext, property: Property) -> FullObjectRecord {
    if property.name().is_string() {
      if self.0.is_fast_mode() {
        return FullObjectRecordTraistion::transition_fast_mode(context, self.0, property);
      }
      return FullObjectRecordTraistion::transition_properties_mode(context, self.0, property);
    }
    return Self::transition_elements_mode(context, self.0, property);
  }

  fn transition_fast_mode(
    context: impl ObjectRecordsInitializedContext,
    mut old_record: FullObjectRecord,
    property: Property,
  ) -> FullObjectRecord {
    return match old_record.find_transition(property) {
      Some(t) => t,
      _ => {
        let new_record = FullObjectRecord::copy_fast(context, old_record);
        old_record.transition_with_record(context, property, new_record);
        new_record
      }
    };
  }

  fn transition_properties_mode(
    context: impl ObjectRecordsInitializedContext,
    old_record: FullObjectRecord,
    property: Property,
  ) -> FullObjectRecord {
    return match old_record.properties.into_storage() {
      Ok(array) => Self::transition_external_array_mode(context, old_record, property, array),
      Err(hash_map) => Self::transition_external_hashmap_mode(context, old_record, property, hash_map),
    };
  }

  fn transition_external_array_mode(
    context: impl ObjectRecordsInitializedContext,
    mut old_record: FullObjectRecord,
    property: Property,
    array: PropertyDescriptorArray,
  ) -> FullObjectRecord {
    return match old_record.find_transition(property) {
      Some(t) => t,
      _ => {
        let new_record = FullObjectRecord::copy_external_array(context, old_record, array);
        old_record.transition_with_record(context, property, new_record);
        new_record
      }
    };
  }

  fn transition_external_hashmap_mode(
    context: impl ObjectRecordsInitializedContext,
    mut old_record: FullObjectRecord,
    property: Property,
    hash_map: PropertyDescriptorTable,
  ) -> FullObjectRecord {
    return match old_record.find_transition(property) {
      Some(t) => t,
      _ => {
        let new_record = FullObjectRecord::copy_external_hashmap(context, old_record, hash_map);
        old_record.transition_with_record(context, property, new_record);
        new_record
      }
    };
  }

  fn transition_elements_mode(
    context: impl ObjectRecordsInitializedContext,
    old_record: FullObjectRecord,
    property: Property,
  ) -> FullObjectRecord {
    return match old_record.elements.into_storage() {
      Ok(array) => Self::transition_elements_array_mode(context, old_record, property, array),
      Err(hash_map) => Self::transition_elements_hashmap_mode(context, old_record, property, hash_map),
    };
  }

  fn transition_elements_array_mode(
    context: impl ObjectRecordsInitializedContext,
    mut old_record: FullObjectRecord,
    property: Property,
    array: PropertyDescriptorArray,
  ) -> FullObjectRecord {
    return match old_record.find_transition(property) {
      Some(t) => t,
      _ => {
        let new_record = FullObjectRecord::copy_elements_array(context, old_record, array);
        old_record.transition_with_record(context, property, new_record);
        new_record
      }
    };
  }

  fn transition_elements_hashmap_mode(
    context: impl ObjectRecordsInitializedContext,
    mut old_record: FullObjectRecord,
    property: Property,
    hash_map: PropertyDescriptorTable,
  ) -> FullObjectRecord {
    return match old_record.find_transition(property) {
      Some(t) => t,
      _ => {
        let new_record = FullObjectRecord::copy_elements_hashmap(context, old_record, hash_map);
        old_record.transition_with_record(context, property, new_record);
        new_record
      }
    };
  }
}

pub trait ObjectSkin {
  fn set_record(&mut self, record: ObjectRecord);
  fn record(&self) -> ObjectRecord;
  fn full_record(&self) -> Result<FullObjectRecord, ()> {
    return FullObjectRecord::try_from(self.record());
  }
  fn full_record_unchecked(&self) -> FullObjectRecord {
    return self.full_record().unwrap();
  }
}

macro_rules! impl_object_record {
  ($name:ty) => {
    impl ObjectSkin for $name {
      fn set_record(&mut self, record: crate::structs::ObjectRecord) {
        self.0.set_record(record);
      }
      fn record(&self) -> crate::structs::ObjectRecord {
        return self.0.record();
      }
    }
  };
}

pub struct FullObjectRecordBuilder {
  context: Repr,
  prototype: JsObject,
  base: FullObjectRecord,
  bits: Vec<usize>,
  size: u32,
  shape: Shape,
  fast_property_capacity: u32,
}

impl FullObjectRecordBuilder {
  pub fn new(context: impl AllocationOnlyContext, shape: Shape, size: u32, fast_property_capacity: u32) -> FullObjectRecordBuilder {
    return FullObjectRecordBuilder {
      context: context.into(),
      prototype: JsObject::default(),
      bits: Vec::new(),
      base: FullObjectRecord::default(),
      size,
      shape,
      fast_property_capacity,
    };
  }

  pub fn from_base(context: impl AllocationOnlyContext, base: FullObjectRecord) -> Self {
    return FullObjectRecordBuilder {
      context: context.into(),
      prototype: JsObject::default(),
      bits: Vec::new(),
      base,
      size: base.size() as u32,
      shape: base.shape(),
      fast_property_capacity: base.fast_properties_capacity() as u32,
    };
  }

  pub fn set_prototype(&mut self, prototype: JsObject) -> &mut Self {
    self.prototype = prototype;
    return self;
  }

  pub fn set_bit_field(&mut self, index: usize) -> &mut Self {
    self.bits.push(index);
    return self;
  }

  pub fn build(&mut self) -> FullObjectRecord {
    let context = LuxContext::from(self.context);
    let full_object_record = if !self.base.is_null() {
      FullObjectRecord::try_from(ObjectRecord::from(self.base).copy_with_size(context, self.size as usize)).unwrap()
    } else {
      FullObjectRecord::new(context, self.size, self.shape, self.fast_property_capacity)
    };

    let mut object_record = ObjectRecord::from(full_object_record);
    for index in &self.bits {
      object_record.data_field().set(*index);
    }

    return full_object_record;
  }
}
