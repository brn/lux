use super::cell::*;
use super::hash_map::HashMap;
use super::hash_map::PredefinedHash;
use super::internal_array::InternalArray;
use super::object::{FastOwnProperties, Header, JsObject, Property, PropertyDescriptor, PropertyName};
use super::repr::Repr;
use super::shape::{Shape, ShapeTag};
use crate::context::{AllocationOnlyContext, Context, LuxContext};
use crate::def::*;
use crate::utility::{BitOperator, MaskedBitsetMut};
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use std::boxed::Box;
use std::convert::TryFrom;
use std::marker::PhantomData;
use std::mem::size_of;

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct TransitionRecordLayout {
  property_name: PropertyName,
  next_record: FullObjectRecord,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct TransitionRecord(HeapLayout<TransitionRecordLayout>);
impl_object!(TransitionRecord, HeapLayout<TransitionRecordLayout>);

impl TransitionRecord {
  pub const SIZE: usize = size_of::<TransitionRecordLayout>();
  pub fn new(
    context: impl AllocationOnlyContext,
    property_name: PropertyName,
    next_record: FullObjectRecord,
  ) -> TransitionRecord {
    let layout =
      HeapLayout::<TransitionRecordLayout>::new(context, context.object_records().transition_record_record());
    layout.property_name = property_name;
    layout.next_record = next_record;
    return TransitionRecord(layout);
  }

  pub fn property_name(&self) -> PropertyName {
    return self.property_name;
  }

  pub fn next_record(&self) -> FullObjectRecord {
    return self.next_record;
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
      mode: PropertyMode::ELEMENT,
    };
  }
  pub fn new_with(key: PropertyName, index: isize, mode: PropertyMode) -> OwnPropertySearchHint {
    return OwnPropertySearchHint { key, index, mode };
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

  pub fn new_with(
    key: PropertyName,
    index: isize,
    mode: PropertyMode,
    full_object_record: FullObjectRecord,
  ) -> PropertySearchHint {
    return PropertySearchHint {
      own_property_search_hint: OwnPropertySearchHint::new_with(key, index, mode),
      full_object_record,
    };
  }
}

#[derive(Copy, Clone, FromPrimitive, PartialEq)]
enum PropertyMode {
  FAST = 0,
  EXTERNAL,
  ELEMENT,
  SLOW,
  INVALID,
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
    return self.property_mode == PropertyMode::FAST && self.index >= 0;
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

  pub fn into_storage(&self) -> Result<InternalArray<Property>, HashMap<PropertyName, PropertyDescriptor>> {
    match Cell::from(self.0).shape().tag() {
      ShapeTag::InternalArray => return Ok(InternalArray::<Property>::from(self.0)),
      ShapeTag::HashMap => return Err(HashMap::<PropertyName, PropertyDescriptor>::from(self.0)),
      _ => unreachable!(),
    }
  }

  pub fn is_null(&self) -> bool {
    return self.0.is_invalid();
  }
}

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct LightObjectRecordLayout {
  header: Header,
}

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct ObjectRecordLayout {
  header: Header,
  parent: FullObjectRecord,
  prototype: JsObject,
  transitions: TransitionArray,
  hash: u64,
  properties: PropertyStorageCoercible,
  elements: PropertyStorageCoercible,
  _data_field: PhantomData<u8>,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct ObjectRecord(HeapLayout<LightObjectRecordLayout>);
impl_object!(ObjectRecord, HeapLayout<LightObjectRecordLayout>);

#[derive(Copy, Clone, Property)]
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

struct PropertyLocation {
  target_property: Option<PropertyName>,
  own_property_len: u8,
  current: PropertyMode,
  index: usize,
  fast: FastOwnProperties,
  properties: PropertyStorageCoercible,
  elements: PropertyStorageCoercible,
}

pub struct OwnPropertyIterator(Option<PropertyLocation>);
impl Iterator for OwnPropertyIterator {
  type Item = OwnPropertyDescriptorSearchResult;

  fn next(&mut self) -> Option<Self::Item> {
    assert!(self.0.is_some());
    assert!(self.0.unwrap().current != PropertyMode::INVALID);
    let loc = &mut self.0.unwrap();
    loop {
      match loc.current {
        PropertyMode::ELEMENT => match self.search_element_properties(loc) {
          Some(result) => return Some(result),
          _ => {
            loc.index = 0;
            loc.current = PropertyMode::FAST;
          }
        },
        PropertyMode::FAST => match self.search_fast_own_properties(loc) {
          Some(result) => return Some(result),
          _ => {
            loc.index = 0;
            loc.current = PropertyMode::EXTERNAL;
          }
        },
        PropertyMode::EXTERNAL => match self.search_external_properties(loc) {
          Some(result) => return Some(result),
          None => return None,
        },
      }
    }
  }
}

impl OwnPropertyIterator {
  fn search_element_properties(&self, loc: &mut PropertyLocation) -> Option<OwnPropertyDescriptorSearchResult> {
    if loc.properties.is_null() {
      return None;
    }
    match loc.elements.into_storage() {
      Ok(array) => {
        if loc.index == array.len() {
          return None;
        }
        if loc.target_property.is_some() && loc.target_property.unwrap() != array[loc.index].name() {
          return None;
        }
        let p = array[loc.index];
        let ret = Some(OwnPropertyDescriptorSearchResult::new(
          p.name(),
          p.desc(),
          PropertyMode::ELEMENT,
          loc.index as isize,
        ));
        loc.index += 1;
        return ret;
      }
      Err(hash_map) => match hash_map.entry_from_index(loc.index) {
        Some((key, value)) => {
          if loc.target_property.is_some() && loc.target_property.unwrap() != key {
            return None;
          }
          let ret = Some(OwnPropertyDescriptorSearchResult::new(
            key,
            value,
            PropertyMode::ELEMENT,
            loc.index as isize,
          ));
          return ret;
        }
        _ => {
          return None;
        }
      },
    }
  }

  fn search_fast_own_properties(&self, loc: &mut PropertyLocation) -> Option<OwnPropertyDescriptorSearchResult> {
    if loc.index == loc.own_property_len as usize {
      return None;
    }
    let property = loc.fast.offset_ref(loc.index as isize);
    if loc.target_property.is_some() && loc.target_property.unwrap() != property.name() {
      return None;
    }
    let ret =
      OwnPropertyDescriptorSearchResult::new(property.name(), property.desc(), PropertyMode::FAST, loc.index as isize);
    loc.index += 1;
    return Some(ret);
  }

  fn search_external_properties(&self, loc: &mut PropertyLocation) -> Option<OwnPropertyDescriptorSearchResult> {
    if loc.properties.is_null() {
      return None;
    }
    match loc.properties.into_storage() {
      Ok(array) => {
        if loc.index == array.len() {
          loc.index = 0;
          loc.current = PropertyMode::INVALID;
          return None;
        }
        let property = array[loc.index];
        if loc.target_property.is_some() && loc.target_property.unwrap() != property.name() {
          return None;
        }
        let ret = Some(OwnPropertyDescriptorSearchResult::new(
          property.name(),
          property.desc(),
          PropertyMode::EXTERNAL,
          loc.index as isize,
        ));
        loc.index += 1;
        return ret;
      }
      Err(hash_map) => match hash_map.entry_from_index(loc.index) {
        Some((key, value)) => {
          if loc.target_property.is_some() && loc.target_property.unwrap() != key {
            return None;
          }
          let ret = Some(OwnPropertyDescriptorSearchResult::new(
            key,
            value,
            PropertyMode::SLOW,
            loc.index as isize,
          ));
          loc.index += 1;
          return ret;
        }
        _ => {
          loc.index = 0;
          loc.current = PropertyMode::INVALID;
          return None;
        }
      },
    }
  }
}

struct PropertyFinder {
  target_property: OwnPropertySearchHint,
  result: Option<OwnPropertyDescriptorSearchResult>,
}

const LIGHT_LAYOUT: usize = 1;
const EXTENSIBLE_INDEX: usize = 2;

const LIGHT_SIZE: u32 = size_of::<LightObjectRecordLayout>() as u32;
const FULL_SIZE: u32 = size_of::<ObjectRecordLayout>() as u32;

impl ObjectRecord {
  pub const SIZE: usize = LIGHT_SIZE as usize;
  pub fn new(context: impl AllocationOnlyContext, size: u32, shape: Shape) -> ObjectRecord {
    let mut base_layout = HeapLayout::<LightObjectRecordLayout>::new_raw(context, LIGHT_SIZE);
    let object_record = ObjectRecord(base_layout);
    object_record.header().set_size(size);
    object_record.header().set_shape(Shape::object_record());
    object_record.header().data().set(LIGHT_LAYOUT);
    return object_record;
  }

  pub fn new_into_heap(heap: Addr, size: u32, shape: Shape) -> ObjectRecord {
    let mut base_layout = HeapLayout::<LightObjectRecordLayout>::new_into_raw_heap(heap);
    let object_record = ObjectRecord(base_layout);
    object_record.header().set_size(size);
    object_record.header().set_shape(Shape::object_record());
    object_record.header().data().set(LIGHT_LAYOUT);
    return object_record;
  }

  pub fn copy_with_size(&self, context: impl AllocationOnlyContext, size: usize) -> ObjectRecord {
    let record_size = if self.is_light_layout() { LIGHT_SIZE } else { FULL_SIZE };
    let mut layout = HeapLayout::<LightObjectRecordLayout>::new_raw(context, record_size);
    unsafe { std::ptr::copy_nonoverlapping(self.raw_heap(), layout.as_addr(), record_size as usize) };
    layout.header.set_size(size as u32);
    return ObjectRecord(layout);
  }

  pub fn header(&self) -> &Header {
    return &self.header;
  }

  pub fn header_mut(&mut self) -> &mut Header {
    return &mut self.header;
  }

  pub fn size(&self) -> u32 {
    return self.header.size();
  }

  pub fn size_contains_object_record(&self) -> u32 {
    return self.size()
      + if self.is_light_layout() {
        LIGHT_SIZE
      } else {
        size_of::<ObjectRecord>() as u32
      };
  }

  pub fn shape_tag(&self) -> ShapeTag {
    return self.header.shape().tag();
  }

  pub fn data_field(&mut self) -> MaskedBitsetMut<u64> {
    return self.header.data_offset(EXTENSIBLE_INDEX);
  }

  pub fn is_light_layout(&self) -> bool {
    return self.header.data().get(LIGHT_LAYOUT);
  }
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct FullObjectRecord(HeapLayout<ObjectRecordLayout>);
impl_object!(FullObjectRecord, HeapLayout<ObjectRecordLayout>);

impl FullObjectRecord {
  pub fn new(context: impl Context, properties: InternalArray<Property>, size: u32, shape: Shape) -> FullObjectRecord {
    const SIZE: u32 = size_of::<LightObjectRecordLayout>() as u32;
    let mut base_layout = HeapLayout::<LightObjectRecordLayout>::new_raw(context, SIZE);
    let object_record = ObjectRecord(base_layout);
    object_record.header().set_size(size);
    object_record.header().set_shape(Shape::object_record());

    let mut layout = HeapLayout::<ObjectRecordLayout>::new(context, object_record);
    let full_object_record = FullObjectRecord(layout);
    FullObjectRecord::init_properties(context, full_object_record, properties);
    return full_object_record;
  }

  pub fn hash(&self) -> u64 {
    return self.hash;
  }

  pub fn set_hash(&self, hash: u64) {
    self.hash = hash;
  }

  pub fn is_fast_mode(&self) -> bool {
    let field = ObjectRecord::from(*self).data_field();
    return !field.get(PropertyMode::EXTERNAL.into())
      && !field.get(PropertyMode::SLOW.into())
      && !field.get(PropertyMode::ELEMENT.into());
  }

  pub fn is_external_field_mode(&self) -> bool {
    let field = ObjectRecord::from(*self).data_field();
    return !field.get(PropertyMode::EXTERNAL.into());
  }

  pub fn set_external_field_mode(&self) {
    ObjectRecord::from(*self)
      .data_field()
      .set(PropertyMode::EXTERNAL.into());
  }

  pub fn is_element_mode(&self) -> bool {
    let field = ObjectRecord::from(*self).data_field();
    return !field.get(PropertyMode::ELEMENT.into()) && !field.get(PropertyMode::ELEMENT.into());
  }

  pub fn is_slow_mode(&self) -> bool {
    let field = ObjectRecord::from(*self).data_field();
    return !field.get(PropertyMode::SLOW.into());
  }

  pub fn own_properties_len(&self) -> u8 {
    return ObjectRecord::from(*self).header().own_properties_len();
  }

  pub fn is_transited(&self) -> bool {
    return !self.transitions.is_null();
  }

  pub fn transition(
    &mut self,
    context: impl AllocationOnlyContext,
    property_name: PropertyName,
    object_record: FullObjectRecord,
  ) -> FullObjectRecord {
    if self.transitions.is_null() {
      self.transitions = TransitionArray::new(context, 10);
    }
    if self.transitions.is_full() {
      self.transitions = TransitionArray::expand_and_copy(context, self.transitions);
    }
    self
      .transitions
      .push(TransitionRecord::new(context, property_name, object_record));
    object_record.parent = *self;
    return object_record;
  }

  pub fn prototype(&self) -> Option<JsObject> {
    return if !self.prototype.is_null() {
      Some(self.prototype)
    } else {
      None
    };
  }

  pub fn set_prototype(&mut self, proto: JsObject) -> bool {
    self.prototype = proto;
    return true;
  }

  pub fn is_extensible(&self) -> bool {
    return self.header.data().get(EXTENSIBLE_INDEX);
  }

  pub fn prevent_extensions(&mut self) -> bool {
    self.header.data().unset(EXTENSIBLE_INDEX);
    return true;
  }

  pub fn define_own_property(
    &mut self,
    context: impl AllocationOnlyContext,
    property: Property,
  ) -> OwnPropertyDescriptorSearchResult {
    if property.name().is_number() {
      self.set_external_field_mode();
    } else if self.is_fast_mode() {
      return FullObjectRecord::define_fast_property(self, context, property);
    }
    return FullObjectRecord::define_external_property(self, context, property);
  }

  pub fn define_own_properties(&self, context: impl Context, properties: InternalArray<Property>) {
    FullObjectRecord::init_properties(context, *self, properties);
  }

  pub fn has_property(&self, hint: PropertySearchHint) -> Option<PropertyDescriptorSearchResult> {
    if hint.full_object_record().is_null() {
      let mut maybe_full_object_record = self.prototype();
      while maybe_full_object_record.is_some() {
        let full_object_record = maybe_full_object_record.unwrap().full_record_unchecked();
        let result = full_object_record.get_own_property(hint.own_property_search_hint());
        if result.is_some() {
          return Some(PropertyDescriptorSearchResult::new(
            &result.unwrap(),
            full_object_record,
          ));
        }
        maybe_full_object_record = full_object_record.prototype();
      }
      return None;
    }
    match hint
      .full_object_record
      .get_own_property(hint.own_property_search_hint())
    {
      Some(result) => {
        return Some(PropertyDescriptorSearchResult::new(&result, hint.full_object_record()));
      }
      _ => return None,
    }
  }

  pub fn get_own_property(&self, hint: OwnPropertySearchHint) -> Option<OwnPropertyDescriptorSearchResult> {
    let iterator = FullObjectRecord::init_own_property_iterator_with_hint(self, hint);
    if hint.index() >= 0 {
      return iterator.next();
    }
    for p in iterator {
      if p.name() == hint.key() {
        return Some(p);
      }
    }
    return None;
  }

  pub fn own_property_keys(&mut self, context: impl Context) -> InternalArray<PropertyName> {
    let iterator = FullObjectRecord::init_own_property_iterator(self);
    let estimated_len = self.own_properties_len() as usize;
    let mut array = InternalArray::<PropertyName>::new(context, if estimated_len == 0 { 10 } else { estimated_len });
    for r in iterator {
      array = array.push_safe(context, r.name());
    }
    return array;
  }

  fn init_properties(context: impl Context, this: FullObjectRecord, properties: InternalArray<Property>) {
    for p in properties {
      p.name().prepare_hash(context);
      p.name().predefined_hash();
      FullObjectRecord::define_fast_property(&mut this, context, p);
    }
  }

  fn define_fast_property(
    this: &mut Self,
    context: impl AllocationOnlyContext,
    property: Property,
  ) -> OwnPropertyDescriptorSearchResult {
    let offset = this.own_properties_len();
    ObjectRecord::from(*this)
      .header_mut()
      .set_own_properties_len(this.own_properties_len() + 1);
    FastOwnProperties::from(*this).add_property(offset, property);
    return OwnPropertyDescriptorSearchResult::new(property.name(), property.desc(), PropertyMode::FAST, offset.into());
  }

  fn define_element_property(
    this: &mut Self,
    context: impl AllocationOnlyContext,
    property: Property,
  ) -> OwnPropertyDescriptorSearchResult {
    let index = property.name().to_number_unchecked() as u32;
    if this.elements.is_null() {
      if index > 7 {
        this.elements = PropertyStorageCoercible::new(HashMap::<PropertyName, PropertyDescriptor>::new(context).into());
      } else {
        this.elements = PropertyStorageCoercible::new(InternalArray::<Property>::new(context, 7).into());
      }
    }

    let mut storage_index = 0;
    match this.elements.into_storage() {
      Ok(mut array) => {
        if (index as usize) < array.len() {
          array = array.push_safe(context, property);
          storage_index = index;
        } else {
          let hash = HashMap::<PropertyName, PropertyDescriptor>::new(context);
          for p in array.into_iter() {
            hash.insert(context, p.name(), p.desc());
          }
          storage_index = hash.insert(context, property.name(), property.desc()) as u32;
          this.elements = PropertyStorageCoercible(hash.into());
        }
      }
      Err(hash_map) => {
        storage_index = hash_map.insert(context, property.name(), property.desc()) as u32;
      }
    }

    return OwnPropertyDescriptorSearchResult::new(
      property.name(),
      property.desc(),
      PropertyMode::ELEMENT,
      storage_index as isize,
    );
  }

  fn define_external_property(
    this: &mut Self,
    context: impl AllocationOnlyContext,
    property: Property,
  ) -> OwnPropertyDescriptorSearchResult {
    match this.properties.into_storage() {
      Ok(array) => return FullObjectRecord::define_external_array_proeprty(this, context, array, property),
      Err(hash_map) => return FullObjectRecord::define_external_hash_map_proeprty(this, context, hash_map, property),
    }
    unreachable!();
  }

  fn define_external_array_proeprty(
    this: &mut Self,
    context: impl AllocationOnlyContext,
    mut array: InternalArray<Property>,
    property: Property,
  ) -> OwnPropertyDescriptorSearchResult {
    if array.is_full() {
      array = InternalArray::expand_and_copy(context, array);
      this.properties = PropertyStorageCoercible::new(array.into());
    }
    let offset = array.len();
    array.push(property);
    return OwnPropertyDescriptorSearchResult::new(
      property.name(),
      property.desc(),
      PropertyMode::EXTERNAL,
      offset as isize,
    );
  }

  fn define_external_hash_map_proeprty(
    this: &mut Self,
    context: impl AllocationOnlyContext,
    hash_map: HashMap<PropertyName, PropertyDescriptor>,
    property: Property,
  ) -> OwnPropertyDescriptorSearchResult {
    property.name().prepare_hash(context);
    hash_map.insert(context, property.name(), property.desc());
    return OwnPropertyDescriptorSearchResult::new(property.name(), property.desc(), PropertyMode::SLOW, -1);
  }

  fn init_own_property_iterator(this: &Self) -> OwnPropertyIterator {
    return OwnPropertyIterator(Some(PropertyLocation {
      target_property: None,
      own_property_len: this.own_properties_len(),
      index: 0,
      current: PropertyMode::FAST,
      fast: FastOwnProperties::from(*this),
      properties: this.properties,
      elements: this.elements,
    }));
  }

  fn init_own_property_iterator_with_hint(this: &Self, hint: OwnPropertySearchHint) -> OwnPropertyIterator {
    return OwnPropertyIterator(Some(PropertyLocation {
      target_property: None,
      own_property_len: this.own_properties_len(),
      index: if hint.index() < 0 { 0 } else { hint.index() as usize },
      current: hint.mode(),
      fast: FastOwnProperties::from(*this),
      properties: this.properties,
      elements: this.elements,
    }));
  }
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct FullObjectRecordTraversal(HeapLayout<ObjectRecordLayout>);
impl_object!(FullObjectRecordTraversal, HeapLayout<ObjectRecordLayout>);

impl FullObjectRecordTraversal {
  pub fn search_root_record(&self) -> FullObjectRecord {
    let parent = self.parent;
    while !parent.is_null() {
      parent = parent.parent;
    }
    return parent;
  }

  pub fn search(&self, name: PropertyName) -> Option<FullObjectRecord> {
    if self.transitions.is_null() {
      let transitions = self.transitions;
      for record in transitions {
        if record.property_name() == name {
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
    return ObjectRecord(HeapLayout::<LightObjectRecordLayout>::wrap(a.0.as_addr()));
  }
}

impl TryFrom<ObjectRecord> for FullObjectRecord {
  type Error = ();
  fn try_from(record: ObjectRecord) -> Result<FullObjectRecord, ()> {
    return if !record.is_light_layout() {
      Ok(FullObjectRecord(HeapLayout::<ObjectRecordLayout>::wrap(
        record.0.as_addr(),
      )))
    } else {
      Err(())
    };
  }
}

impl From<FullObjectRecord> for FastOwnProperties {
  fn from(a: FullObjectRecord) -> FastOwnProperties {
    return FastOwnProperties::wrap(unsafe {
      std::mem::transmute::<&PhantomData<u8>, *mut Property>(&a.0._data_field)
    });
  }
}

pub trait ObjectSkin {
  fn set_record(&self, record: ObjectRecord);
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
      fn set_record(&self, record: ObjectRecord) {
        self.0.set_record(record);
      }
      fn record(&self) -> ObjectRecord {
        return self.0.record();
      }
    }
  };
}

pub struct FullObjectRecordBuilder {
  context: Repr,
  prototype: JsObject,
  properties: Vec<Property>,
  base: FullObjectRecord,
  bits: Vec<usize>,
  size: u32,
  shape: Shape,
}

impl FullObjectRecordBuilder {
  pub fn new(context: impl AllocationOnlyContext, shape: Shape, size: u32) -> FullObjectRecordBuilder {
    return FullObjectRecordBuilder {
      context: context.into(),
      properties: Vec::new(),
      prototype: JsObject::default(),
      bits: Vec::new(),
      base: FullObjectRecord::default(),
      size,
      shape,
    };
  }

  pub fn from_base(context: impl AllocationOnlyContext, base: FullObjectRecord) -> Self {
    return FullObjectRecordBuilder {
      context: context.into(),
      properties: Vec::new(),
      prototype: JsObject::default(),
      bits: Vec::new(),
      base,
      size: base.size() as u32,
      shape: base.shape(),
    };
  }

  pub fn add_peroperty(&self, name: PropertyName, value: Repr) -> Self {
    self
      .properties
      .push(new_property!(LuxContext::from(self.context), value: name, value));
    return *self;
  }

  pub fn set_prototype(&self, prototype: JsObject) -> Self {
    self.prototype = prototype;
    return *self;
  }

  pub fn set_bit_field(&self, index: usize) -> Self {
    self.bits.push(index);
    return *self;
  }

  pub fn build(&self) -> FullObjectRecord {
    let context = LuxContext::from(self.context);
    if !self.base.is_null() {
      let full_object_record =
        FullObjectRecord::try_from(ObjectRecord::from(self.base).copy_with_size(context, self.size as usize));
    }
    let props = InternalArray::<Property>::new(context, self.properties.len());
    for p in self.properties {
      props.push(p);
    }
    let full_object_record = FullObjectRecord::new(context, props, self.size, self.shape);
    let object_record = ObjectRecord::from(full_object_record);
    for index in self.bits {
      object_record.data_field().set(index);
    }
    return full_object_record;
  }
}
