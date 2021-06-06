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

#[repr(C)]
#[derive(Copy, Clone, Default, Prop)]
pub struct TransitionRecord {
  #[property(get(type = "copy"))]
  property_name: PropertyName,

  #[property(get(type = "copy"))]
  next_record: FullObjectRecord,
}

impl TransitionRecord {
  pub fn new(property_name: PropertyName, next_record: FullObjectRecord) -> TransitionRecord {
    return TransitionRecord {
      property_name,
      next_record,
    };
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
pub enum PropertyMode {
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

struct PropertyLocation {
  target_property: Option<PropertyName>,
  current: PropertyMode,
  index: usize,
  fast: Result<FastOwnProperties, ()>,
  properties: PropertyStorageCoercible,
  elements: PropertyStorageCoercible,
  fast_property_len: usize,
}

pub struct OwnPropertyIterator(Option<PropertyLocation>);
impl Iterator for OwnPropertyIterator {
  type Item = OwnPropertyDescriptorSearchResult;

  fn next(&mut self) -> Option<Self::Item> {
    loop {
      match self.property_location_mut().current {
        PropertyMode::ELEMENT => match self.search_element_properties() {
          Some(result) => return Some(result),
          _ => {
            let loc = self.property_location_mut();
            loc.index = 0;
            loc.current = PropertyMode::FAST;
          }
        },
        PropertyMode::FAST => match self.search_fast_own_properties() {
          Some(result) => return Some(result),
          _ => {
            let loc = self.property_location_mut();
            loc.index = 0;
            loc.current = PropertyMode::EXTERNAL;
          }
        },
        PropertyMode::EXTERNAL | PropertyMode::SLOW => match self.search_external_properties() {
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
    assert!(loc.current != PropertyMode::INVALID);
    return loc;
  }

  fn search_element_properties(&mut self) -> Option<OwnPropertyDescriptorSearchResult> {
    let loc = self.property_location_mut();
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

  fn search_fast_own_properties(&mut self) -> Option<OwnPropertyDescriptorSearchResult> {
    let loc = self.property_location_mut();
    if loc.index == loc.fast_property_len {
      return None;
    }
    match loc.fast {
      Ok(fast) => {
        let property = fast.offset_ref(loc.index as isize);
        if loc.target_property.is_some() && loc.target_property.unwrap() != property.name() {
          return None;
        }
        let ret = OwnPropertyDescriptorSearchResult::new(
          property.name(),
          property.desc(),
          PropertyMode::FAST,
          loc.index as isize,
        );
        loc.index += 1;
        return Some(ret);
      }
      _ => return None,
    }
  }

  fn search_external_properties(&mut self) -> Option<OwnPropertyDescriptorSearchResult> {
    let loc = self.property_location_mut();
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

#[repr(packed)]
#[derive(Copy, Clone, Default)]
pub struct FastPropertiesLayout {
  fast_property_offset: u32,
  fast_property_capacity: u32,
  fast_property_len: u32,
  _pad: u32,
}

#[repr(C)]
#[derive(Copy, Clone)]
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

const FULL_OBJECT_RECORD_LAYOUT_SIZE: usize = size_of::<ObjectRecordLayout>();
const EMBEDDABLE_LAYOUT_SIZE: usize = size_of::<FastPropertiesLayout>();

#[repr(C)]
#[derive(Copy, Clone)]
pub struct FullObjectRecord(BareHeapLayout<ObjectRecordLayout>);
impl_bare_object!(FullObjectRecord, BareHeapLayout<ObjectRecordLayout>);

impl FullObjectRecord {
  const EMBEDDED_PROPERTY_SIZE: usize = 7;
  pub fn new(mut context: impl Context, size: u32, shape: Shape, fast_property_len: u32) -> FullObjectRecord {
    let mut object_record = ObjectRecord::new_into_heap(
      context.allocate(
        FULL_OBJECT_RECORD_LAYOUT_SIZE
          + if fast_property_len == 0 {
            0
          } else {
            EMBEDDABLE_LAYOUT_SIZE
          },
      ),
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

  pub fn hash(&self) -> u64 {
    return self.hash;
  }

  pub fn set_hash(&mut self, hash: u64) {
    self.hash = hash;
  }

  pub fn fast_properties_capacity(&self) -> usize {
    return match FullObjectRecord::get_fast_properties_layout(*self) {
      Ok(layout) => layout.fast_property_capacity as usize,
      _ => 0,
    };
  }

  pub fn fast_properties_len(&self) -> usize {
    return match FullObjectRecord::get_fast_properties_layout(*self) {
      Ok(layout) => layout.fast_property_len as usize,
      _ => 0,
    };
  }

  pub fn is_enable_fast_mode(&self) -> bool {
    return self.header.data().get(EMBED_PROPERTY);
  }

  pub fn is_fast_mode(&self) -> bool {
    let mut or = ObjectRecord::from(*self);
    let field = or.data_field();
    return !field.get(PropertyMode::EXTERNAL.into())
      && !field.get(PropertyMode::SLOW.into())
      && !field.get(PropertyMode::ELEMENT.into());
  }

  pub fn is_external_field_mode(&self) -> bool {
    let mut or = ObjectRecord::from(*self);
    let field = or.data_field();
    return field.get(PropertyMode::EXTERNAL.into());
  }

  pub fn set_external_field_mode(&self) {
    ObjectRecord::from(*self)
      .data_field()
      .set(PropertyMode::EXTERNAL.into());
  }

  pub fn is_element_mode(&self) -> bool {
    let mut or = ObjectRecord::from(*self);
    let field = or.data_field();
    return field.get(PropertyMode::ELEMENT.into()) && !field.get(PropertyMode::ELEMENT.into());
  }

  pub fn set_element_mode(&self) {
    ObjectRecord::from(*self).data_field().set(PropertyMode::ELEMENT.into());
  }

  pub fn is_slow_mode(&self) -> bool {
    let mut or = ObjectRecord::from(*self);
    let field = or.data_field();
    return field.get(PropertyMode::SLOW.into());
  }

  pub fn own_properties_len(&self) -> usize {
    return self.fast_properties_len() + self.properties.len() + self.elements.len();
  }

  pub fn is_transited(&self) -> bool {
    return !self.transitions.is_null();
  }

  pub fn transition(
    &mut self,
    context: impl ObjectRecordsInitializedContext,
    property_name: PropertyName,
    mut object_record: FullObjectRecord,
  ) -> FullObjectRecord {
    if self.transitions.is_null() {
      self.transitions = TransitionArray::new(context, 10);
    }
    self.transitions = self
      .transitions
      .push_safe(context, TransitionRecord::new(property_name, object_record));
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
    return !self.header.data().get(NOT_EXTENSIBLE_INDEX);
  }

  pub fn prevent_extensions(&mut self) -> bool {
    self.header.data().set(NOT_EXTENSIBLE_INDEX);
    return true;
  }

  pub fn define_own_property(
    this: Self,
    context: impl ObjectRecordsInitializedContext,
    receiver: JsReceiver,
    property: Property,
  ) -> OwnPropertyDescriptorSearchResult {
    if property.name().is_number() {
      this.set_element_mode();
      return Self::define_element_property(this, context, property);
    }
    if this.is_fast_mode() && this.is_enable_fast_mode() {
      return Self::define_fast_property(this, context, receiver, property);
    }
    return Self::define_external_property(this, context, property);
  }

  pub fn define_own_properties(
    this: Self,
    context: impl Context,
    receiver: JsReceiver,
    properties: InternalArray<Property>,
  ) {
    for p in properties.into_iter() {
      Self::define_own_property(this, context, receiver, p);
    }
  }

  pub fn has_property(
    this: Self,
    receiver: JsReceiver,
    hint: PropertySearchHint,
  ) -> Option<PropertyDescriptorSearchResult> {
    if hint.full_object_record().is_null() {
      let mut maybe_full_object_record = this.prototype();
      while maybe_full_object_record.is_some() {
        let full_object_record = maybe_full_object_record.unwrap().full_record_unchecked();
        let result = Self::get_own_property(full_object_record, receiver, hint.own_property_search_hint());
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
    match Self::get_own_property(hint.full_object_record, receiver, hint.own_property_search_hint()) {
      Some(result) => {
        return Some(PropertyDescriptorSearchResult::new(&result, hint.full_object_record()));
      }
      _ => return None,
    }
  }

  pub fn get_own_property(
    this: Self,
    receiver: JsReceiver,
    hint: OwnPropertySearchHint,
  ) -> Option<OwnPropertyDescriptorSearchResult> {
    let mut iterator = FullObjectRecord::init_own_property_iterator_with_hint(this, receiver, hint);
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

  pub fn own_property_keys(
    this: Self,
    context: impl ObjectRecordsInitializedContext,
    receiver: JsReceiver,
  ) -> InternalArray<PropertyName> {
    let iterator = FullObjectRecord::init_own_property_iterator(this, receiver);
    let estimated_len = this.own_properties_len() as usize;
    let mut array = InternalArray::<PropertyName>::new(context, if estimated_len == 0 { 10 } else { estimated_len });
    for r in iterator {
      array = array.push_safe(context, r.name());
    }
    return array;
  }

  fn define_fast_property(
    this: Self,
    context: impl ObjectRecordsInitializedContext,
    receiver: JsReceiver,
    property: Property,
  ) -> OwnPropertyDescriptorSearchResult {
    let mut layout = Self::get_fast_properties_layout_unchecked(this);
    if layout.fast_property_capacity <= layout.fast_property_len {
      return Self::define_external_property(this, context, property);
    }
    let mut fast_own_properties = FullObjectRecord::fast_own_properties_unchecked(this, receiver);
    fast_own_properties.add_property(layout.fast_property_capacity, layout.fast_property_len, property);
    layout.fast_property_len += 1;
    return OwnPropertyDescriptorSearchResult::new(
      property.name(),
      property.desc(),
      PropertyMode::FAST,
      (layout.fast_property_len - 1) as isize,
    );
  }

  fn define_element_property(
    mut this: Self,
    context: impl ObjectRecordsInitializedContext,
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

    let storage_index = match this.elements.into_storage() {
      Ok(mut array) => {
        if (index as usize) < array.len() {
          this.elements = PropertyStorageCoercible::new(array.push_safe(context, property).into());
          index
        } else {
          let mut hash = HashMap::<PropertyName, PropertyDescriptor>::new(context);
          for p in array.into_iter() {
            hash.insert(context, p.name(), p.desc());
          }
          let storage_index = hash.insert(context, property.name(), property.desc()) as u32;
          this.elements = PropertyStorageCoercible(hash.into());
          storage_index
        }
      }
      Err(mut hash_map) => hash_map.insert(context, property.name(), property.desc()) as u32,
    };

    return OwnPropertyDescriptorSearchResult::new(
      property.name(),
      property.desc(),
      PropertyMode::ELEMENT,
      storage_index as isize,
    );
  }

  fn define_external_property(
    this: Self,
    context: impl ObjectRecordsInitializedContext,
    property: Property,
  ) -> OwnPropertyDescriptorSearchResult {
    match this.properties.into_storage() {
      Ok(array) => return FullObjectRecord::define_external_array_proeprty(this, context, array, property),
      Err(hash_map) => return FullObjectRecord::define_external_hash_map_proeprty(context, hash_map, property),
    }
    unreachable!();
  }

  fn define_external_array_proeprty(
    mut this: Self,
    context: impl ObjectRecordsInitializedContext,
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
    context: impl ObjectRecordsInitializedContext,
    mut hash_map: HashMap<PropertyName, PropertyDescriptor>,
    property: Property,
  ) -> OwnPropertyDescriptorSearchResult {
    property.name().prepare_hash(context);
    hash_map.insert(context, property.name(), property.desc());
    return OwnPropertyDescriptorSearchResult::new(property.name(), property.desc(), PropertyMode::SLOW, -1);
  }

  fn init_own_property_iterator(this: Self, receiver: JsReceiver) -> OwnPropertyIterator {
    return OwnPropertyIterator(Some(PropertyLocation {
      target_property: None,
      index: 0,
      current: PropertyMode::FAST,
      fast: FullObjectRecord::fast_own_properties(this, receiver),
      properties: this.properties,
      elements: this.elements,
      fast_property_len: this.fast_properties_len(),
    }));
  }

  fn init_own_property_iterator_with_hint(
    this: Self,
    receiver: JsReceiver,
    hint: OwnPropertySearchHint,
  ) -> OwnPropertyIterator {
    return OwnPropertyIterator(Some(PropertyLocation {
      target_property: None,
      index: if hint.index() < 0 { 0 } else { hint.index() as usize },
      current: hint.mode(),
      fast: FullObjectRecord::fast_own_properties(this, receiver),
      properties: this.properties,
      elements: this.elements,
      fast_property_len: this.fast_properties_len(),
    }));
  }

  fn get_embedded_properties_head_addr(this: Self, receiver: JsReceiver) -> Addr {
    return unsafe {
      Cell::from(receiver.object())
        .get_body()
        .offset(Self::get_fast_properties_layout_unchecked(this).fast_property_offset as isize)
    };
  }

  fn fast_own_properties(this: Self, receiver: JsReceiver) -> Result<FastOwnProperties, ()> {
    if this.is_enable_fast_mode() {
      return Ok(FastOwnProperties::from(
        FullObjectRecord::get_embedded_properties_head_addr(this, receiver),
      ));
    }
    return Err(());
  }

  fn fast_own_properties_unchecked(this: Self, receiver: JsReceiver) -> FastOwnProperties {
    return FastOwnProperties::from(FullObjectRecord::get_embedded_properties_head_addr(this, receiver));
  }

  fn get_fast_properties_layout(this: FullObjectRecord) -> Result<BareHeapLayout<FastPropertiesLayout>, ()> {
    if this.is_enable_fast_mode() {
      return Ok(BareHeapLayout::<FastPropertiesLayout>::wrap(unsafe {
        this.raw_heap().offset(FULL_OBJECT_RECORD_LAYOUT_SIZE as isize)
      }));
    }
    Err(())
  }

  fn get_fast_properties_layout_unchecked(this: FullObjectRecord) -> BareHeapLayout<FastPropertiesLayout> {
    return BareHeapLayout::<FastPropertiesLayout>::wrap(this.raw_heap());
  }
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct FullObjectRecordTraversal(BareHeapLayout<ObjectRecordLayout>);
impl_object!(FullObjectRecordTraversal, BareHeapLayout<ObjectRecordLayout>);

impl FullObjectRecordTraversal {
  pub fn search_root_record(&self) -> FullObjectRecord {
    let mut parent = self.parent;
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
    return ObjectRecord(BareHeapLayout::<LightObjectRecordLayout>::wrap(a.0.as_addr()));
  }
}

impl TryFrom<ObjectRecord> for FullObjectRecord {
  type Error = ();
  fn try_from(record: ObjectRecord) -> Result<FullObjectRecord, ()> {
    return if !record.is_light_layout() {
      Ok(FullObjectRecord(BareHeapLayout::<ObjectRecordLayout>::wrap(
        record.0.as_addr(),
      )))
    } else {
      Err(())
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
      fn set_record(&mut self, record: ObjectRecord) {
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
  base: FullObjectRecord,
  bits: Vec<usize>,
  size: u32,
  shape: Shape,
  fast_property_capacity: u32,
}

impl FullObjectRecordBuilder {
  pub fn new(
    context: impl AllocationOnlyContext,
    shape: Shape,
    size: u32,
    fast_property_capacity: u32,
  ) -> FullObjectRecordBuilder {
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
