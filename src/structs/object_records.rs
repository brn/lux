use super::cell::*;
use super::js_object::*;
use super::natives::*;
use super::object::*;
use super::object_record::*;
use super::repr::*;
use super::shape::*;
use super::string::*;
use crate::context::*;
use property::Property;
use std::mem::size_of;

#[repr(C)]
#[derive(Copy, Clone, Default, Property)]
pub struct ObjectRecordsLayout {
  #[property(get(type = "copy"))]
  internal_array_record: ObjectRecord,

  #[property(get(type = "copy"))]
  root_object_record: FullObjectRecord,

  #[property(get(type = "copy"))]
  hash_map_entry_record: ObjectRecord,

  #[property(get(type = "copy"))]
  hash_map_record: ObjectRecord,

  #[property(get(type = "copy"))]
  string_piece_record: ObjectRecord,

  #[property(get(type = "copy"))]
  string_rope_record: ObjectRecord,

  #[property(get(type = "copy"))]
  small_string_record: ObjectRecord,

  #[property(get(type = "copy"))]
  one_byte_char_record: ObjectRecord,

  #[property(get(type = "copy"))]
  flatten_string_record: ObjectRecord,

  #[property(get(type = "copy"))]
  property_record: ObjectRecord,

  #[property(get(type = "copy"))]
  data_descriptor_record: ObjectRecord,

  #[property(get(type = "copy"))]
  accessor_descriptor_record: ObjectRecord,

  #[property(get(type = "copy"))]
  builtins_record: ObjectRecord,

  #[property(get(type = "copy"))]
  string_record: FullObjectRecord,

  #[property(get(type = "copy"))]
  js_object_record: FullObjectRecord,

  #[property(get(type = "copy"))]
  symbol_record: FullObjectRecord,

  #[property(get(type = "copy"))]
  symbol_registry_record: ObjectRecord,

  #[property(get(type = "copy"))]
  transition_record_record: ObjectRecord,

  #[property(get(type = "copy"))]
  native_function_record: ObjectRecord,

  #[property(get(type = "copy"))]
  function_record: FullObjectRecord,

  #[property(get(type = "copy"))]
  builtin_function_record: FullObjectRecord,

  #[property(get(type = "copy"))]
  function_prototype_record: FullObjectRecord,
}

#[repr(C)]
pub struct ObjectRecords(HeapLayout<ObjectRecordsLayout>);
impl_object!(ObjectRecords, HeapLayout<ObjectRecordsLayout>);

impl ObjectRecords {
  const SIZE: usize = size_of::<ObjectRecordsLayout>();
  pub fn new(context: impl AllocationOnlyContext) -> ObjectRecords {
    let global_objects_record = ObjectRecord::new(context, ObjectRecords::SIZE as u32, Shape::object_records());
    let layout = HeapLayout::<ObjectRecordsLayout>::persist(context, global_objects_record);
    return ObjectRecords(layout);
  }

  pub fn initialize_internal_structs(&mut self, context: impl ObjectRecordsInitializedContext) {
    self.internal_array_record = ObjectRecord::new(context, 0, Shape::internal_array());
    self.hash_map_entry_record = ObjectRecord::new(context, 0, Shape::hash_map_entry());
    self.hash_map_record = ObjectRecord::new(context, 0, Shape::hash_map());
    self.property_record = ObjectRecord::new(context, Property::SIZE as u32, Shape::property());
    self.data_descriptor_record = ObjectRecord::new(
      context,
      PropertyDescriptor::DATA_DESCRIPTOR_SIZE as u32,
      Shape::property_descriptor(),
    );
    self.accessor_descriptor_record = ObjectRecord::new(
      context,
      PropertyDescriptor::ACCESSOR_DESCRIPTOR_SIZE as u32,
      Shape::property_descriptor(),
    );
    self.builtins_record = ObjectRecord::new(context, Builtins::SIZE as u32, Shape::builtins());
    self.symbol_registry_record = ObjectRecord::new(context, SymbolRegistry::SIZE as u32, Shape::symbol_registry());
    self.native_function_record = ObjectRecord::new(context, NATIVE_FUNCTION_SIZE as u32, Shape::native_function());
    // self.transition_record_record =
    //   ObjectRecord::new(context, TransitionRecord::SIZE as u32, Shape::transition_record());
  }

  pub fn initialize_js_objects(&mut self, context: impl ObjectRecordsInitializedContext) {
    self.string_piece_record = JsString::string_piece_record(context);
    self.string_rope_record = JsString::string_rope_record(context);
    self.small_string_record = JsString::small_string_record(context);
    self.one_byte_char_record = JsString::one_byte_char_record(context);
    self.flatten_string_record = JsString::flatten_string_record(context);
    self.root_object_record = FullObjectRecordBuilder::new(context, Shape::root_object_record(), 0, 0).build();
    self.string_record = FullObjectRecordBuilder::new(context, Shape::string(), JsString::SIZE as u32, 1).build();
    let string_record = self.string_record;
    self
      .root_object_record
      .transition_with_record(context, new_property!(context, str: "String", Repr::invalid()), string_record);
    self.js_object_record = self.root_object_record.transition_with_record(
      context,
      new_property!(context, str: "Object", Repr::invalid()),
      FullObjectRecordBuilder::new(context, Shape::object(), JsObject::SIZE as u32, 0).build(),
    );
    self.symbol_record = self.root_object_record.transition_with_record(
      context,
      new_property!(context, str: "Symbol", Repr::invalid()),
      FullObjectRecordBuilder::new(context, Shape::symbol(), JsSymbol::SIZE as u32, 0).build(),
    );
    self.function_record = self.root_object_record.transition_with_record(
      context,
      new_property!(context, str: "Function", Repr::invalid()),
      FullObjectRecordBuilder::new(context, Shape::function(), JsFunction::SIZE as u32, 2).build(),
    );

    self.builtin_function_record = self.root_object_record.transition_with_record(
      context,
      new_property!(context, str: "BuiltinFunction", Repr::invalid()),
      FullObjectRecordBuilder::new(context, Shape::function(), JsFunction::SIZE as u32, 2)
        .set_bit_field(JsFunction::BUILTIN_BIT)
        .build(),
    );

    self.function_prototype_record = self.root_object_record.transition_with_record(
      context,
      new_property!(context, str: "Function.prototype", Repr::invalid()),
      FullObjectRecordBuilder::new(context, Shape::function_prototype(), JsFunction::SIZE as u32, 4).build(),
    );
  }
}
