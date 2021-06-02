use super::cell::*;
use super::js_object::*;
use super::natives::*;
use super::object::*;
use super::object_record::*;
use super::shape::*;
use super::string::*;
use crate::context::*;
use property::Property;
use std::mem::size_of;

#[repr(C)]
#[derive(Copy, Clone, Default, Property)]
pub struct ObjectRecordsLayout {
  #[property(get(type = "copy"))]
  root_object_record: FullObjectRecord,

  #[property(get(type = "copy"))]
  internal_array_record: ObjectRecord,

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
#[derive(Copy, Clone)]
pub struct ObjectRecords(HeapLayout<ObjectRecordsLayout>);
impl_object!(ObjectRecords, HeapLayout<ObjectRecordsLayout>);

impl ObjectRecords {
  const SIZE: usize = size_of::<ObjectRecordsLayout>();
  pub fn new(context: impl AllocationOnlyContext) -> ObjectRecords {
    let global_objects_record = ObjectRecord::new(context, ObjectRecords::SIZE as u32, Shape::object_records());
    let mut layout = HeapLayout::<ObjectRecordsLayout>::persist(context, global_objects_record);
    layout.internal_array_record = ObjectRecord::new(context, 0, Shape::internal_array());
    layout.hash_map_entry_record = ObjectRecord::new(context, 0, Shape::hash_map_entry());
    layout.hash_map_record = ObjectRecord::new(context, 0, Shape::hash_map());
    layout.string_piece_record = JsString::string_piece_record(context);
    layout.string_rope_record = JsString::string_rope_record(context);
    layout.small_string_record = JsString::small_string_record(context);
    layout.one_byte_char_record = JsString::one_byte_char_record(context);
    layout.flatten_string_record = JsString::flatten_string_record(context);
    layout.property_record = ObjectRecord::new(context, Property::SIZE as u32, Shape::property());
    layout.root_object_record = FullObjectRecordBuilder::new(context, Shape::root_object_record(), 0).build();
    layout.string_record = layout.root_object_record.transition(
      context,
      PropertyName::from_utf8_string(context, "String"),
      FullObjectRecordBuilder::new(context, Shape::string(), JsString::SIZE as u32).build(),
    );
    layout.js_object_record = layout.root_object_record.transition(
      context,
      PropertyName::from_utf8_string(context, "Object"),
      FullObjectRecordBuilder::new(context, Shape::object(), JsObject::SIZE as u32).build(),
    );
    layout.data_descriptor_record = ObjectRecord::new(
      context,
      PropertyDescriptor::DATA_DESCRIPTOR_SIZE as u32,
      Shape::property_descriptor(),
    );
    layout.accessor_descriptor_record = ObjectRecord::new(
      context,
      PropertyDescriptor::ACCESSOR_DESCRIPTOR_SIZE as u32,
      Shape::property_descriptor(),
    );
    layout.builtins_record = ObjectRecord::new(context, Builtins::SIZE as u32, Shape::builtins());

    layout.symbol_record = layout.root_object_record.transition(
      context,
      PropertyName::from_utf8_string(context, "Symbol"),
      FullObjectRecordBuilder::new(context, Shape::symbol(), JsSymbol::SIZE as u32).build(),
    );

    layout.symbol_registry_record = ObjectRecord::new(context, SymbolRegistry::SIZE as u32, Shape::symbol_registry());

    layout.native_function_record = ObjectRecord::new(context, NATIVE_FUNCTION_SIZE as u32, Shape::native_function());

    layout.function_record = layout.root_object_record.transition(
      context,
      PropertyName::from_utf8_string(context, "Function"),
      FullObjectRecordBuilder::new(context, Shape::function(), JsFunction::SIZE as u32).build(),
    );

    layout.builtin_function_record = layout.root_object_record.transition(
      context,
      PropertyName::from_utf8_string(context, "BuiltinFunction"),
      FullObjectRecordBuilder::new(context, Shape::function(), JsFunction::SIZE as u32)
        .set_bit_field(JsFunction::BUILTIN_BIT)
        .build(),
    );

    layout.transition_record_record =
      ObjectRecord::new(context, TransitionRecord::SIZE as u32, Shape::transition_record());

    layout.function_prototype_record = layout.root_object_record.transition(
      context,
      PropertyName::from_utf8_string(context, "Function.prototype"),
      FullObjectRecordBuilder::new(context, Shape::function_prototype(), JsFunction::SIZE as u32).build(),
    );

    return ObjectRecords(layout);
  }
}
