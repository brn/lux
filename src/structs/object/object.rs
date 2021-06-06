use super::super::cell::*;
use super::super::internal_array::InternalArray;
use super::super::object_record::{
  FullObjectRecord, ObjectSkin, OwnPropertyDescriptorSearchResult, OwnPropertySearchHint, PropertySearchHint,
};
use super::super::repr::Repr;
use super::super::shape::{Shape, ShapeTag};
use super::super::string::JsString;
use super::property::{Property, PropertyName};
use super::receiver::JsReceiver;
use super::symbol::WellKnownSymbolType;
use crate::context::Context;
use std::mem::size_of;

#[derive(Copy, Clone, Default)]
pub struct JsObjectLayout;

#[derive(Copy, Clone)]
pub struct JsObject(HeapLayout<JsObjectLayout>);
impl_object!(JsObject, HeapLayout<JsObjectLayout>);

#[derive(Copy, Clone)]
pub enum PreferredType {
  Default,
  String,
  Number,
}

impl JsObject {
  pub const SIZE: usize = size_of::<JsObjectLayout>();
  pub fn new(context: impl Context, object_record: FullObjectRecord) -> JsObject {
    let layout = HeapLayout::<JsObjectLayout>::new_object(context, object_record);
    return JsObject(layout);
  }

  pub fn new_with_initial_record(context: impl Context) -> JsObject {
    let layout = HeapLayout::<JsObjectLayout>::new_object(context, context.object_records().js_object_record());
    return JsObject(layout);
  }

  pub fn get_prototype_of(object: impl ObjectSkin) -> Option<JsObject> {
    return object.full_record_unchecked().prototype();
  }

  pub fn set_prototype_of(object: impl ObjectSkin, prototype: JsObject) -> bool {
    return object.full_record_unchecked().set_prototype(prototype);
  }

  pub fn is_extensible(object: impl ObjectSkin) -> bool {
    return object.full_record_unchecked().is_extensible();
  }

  pub fn prevent_extensions(object: impl ObjectSkin) -> bool {
    return object.full_record_unchecked().prevent_extensions();
  }

  pub fn get_own_property(object: Repr, hint: OwnPropertySearchHint) -> Option<OwnPropertyDescriptorSearchResult> {
    debug_assert!(object.is_object());
    return FullObjectRecord::get_own_property(object.full_record_unchecked(), JsReceiver::new(object), hint);
  }

  pub fn define_own_property(
    context: impl Context,
    object: Repr,
    property: Property,
  ) -> OwnPropertyDescriptorSearchResult {
    return FullObjectRecord::define_own_property(
      object.full_record_unchecked(),
      context,
      JsReceiver::new(object),
      property,
    );
  }

  pub fn has_property(object: Repr, hint: PropertySearchHint) -> bool {
    return FullObjectRecord::has_property(object.full_record_unchecked(), JsReceiver::new(object), hint).is_some();
  }

  pub fn own_property_keys(context: impl Context, object: Repr) -> InternalArray<PropertyName> {
    return FullObjectRecord::own_property_keys(object.full_record_unchecked(), context, JsReceiver::new(object));
  }

  // https://262.ecma-international.org/11.0/#sec-ordinary-object-internal-methods-and-internal-slots-get-p-receiver
  pub fn get(context: impl Context, p: Repr, receiver: Repr) -> Repr {
    return Repr::js_undefined();
  }

  pub fn set(context: impl Context, name: PropertyName, value: Repr, receiver: Repr) -> bool {
    return false;
  }

  pub fn delete(context: impl Context, name: PropertyName, receiver: Repr) -> bool {
    return false;
  }

  pub fn call(context: impl Context, receiver: Repr, args: InternalArray<Repr>) -> Repr {
    return Repr::invalid();
  }

  pub fn construct(context: impl Context, args: InternalArray<Repr>, target: JsObject) -> JsObject {
    return JsObject::from(Repr::invalid());
  }

  // https://262.ecma-international.org/11.0/#sec-toobject
  pub fn to_object(_context: impl Context, object: Repr) -> Repr {
    if object.is_js_null() || object.is_js_undefined() {
      // TypeError
      unreachable!();
    }
    if object.is_boxed() {
      match Cell::from(object.unbox_unchecked()).shape().tag() {
        ShapeTag::Object => {
          return JsObject::from(object).into();
        }
        ShapeTag::String => {
          return JsString::to_object(object).into();
        }
        // TODO: implements boxed type
        _ => unreachable!(),
      }
    }
    unreachable!();
  }

  // https://262.ecma-international.org/11.0/#sec-iscallable
  pub fn is_callable(o: Repr) -> bool {
    if o.is_boxed() {
      // TODO: implements [[Call]] slot.
      match Cell::from(o.unbox_unchecked()).shape().tag() {
        ShapeTag::Function => return true,
        _ => return false,
      }
    }
    return false;
  }

  // https://262.ecma-international.org/11.0/#sec-getv
  pub fn get_value(context: impl Context, object: Repr, p: Repr) -> Repr {
    assert!(JsObject::is_property_key(p));
    let o = JsObject::to_object(context, object);
    return JsObject::get(context, p, o);
  }

  pub fn is_property_key(object: Repr) -> bool {
    if object.is_boxed() {
      match Cell::from(object.unbox_unchecked()).shape().tag() {
        ShapeTag::String | ShapeTag::Symbol => return true,
        _ => return false,
      }
    }
    return false;
  }

  pub fn get_method(context: impl Context, object: Repr, key: Repr) -> Repr {
    if JsObject::is_property_key(key) {
      let func = JsObject::get_value(context, object, key);
      if func.is_js_undefined() || func.is_js_null() {
        return Repr::js_undefined();
      }
      if JsObject::is_callable(func) {
        return func;
      }
      unreachable!();
      // TODO: Throw TypeError.
    }
    unreachable!();
  }

  pub fn to_primitive(context: impl Context, object: Repr, preferred_type: Option<PreferredType>) -> Repr {
    if object.is_boxed() {
      let hint = if preferred_type.is_some() {
        preferred_type.unwrap()
      } else {
        PreferredType::Default
      };
      if object.is_object_type() {
        let exotic_to_prim = JsObject::get_method(
          context,
          object,
          context
            .symbol_registry()
            .get_wellknown_symbol(WellKnownSymbolType::ToPrimitive)
            .into(),
        );
        if !exotic_to_prim.is_js_undefined() {
          let result = JsObject::call(
            context,
            exotic_to_prim,
            fixed_array!(type: Repr, context: context, capacity: 1, Repr::from(hint as u64)),
          );
          if !result.is_object_type() {
            return result;
          }
          unreachable!();
          // TODO: Throw TypeError.
        }
      }
    }
    return object;
  }

  pub fn to_string(context: impl Context, object: Repr) -> Repr {
    if object.is_js_undefined() {
      return context.globals().undefined_str().into();
    }
    if object.is_js_null() {
      return context.globals().null_str().into();
    }
    if object.is_js_true() {
      return context.globals().true_str().into();
    }
    if object.is_js_false() {
      return context.globals().false_str().into();
    }
    if !object.is_boxed() {
      unreachable!();
      // TODO: implements Number::ToString;
    }
    match object.into_cell_unchecked().shape().tag() {
      ShapeTag::String => return object,
      ShapeTag::Symbol => {
        unreachable!() /*TODO: throw TypeError*/
      }
      ShapeTag::Object => {
        let prim = JsObject::to_primitive(context, object, Some(PreferredType::String));
        return JsObject::to_string(context, prim);
      }
      _ => {
        unreachable!(); /*TODO: implements BitInt*/
      }
    }
    unreachable!();
  }

  pub fn to_property_key(context: impl Context, object: Repr) -> PropertyName {
    let key = JsObject::to_primitive(context, object, Some(PreferredType::String));
    if key.is_symbol() {
      return PropertyName::new(key);
    }
    return PropertyName::new(JsObject::to_string(context, key));
  }

  pub fn has_own_property(_context: impl Context, o: Repr, hint: OwnPropertySearchHint) -> bool {
    return JsObject::get_own_property(o, hint).is_some();
  }
}
