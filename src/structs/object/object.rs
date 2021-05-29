use super::super::cell::*;
use super::super::internal_array::InternalArray;
use super::super::repr::Repr;
use super::super::shadow_class::{ShadowClass, ShadowInstance};
use super::super::shape::{Shape, ShapeTag};
use super::super::string::JsString;
use super::property::{OwnPropertyDescriptorSearchResult, Property, PropertyName, PropertySearchHint};
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
  const SIZE: usize = size_of::<JsObjectLayout>();
  pub fn new(context: impl Context) -> JsObject {
    let layout = HeapLayout::<JsObjectLayout>::new_object(context, JsObject::SIZE, Shape::object());
    return JsObject(layout);
  }

  pub fn get_prototype_of(object: impl ShadowInstance) -> Option<JsObject> {
    return object.class().prototype();
  }

  pub fn set_prototype_of(object: impl ShadowInstance, prototype: JsObject) -> bool {
    return object.class().set_prototype(prototype);
  }

  pub fn is_extensible(object: impl ShadowInstance) -> bool {
    return object.class().is_extensible();
  }

  pub fn prevent_extensions(object: impl ShadowInstance) -> bool {
    return object.class().prevent_extensions();
  }

  pub fn get_own_property(object: Repr, hint: PropertySearchHint) -> Option<OwnPropertyDescriptorSearchResult> {
    debug_assert!(object.is_object());
    return object.class().get_own_property(hint);
  }

  pub fn define_own_property(
    context: impl Context,
    object: impl ShadowInstance,
    property: Property,
  ) -> OwnPropertyDescriptorSearchResult {
    return object.class().define_own_property(context, property);
  }

  pub fn has_property(object: impl ShadowInstance, hint: PropertySearchHint) -> bool {
    return object.class().has_property(hint).is_some();
  }

  pub fn own_property_keys(context: impl Context, object: impl ShadowInstance) -> InternalArray<PropertyName> {
    return object.class().own_property_keys(context);
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
      return context.undefined_str().into();
    }
    if object.is_js_null() {
      return context.null_str().into();
    }
    if object.is_js_true() {
      return context.true_str().into();
    }
    if object.is_js_false() {
      return context.false_str().into();
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

  pub fn has_own_property(_context: impl Context, o: Repr, hint: PropertySearchHint) -> bool {
    return JsObject::get_own_property(o, hint).is_some();
  }
}

#[derive(Copy, Clone, Default)]
pub struct JsObjectPrototypeLayout {
  shadow_class: ShadowClass,
}

#[derive(Copy, Clone)]
pub struct JsObjectPrototype(HeapLayout<JsObjectPrototypeLayout>);
impl_object!(JsObjectPrototype, HeapLayout<JsObjectPrototypeLayout>);

impl JsObjectPrototype {
  pub fn has_own_property(&self, context: impl Context, receiver: Repr, v: Repr) {
    let p = JsObject::to_property_key(context, v);
    let o = JsObject::to_object(context, receiver);
  }
}
