// The MIT License (MIT)
//
// Copyright (c) Taketoshi Aono(brn)
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

#include "./object.h"
#include "./heap_object.h"
#include "./instances.h"
#include "./jsobject.h"
#include "./shape.h"

namespace lux {
const int Object::kHeapObjectTag;
const int Object::kHeapObjectTagSize;
const intptr_t Object::kHeapObjectTagMask;
const smi_t Smi::kMaxValue;

bool Object::Equals(Object* o) const {
  if (IsSmi() && o->IsSmi()) {
    return Smi::Cast(o)->value() ==
      Smi::Cast(this)->value();
  }

  auto ho = HeapObject::Cast(o);
  if (IsSmi()) {
    if (ho->shape()->instance_type() == InstanceType::JS_NUMBER) {
      return JSNumber::Cast(ho)->Equals(Smi::Cast(this));
    }
  }

  auto self = HeapObject::Cast(this);

  switch (self->shape()->instance_type()) {
    case InstanceType::JS_STRING: {
      if (ho->shape()->instance_type() ==
          InstanceType::JS_STRING) {
        return JSString::Cast(self)->Equals(
            JSString::Cast(ho));
      }
      return false;
    }
    case InstanceType::JS_NUMBER:
      if (ho->shape()->instance_type() == InstanceType::JS_NUMBER) {
        return JSNumber::Cast(ho)->Equals(JSNumber::Cast(this));
      }
      return false;
    default:
      return o == this;
  }
}

bool Object::GreaterThan(Object* o) const {
  if (IsSmi() && o->IsSmi()) {
    return Smi::Cast(this)->value() > Smi::Cast(o)->value();
  }

  auto ho = HeapObject::Cast(o);
  if (IsSmi()) {
    if (ho->shape()->instance_type() == InstanceType::JS_NUMBER) {
      return JSNumber::Cast(ho)->GreaterThan(Smi::Cast(this));
    }
  }

  auto self = HeapObject::Cast(this);

  switch (self->shape()->instance_type()) {
    case InstanceType::JS_STRING: {
      if (ho->shape()->instance_type() ==
          InstanceType::JS_STRING) {
        return JSString::Cast(self)->GreaterThan(
            JSString::Cast(ho));
      }
      return false;
    }
    case InstanceType::JS_NUMBER:
      if (ho->shape()->instance_type() == InstanceType::JS_NUMBER) {
        return JSNumber::Cast(this)->GreaterThan(JSNumber::Cast(ho));
      }
      return false;
    default:
      return false;
  }
}

std::string Object::ToString() {
  std::stringstream st;
  if (IsSmi()) {
    st << "Smi(" << Smi::Cast(this)->value() << ")";
    return st.str();
  }

  auto ho = HeapObject::Cast(this);
  switch (ho->shape()->instance_type()) {
    case InstanceType::JS_STRING: {
      JSString::Utf8String str(JSString::Cast(ho));
      st << "String(\"" << str.value() << "\")";
      return st.str();
    }
    case InstanceType::JS_ARRAY: {
      st << "JSArray[";
      auto arr = JSArray::Cast(ho);
      auto el = arr->element();
      for (int i = 0; i < arr->length(); i++) {
        if (i > 0) {
          st << ", ";
        }
        auto o = el->at(i);
        st << o->ToString();
      }
      st << "]";
      return st.str();
    }
    case InstanceType::JS_NUMBER: {
      st << "Number(" << JSNumber::Cast(ho)->value() << ")";
      return st.str();
    }
    case InstanceType::JS_SPECIALS: {
      auto jssp = JSSpecials::Cast(ho);
      switch (jssp->type()) {
        case JSSpecials::kFalse:
          st << "false";
          break;
        case JSSpecials::kTrue:
          st << "true";
          break;
        case JSSpecials::kUndefined:
          st << "undefined";
          break;
        case JSSpecials::kNull:
          st << "null";
          break;
        default:
          UNREACHABLE();
      }
      return st.str();
    }
    case InstanceType::FIXED_ARRAY: {
      st << "FixedArray[";
      auto el = FixedArray::Cast(ho);
      for (int i = 0; i < el->length(); i++) {
        if (i > 0) {
          st << ", ";
        }
        auto o = el->at(i);
        st << o->ToString();
      }
      st << "]";
      return st.str();
    }
    default:
      UNREACHABLE();
  }
  return "";
}
}  // namespace lux
