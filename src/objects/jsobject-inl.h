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

#ifndef SRC_OBJECTS_JSOBJECT_INL_H_
#define SRC_OBJECTS_JSOBJECT_INL_H_

namespace lux {
bool JSSpecials::ToBoolean(Object* obj) {
  if (obj->IsSmi()) {
    return Smi::Cast(obj)->value() != 0;
  }

  auto heap_obj = HeapObject::Cast(obj);
  switch (heap_obj->shape()->instance_type()) {
    case InstanceType::JS_STRING:
      return JSString::Cast(heap_obj)->length() > 0;
    case InstanceType::JS_OBJECT:
    case InstanceType::JS_ARRAY:
      return true;
    case InstanceType::JS_SPECIALS:
      return JSSpecials::Cast(heap_obj)->ToBoolean();
    default:
      return false;
  }
}

void JSArray::Push(Isolate* isolate, Object* object) {
  auto len = length();
  if (length() < element()->length()) {
    element()->write(len, object);
  } else {
    auto next = FixedArray::New(isolate, element()->length() * 2);
    next->Copy(element());
    next->write(len, object);
    set_element(*next);
  }
  set_length(len + 1);
}

Object* JSArray::Pop(Isolate* isolate) {
  auto new_length = length() > 0? length() - 1: 0;
  Object* value = isolate->jsval_undefined();
  if (length() != 0) {
    value = element()->at(new_length - 1);
    element()->write(new_length, isolate->jsval_undefined());
  }
  set_length(new_length);
  return value;
}

Object* JSArray::Shift(Isolate* isolate) {
  auto new_length = length() > 0? length() - 1: 0;
  Object* value = isolate->jsval_undefined();
  if (length() != 0) {
    value = element()->at(0);
    auto next = FixedArray::New(isolate, element()->length());
    next->Copy(element(), 1, element()->length());
    set_element(*next);
  }
  set_length(new_length);
  return value;
}

Object* JSArray::Unshift(Isolate* isolate, Object* new_value) {
  Object* value = isolate->jsval_undefined();
  if (length() != 0) {
    value = element()->at(0);
  }
  element()->write(0, new_value);
  set_length(length() == 0? 1: length());
  return value;
}
}  // namespace lux

#endif  // SRC_OBJECTS_JSOBJECT_INL_H_
