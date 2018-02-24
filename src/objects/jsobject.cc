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

#include <sstream>
#include "./jsobject.h"
#include "./shape.h"
#include "./string.h"
#include "../isolate.h"
#include "../unicode.h"
#include "../bytecode.h"
#include "../vm.h"

namespace lux {
const size_t JSObject::kSize = HeapObject::kSize + kPointerSize;
const size_t JSString::kSize = HeapObject::kSize + kPointerSize;
const size_t JSSpecials::kSize = HeapObject::kSize + sizeof(uint8_t);
const size_t BytecodeExecutable::kSize = HeapObject::kSize + (kPointerSize * 2);


Handle<BytecodeExecutable> BytecodeExecutable::New(
    Isolate* isolate,
    BytecodeArray* bytecode_array,
    BytecodeConstantArray* constant_pool) {
  auto heap_obj = HeapObject::New(isolate,
                                  isolate->bytecode_executable_shape());
  auto array = reinterpret_cast<BytecodeArray**>(
      FIELD_ADDR(heap_obj, kBytecodeArrayOffset));
  *array = bytecode_array;
  auto pool = reinterpret_cast<BytecodeConstantArray**>(
      FIELD_ADDR(heap_obj, kBytecodeConstantPoolOffset));
  *pool = constant_pool;
  return make_handle(reinterpret_cast<BytecodeExecutable*>(heap_obj));
}

#ifdef DEBUG
std::string BytecodeExecutable::ToString() {
  return bytecode_array()->ToString(constant_pool());
}
#endif

FixedArrayBase* FixedArrayBase::NewArray(Isolate* isolate, uint32_t size) {
  auto required_size = kFixedArrayLengthSize + size;
  auto aligned_size = LUX_ALIGN_OFFSET(required_size, kAlignment);
  auto arr = HeapObject::New(isolate, isolate->fixed_array_shape(),
                             aligned_size);
  auto length = reinterpret_cast<uint32_t*>(
      FIELD_ADDR(arr, kFixedArrayLengthOffset));
  *length = size;
  return reinterpret_cast<FixedArrayBase*>(arr);
}

Object* JSObject::GetLength(Isolate* isolate, Object* o) {
  if (o->IsSmi()) {
    std::stringstream st;
    st << Smi::Cast(o)->value();
    return Smi::FromInt(st.str().size());
  }

  switch (HeapObject::Cast(o)->shape()->instance_type()) {
    case InstanceType::JS_FUNCTION:
      return Smi::FromInt(JSFunction::Cast(o)->length());
    case InstanceType::JS_ARRAY:
      return Smi::FromInt(JSArray::Cast(o)->length());
    case InstanceType::JS_STRING:
      return Smi::FromInt(JSString::Cast(o)->length());
    default:
      return isolate->jsval_undefined();
  }
}

Handle<JSFunction> JSFunction::New(Isolate* isolate,
                                   JSString* name,
                                   uint8_t length,
                                   BytecodeExecutable* be) {
  auto heap_obj = HeapObject::New(isolate,
                                  isolate->function_shape());
  auto length_ptr = FIELD_PROPERTY(uint8_t*, heap_obj, kLengthOffset);
  *length_ptr = length;
  auto name_ptr = FIELD_PROPERTY(JSString**, heap_obj, kNameOffset);
  *name_ptr = name;
  auto be_ptr = FIELD_PROPERTY(BytecodeExecutable**, heap_obj, kCodeOffset);
  *be_ptr = be;
  return make_handle(reinterpret_cast<JSFunction*>(heap_obj));
}

Object* JSFunction::Call(
    Isolate* isolate, std::initializer_list<Object*> parameters) {
  lux::VirtualMachine vm(isolate);
  return vm.Execute(code(), parameters);
}

Handle<JSRegExp> JSRegExp::New(Isolate* isolate,
                               BytecodeExecutable* executable,
                               uint8_t flag) {
  return make_handle(NewWithoutHandle(isolate, executable, flag));
}

JSRegExp* JSRegExp::NewWithoutHandle(Isolate* isolate,
                                     BytecodeExecutable* be,
                                     uint8_t flag) {
  auto heap_obj = HeapObject::New(isolate,
                                  isolate->regexp_shape());
  auto flag_ptr = FIELD_PROPERTY(uint8_t*, heap_obj, kFlagOffset);
  *flag_ptr = flag;
  auto be_ptr = FIELD_PROPERTY(BytecodeExecutable**, heap_obj, kCodeOffset);
  *be_ptr = be;
  return reinterpret_cast<JSRegExp*>(heap_obj);
}

JSSpecials* JSRegExp::Test(Isolate* isolate, JSString* input) {
  lux::VirtualMachine vm(isolate);
  return reinterpret_cast<JSSpecials*>(vm.ExecuteRegex(
      code(), input, flag(), false));
}

JSArray* JSRegExp::Match(Isolate* isolate, JSString* input) {
  lux::VirtualMachine vm(isolate);
  return JSArray::Cast(vm.ExecuteRegex(code(), input, flag(), true));
}

Handle<JSArray> JSArray::NewEmptyArray(Isolate* isolate,
                                       uint32_t length,
                                       size_t initial_capacity) {
  auto fixed_array = FixedArray::New(isolate, initial_capacity);
  for (int i = 0; i < length; i++) {
    fixed_array->write(i, isolate->jsval_undefined());
  }
  return NewWithElement(isolate, length, *fixed_array);
}

Handle<JSArray> JSArray::NewWithElement(Isolate* isolate,
                                        uint32_t length,
                                        FixedArray* element) {
  auto heap_obj = HeapObject::New(isolate, isolate->array_shape());
  auto length_ptr = reinterpret_cast<uint32_t*>(
      FIELD_ADDR(heap_obj, kLengthOffset));
  *length_ptr = length;
  auto element_ptr = reinterpret_cast<FixedArray**>(
      FIELD_ADDR(heap_obj, kElementOffset));
  *element_ptr = element;
  return make_handle(reinterpret_cast<JSArray*>(heap_obj));
}

Handle<JSString> JSString::New(Isolate* isolate, const char* data) {
  auto utf16_string = Unicode::ConvertUtf8StringToUtf16String(data);
  return New(isolate, utf16_string.data(), utf16_string.size());
}

Handle<JSString> JSString::New(Isolate* isolate, size_t length) {
  return New(isolate, nullptr, length);
}

Handle<JSString> JSString::New(
    Isolate* isolate, const Utf16CodePoint* cp, size_t length) {
  auto required_size = kStringLengthSize + kPointerSize;
  auto backing_store_size =  sizeof(u32) * (length + 1);
  auto aligned_size = LUX_ALIGN_OFFSET(required_size, kAlignment);
  auto aligned_backing_store_size = LUX_ALIGN_OFFSET(
      backing_store_size, kAlignment);
  auto str = HeapObject::New(isolate, isolate->string_shape(),
                             aligned_size);
  auto store = U32FixedArray::New(isolate, aligned_backing_store_size);
  auto size_field = reinterpret_cast<uint32_t*>(
      FIELD_ADDR(str, kStringLengthOffset));
  *size_field = length;
  auto store_field = reinterpret_cast<U32FixedArray**>(
      FIELD_ADDR(str, kStringPtrOffset));
  if (cp) {
    for (int i = 0; i < length; i++) {
      store->write(i, cp[i]);
    }
  }
  *store_field = *store;
  return make_handle(reinterpret_cast<JSString*>(str));
}

JSString::Utf8String::Utf8String(JSString* str)
    : length_(str->length()) {
  auto cp = str->data();
  std::stringstream st;
  for (int i = 0; i < length_; i++) {
    st << cp->at(i).ToUtf8String();
  }
  buffer_ = st.str();
}

void JSString::Append(Isolate* isolate, u32 v) {
  auto next_size = length() + 1;
  if (next_size >= data()->length()) {
    auto capacity = data()->length();
    auto next = U32FixedArray::NewWithoutHandle(
        isolate, (capacity < 10? 10: capacity) * 2);
    next->Copy(data());
    *FIELD_PROPERTY(U32FixedArray**, this, kStringPtrOffset) = next;
  }
  data()->write(length(), Utf16CodePoint(v));
  *FIELD_PROPERTY(uint32_t*, this, kStringLengthOffset) = next_size;
}

bool JSString::Equals(const JSString* js_string) const {
  if (length() != js_string->length()) {
    return false;
  }

  for (auto i = 0; i < length(); i++) {
    if (at(i) != js_string->at(i)) {
      return false;
    }
  }

  return true;
}

bool JSString::GreaterThan(const JSString* js_string) const {
  auto bigger = length() > js_string->length();
  auto len = bigger? length(): js_string->length();

  for (auto i = 0; i < len; i++) {
    if (at(i) > js_string->at(i)) {
      return true;
    }
  }

  return bigger? true: false;
}

JSNumber* JSNumber::NewWithoutHandle(Isolate* isolate, double value) {
  auto required_size = kValueSize;
  auto aligned_size = LUX_ALIGN_OFFSET(required_size, kAlignment);
  auto n = HeapObject::New(isolate, isolate->number_shape(),
                           aligned_size);
  auto d = FIELD_PROPERTY(double*, n, kValueOffset);
  *d = value;
  return reinterpret_cast<JSNumber*>(n);
}

Handle<JSNumber> JSNumber::New(Isolate* isolate, double value) {
  return make_handle(NewWithoutHandle(isolate, value));
}

const uint8_t JSSpecials::kSpecialsStartOffset;
const uint8_t JSSpecials::kSpecialsFlagSize;

Handle<JSSpecials> JSSpecials::New(Isolate* isolate, JSSpecials::Type type) {
  return make_handle(NewWithoutHandle(isolate, type));
}

JSSpecials* JSSpecials::NewWithoutHandle(
    Isolate* isolate, JSSpecials::Type type) {
  auto heap_obj = HeapObject::New(isolate, isolate->specials_shape());
  auto flag = reinterpret_cast<uint8_t*>(
      FIELD_ADDR(heap_obj, kSpecialsStartOffset));
  *flag = type;
  return reinterpret_cast<JSSpecials*>(heap_obj);
}
}  // namespace lux
