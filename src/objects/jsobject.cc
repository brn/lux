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

namespace lux {
const size_t JSObject::kSize = HeapObject::kSize + kPointerSize;
const size_t JSArray::kSize = HeapObject::kSize + kPointerSize;
const size_t JSString::kSize = HeapObject::kSize + kPointerSize;

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

Handle<JSString> JSString::New(Isolate* isolate, const char* data) {
  auto utf16_string = Unicode::ConvertUtf8StringToUtf16String(data);
  auto required_size = kStringLengthSize
    + sizeof(u32) * (utf16_string.size() + 1);
  auto aligned_size = LUX_ALIGN_OFFSET(required_size, kAlignment);
  auto str = HeapObject::New(isolate, isolate->string_map(),
                             aligned_size);
  auto addr = reinterpret_cast<Address>(str);
  auto size_field = reinterpret_cast<uint32_t*>(addr + kStringLengthOffset);
  *size_field = utf16_string.size();
  auto u16cp = reinterpret_cast<Utf16CodePoint*>(
      addr + kStringPtrOffset);
  int i = 0;
  for (auto &code : utf16_string) {
    u16cp[i++] = code;
  }
  return Handle<JSString>::New(reinterpret_cast<JSString*>(str));
}

JSString::Utf8String::Utf8String(JSString* str)
    : length_(str->length()) {
  auto cp = str->data();
  std::stringstream st;
  for (int i = 0; i < length_; i++) {
    st << cp[i].ToUtf8String();
  }
  buffer_ = st.str();
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
}  // namespace lux
