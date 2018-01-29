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

#ifndef SRC_OBJECTS_JSOBJECT_H_
#define SRC_OBJECTS_JSOBJECT_H_

#include <string>
#include <utility>
#include "./object.h"
#include "./heap_object.h"
#include "./shape.h"
#include "../heap.h"
#include "../unicode.h"

namespace lux {
class Isolate;
class Utf16CodePoint;

class JSObject: public HeapObject {
 public:
  static const size_t kSize;
};

class JSArray: public HeapObject {
 public:
  static const size_t kSize;
};

class JSString: public HeapObject {
 public:
  enum {
    kStringStartOffset = Shape::kShapeOffset,
    kStringLengthOffset = kStringStartOffset,
    kStringLengthSize = sizeof(uint32_t),
    kStringPtrOffset = kStringLengthOffset + kStringLengthSize
  };
  static const size_t kSize;

  class Utf8String {
   public:
    explicit Utf8String(JSString* str);
    Utf8String(Utf8String&& u8s)
        : length_(u8s.length_),
          buffer_(std::move(u8s.buffer_)) {}

    Utf8String(const Utf8String& u8s)
        : length_(u8s.length_),
          buffer_(u8s.buffer_) {}

    Utf8String& operator=(Utf8String u8s) noexcept {
      using std::swap;
      swap(length_, u8s.length_);
      buffer_.swap(u8s.buffer_);
      return *this;
    }

    Utf8String& operator=(Utf8String&& u8s) noexcept {
      using std::swap;
      swap(length_, u8s.length_);
      buffer_ = std::move(u8s.buffer_);
      return *this;
    }

    LUX_CONST_GETTER(const char*, buffer, buffer_.c_str());

    LUX_CONST_GETTER(size_t, length, length_)

   private:
    size_t length_;
    std::string buffer_;
  };

  static Handle<JSString> New(Isolate* isolate, const char* data);

  LUX_INLINE uint32_t length() const {
    return *reinterpret_cast<uint32_t*>(
        FIELD_ADDR(this, kStringLengthOffset));
  }

  LUX_INLINE Utf16CodePoint at(int index) const {
    INVALIDATE(index < length());
    return data()[index];
  }

  bool Equals(const JSString* str) const;

  friend class Utf8String;
 private:
  LUX_INLINE Utf16CodePoint* data() const {
    return reinterpret_cast<Utf16CodePoint*>(
        FIELD_ADDR(this, kStringPtrOffset));
  }
};
}  // namespace lux

#endif  // SRC_OBJECTS_JSOBJECT_H_
