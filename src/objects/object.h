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

#ifndef SRC_OBJECTS_OBJECT_H_
#define SRC_OBJECTS_OBJECT_H_

#include <string>
#include <stdint.h>
#include "../utils.h"

namespace lux {
#define FIELD_ROOT_ADDR(ptr, offset)              \
  reinterpret_cast<Address>(                      \
      reinterpret_cast<uintptr_t>(ptr)) + offset

#define FIELD_ADDR(ptr, offset)                                         \
  (reinterpret_cast<Address>(                                           \
      reinterpret_cast<uintptr_t>(ptr)) - Object::kHeapObjectTag) + offset

#define FIELD_PROPERTY(Type, ptr, offset)       \
  reinterpret_cast<Type>(FIELD_ADDR(ptr, offset))

#define OBJECT_CAST(Type, Arg)                  \
  static inline Type Cast(Arg o) {              \
    return reinterpret_cast<Type>(o);           \
  }

class Object {
 public:
  static const int kHeapObjectTag = 1;
  static const int kHeapObjectTagSize = 2;
  static const intptr_t kHeapObjectTagMask = (1 << kHeapObjectTagSize) - 1;

  inline static bool IsSmi(void* obj) {
    return ((reinterpret_cast<intptr_t>(obj) & kHeapObjectTagMask) !=
            kHeapObjectTag);
  }

  inline static bool IsHeapObject(void* obj) {
    return ((reinterpret_cast<intptr_t>(obj) & kHeapObjectTagMask) ==
            kHeapObjectTag);
  }

  inline bool IsSmi() const {
    return IsSmi(reinterpret_cast<Address>(
        reinterpret_cast<uintptr_t>(this)));
  }

  inline bool IsHeapObject() const {
    return IsHeapObject(FIELD_ROOT_ADDR(this, 0));
  }

  template <typename T>
  inline static Object* Cast(T v) {
    return reinterpret_cast<Object*>(v);
  }

  std::string ToString();

  bool Equals(Object* o) const;

  bool GreaterThan(Object* o) const;
};

class Smi: public Object {
 public:
  static const smi_t kMaxValue = smi_t(~0) >> 1;

  LUX_INLINE static bool IsFit(smi_t value) {
    return value <= kMaxValue;
  }

  LUX_INLINE static Smi* FromInt(smi_t value) {
    INVALIDATE(IsFit(value));
    return reinterpret_cast<Smi*>(value << 1);
  }

  smi_t raw_value() const {
    return reinterpret_cast<smi_t>(this);
  }

  smi_t value() const {
    return reinterpret_cast<smi_t>(this) >> 1;
  }

  bool Equals(Smi* smi) {
    return smi->raw_value() == raw_value();
  }

  OBJECT_CAST(Smi*, Object*)
  OBJECT_CAST(const Smi*, const Object*)
};
}  // namespace lux

#endif  // SRC_OBJECTS_OBJECT_H_
