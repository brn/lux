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

#ifndef SRC_OBJECTS_SHAPE_H_
#define SRC_OBJECTS_SHAPE_H_

#include "./heap_object.h"
#include "./instances.h"
#include "../utils.h"

namespace lux {
class Isolate;

// Shape
// +-------------------------+
// |  1byte | instance type  |
// --------------------------+
// |  1byte | instance size  |
// +-------------------------+
// |  padding(7/3)           |
// +-------------------------+
// |  Forwarding Pointer     |
// +-------------------------+
// |  padding(7/3)           |
// +-------------------------+
#pragma pack(push)
class Shape: public HeapObject {
 public:
  enum {
    kOffset = HeapObject::kHeapObjectOffset,
    kPaddingSize = kPointerSize - kOneByteSize,
    kHeaderSize = (kOneByteSize << 1),
    kSize = kHeaderSize + (kPaddingSize * 2) + kPointerSize,
    kInstanceTypeOffset = kOffset,
    kInstanceSizeOffset = kInstanceTypeOffset + kOneByteSize,
    kForwardingPointerOffset =
    kInstanceSizeOffset + kOneByteSize + kPaddingSize,
    kShapeOffset = kForwardingPointerOffset + kPointerSize + kPaddingSize,
  };

  static Shape* New(Isolate* isolate, InstanceType type);

  InstanceType instance_type() const {
    return static_cast<InstanceType>(
        (*reinterpret_cast<uint8_t*>(
            FIELD_ADDR(this, kInstanceTypeOffset))));
  }

  uint8_t size() const {
    return *(FIELD_ADDR(this, kInstanceSizeOffset));
  }

  LUX_INLINE bool IsForwardingPointer() const {
    return IsSmi();
  }

  LUX_INLINE HeapObject* ForwardingPointer() const {
    return reinterpret_cast<HeapObject*>(
        FIELD_ADDR(this, kForwardingPointerOffset));
  }
};
#pragma pack(pop)
}  // namespace lux

#endif  // SRC_OBJECTS_SHAPE_H_
