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

#ifndef SRC_OBJECTS_HEAP_OBJECT_H_
#define SRC_OBJECTS_HEAP_OBJECT_H_

#include "../utils.h"
#include "./instances.h"
#include "./object.h"

namespace lux {
class Isolate;
class Shape;

class RootMaps {
 public:
  explicit RootMaps(Isolate* isolate);

#define ROOT_MAP_PROPS_DEF(A, N, p) LUX_CONST_GETTER(Shape*, p, p##_)
  OBJECT_TYPES(ROOT_MAP_PROPS_DEF)
#undef ROOT_MAP_PROPS_DEF

 private:
#define ROOT_MAP_PROPS_DEF(A, N, p) Shape* p##_;
  OBJECT_TYPES(ROOT_MAP_PROPS_DEF)
#undef ROOT_MAP_PROPS_DEF
};

//                       00000000 00000000
// SmiTag(smi = 0)                       |
// Shape.instance_type          |||
class HeapObject : public Object {
 public:
  enum {
    kSize = LUX_ALIGN_OFFSET(kPointerSize + kHeapObjectTag, kPointerSize),
    kHeapObjectOffset = kPointerSize,
  };

  LUX_INLINE Shape* shape() const {
    return *(reinterpret_cast<Shape**>(FIELD_ADDR(this, 0)));
  }

  OBJECT_CAST(HeapObject*, Object*)
  OBJECT_CAST(const HeapObject*, const Object*)

 protected:
  static HeapObject* New(Isolate* isolate, Shape* shape, size_t size = 0);
  static HeapObject* NewWithoutShape(Isolate* isolate, size_t size);
};

}  // namespace lux

#endif  // SRC_OBJECTS_HEAP_OBJECT_H_
