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

#include "./instances.h"
#include "./object.h"

namespace lux {
class Isolate;
class Shape;

class RootMaps {
 public:
  explicit RootMaps(Isolate* isolate);

  LUX_CONST_GETTER(Shape*, map_object, map_object_)
  LUX_CONST_GETTER(Shape*, string_map, string_map_)
  LUX_CONST_GETTER(Shape*, object_map, object_map_)
  LUX_CONST_GETTER(Shape*, fixed_array_shape, fixed_array_shape_)

 private:
  Shape* map_object_;
  Shape* string_map_;
  Shape* object_map_;
  Shape* fixed_array_shape_;
};

//                       00000000 00000000
// SmiTag(smi = 0)                       |
// Shape.instance_type          |||
class HeapObject: public Object {
 public:
  enum {
    kSize = kPointerSize + kHeapObjectTag,
    kHeapObjectOffset = kPointerSize + kHeapObjectTag,
  };

  LUX_INLINE Shape* shape() const {
    return *(reinterpret_cast<Shape**>(FIELD_ADDR(this, kHeapObjectTag)));
  }

 protected:
  static HeapObject* New(Isolate* isolate, Shape* shape, size_t size = 0);
  static HeapObject* NewWithoutShape(Isolate* isolate, size_t size);
};

}  // namespace lux

#endif  // SRC_OBJECTS_HEAP_OBJECT_H_
