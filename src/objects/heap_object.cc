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

#include "../heap.h"
#include "./heap_object.h"
#include "../isolate.h"
#include "./shape.h"

namespace lux {
RootMaps::RootMaps(Isolate* isolate) {
  map_object_ = Shape::New(isolate, InstanceType::SHAPE);
  string_map_ = Shape::New(isolate, InstanceType::JS_STRING);
  object_map_ = Shape::New(isolate, InstanceType::JS_OBJECT);
}

const size_t HeapObject::kSize = kPointerSize + kHeapObjectTag;
const size_t HeapObject::kHeapObjectOffset = Shape::kShapeOffset;;

HeapObject* HeapObject::New(Isolate* isolate, Shape* shape, size_t size) {
  auto addr = isolate->heap()->Allocate(
      HeapObject::kSize + (size > 0? size: shape->size()));
  auto heap_object = reinterpret_cast<HeapObject*>(addr + kHeapObjectTag);
  auto shape_p = reinterpret_cast<Shape**>(heap_object);
  *shape_p = shape;
  return heap_object;
}

HeapObject* HeapObject::NewWithoutShape(Isolate* isolate, size_t size) {
  auto addr = isolate->heap()->Allocate(
      kPointerSize + size + kHeapObjectTag);
  return reinterpret_cast<HeapObject*>(addr + kHeapObjectTag);
}
}  // namespace lux
