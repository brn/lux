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

#include <array>
#include "./shape.h"
#include "../heap.h"
#include "../isolate.h"
#include "./jsobject.h"

namespace lux {

static std::array<size_t, kInstanceTypeCount + 1> kInstanceSizeMap = {{
    0,
#define SIZE_DECL(A, Name, p) Name::kSize,
    OBJECT_TYPES(SIZE_DECL)
#undef SIZE_DECL
  }};

Shape* Shape::New(Isolate* isolate, InstanceType type) {
  auto heap_obj = reinterpret_cast<Address>(
      HeapObject::NewWithoutShape(isolate, kSize));
  auto instance_type = FIELD_ADDR(heap_obj, kInstanceTypeOffset);
  *instance_type = static_cast<uint8_t>(type);
  auto instance_size = FIELD_ADDR(heap_obj, kInstanceSizeOffset);
  *instance_size = kInstanceSizeMap[static_cast<uint8_t>(type)];
  return reinterpret_cast<Shape*>(heap_obj);
}
}  // namespace lux
