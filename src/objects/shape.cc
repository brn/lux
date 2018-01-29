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

static std::array<size_t, kInstanceTypeCount> kInstanceSizeMap = {{
#define SIZE_DECL(A, Name) Name::kSize,
    OBJECT_TYPES(SIZE_DECL)
#undef SIZE_DECL
  }};

Shape* Shape::New(Isolate* isolate, InstanceType type) {
  auto addr = reinterpret_cast<Address>(
      HeapObject::NewWithoutShape(isolate, 0));
  *addr = static_cast<uint8_t>(type);
  auto next = addr + 1;
  *next = kInstanceSizeMap[static_cast<uint8_t>(type)];
  return reinterpret_cast<Shape*>(addr);
}
}  // namespace lux
