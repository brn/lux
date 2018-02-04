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

#ifndef SRC_OBJECTS_INSTANCES_H_
#define SRC_OBJECTS_INSTANCES_H_

#include <stdint.h>

namespace lux {
#define OBJECT_TYPES(A)                                                 \
  A(SHAPE, Shape, meta_shape)                                           \
  A(JS_FUNCTION, JSFunction, function_shape)                            \
  A(JS_STRING, JSString, string_shape)                                  \
  A(JS_OBJECT, JSObject, object_shape)                                  \
  A(JS_ARRAY, JSArray, array_shape)                                     \
  A(JS_SPECIALS, JSSpecials, specials_shape)                            \
  A(BYTECODE_EXECUTABLE, BytecodeExecutable, bytecode_executable_shape) \
  A(FIXED_ARRAY, FixedArray, fixed_array_shape)

enum class InstanceType: uint8_t {
  INVALID,
#define INSTANCE_TYPE_DEF(TYPE, Name, s) TYPE,
  OBJECT_TYPES(INSTANCE_TYPE_DEF)
#undef INSTANCE_TYPE_DEF
};

enum InstanceTypeCount__ {
  PADDING_ = -1,
#define INSTANCE_TYPE_DEF(TYPE, Name, s) TYPE,
  OBJECT_TYPES(INSTANCE_TYPE_DEF)
#undef INSTANCE_TYPE_DEF
  LAST
};

static const uint8_t kInstanceTypeCount = InstanceTypeCount__::LAST;
}  // namespace lux

#endif  // SRC_OBJECTS_INSTANCES_H_
