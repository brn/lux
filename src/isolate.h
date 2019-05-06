/**
 * The MIT License (MIT)
 * Copyright (c) Taketoshi Aono
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 * @fileoverview
 * @author Taketoshi Aono
 */

#ifndef SRC_ISOLATE_H_
#define SRC_ISOLATE_H_

#include <atomic>
#include "./objects/instances.h"
#include "./utils.h"

namespace lux {
class Heap;
class RootMaps;
class Shape;
class JSSpecials;
class JSNumber;
class FixedArray;

class Isolate {
 public:
  static Isolate* GetPerThreadInstance();

  Heap* heap() const { return heap_; }

  inline FixedArray* empty_fixed_array() { return empty_fixed_array_; }

#define ISOLATE_SHAPE_GETTER(NAME, Name, p) Shape* p() const;
  OBJECT_TYPES(ISOLATE_SHAPE_GETTER)
#undef ISOLATE_SHAPE_GETTER

  LUX_CONST_GETTER(JSNumber*, jsval_nan, jsval_nan_)
  LUX_CONST_GETTER(JSSpecials*, jsval_null, jsval_null_)
  LUX_CONST_GETTER(JSSpecials*, jsval_undefined, jsval_undefined_)
  LUX_CONST_GETTER(JSSpecials*, jsval_true, jsval_true_)
  LUX_CONST_GETTER(JSSpecials*, jsval_false, jsval_false_)

 private:
  void InitOnce();

  Heap* heap_;
  RootMaps* root_maps_;
  std::atomic_flag once_flag_;
  JSNumber* jsval_nan_;
  JSSpecials* jsval_null_;
  JSSpecials* jsval_undefined_;
  JSSpecials* jsval_true_;
  JSSpecials* jsval_false_;
  FixedArray* empty_fixed_array_;
};
}  // namespace lux

#endif  // SRC_ISOLATE_H_
