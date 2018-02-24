/**
 * The MIT License (MIT)
 * Copyright (c) Taketoshi Aono
 *  
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *  
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *  
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * @fileoverview 
 * @author Taketoshi Aono
 */

#include "./alloc.h"
#include "./heap.h"
#include "./isolate.h"
#include "./objects/shape.h"
#include "./objects/heap_object.h"
#include "./objects/jsobject.h"

namespace lux {
Isolate* Isolate::GetPerThreadInstance() {
  static thread_local Isolate isolate;
  isolate.InitOnce();
  return &isolate;
}

void Isolate::InitOnce() {
  if (!once_flag_.test_and_set()) {
    heap_ = Alloc<Heap>();
    root_maps_ = Alloc<RootMaps>(this);
    jsval_nan_ = JSNumber::NewWithoutHandle(this, Double::kNaN);
    jsval_true_ = JSSpecials::NewWithoutHandle(this, JSSpecials::kTrue);
    jsval_false_ = JSSpecials::NewWithoutHandle(this, JSSpecials::kFalse);
    jsval_undefined_
      = JSSpecials::NewWithoutHandle(this, JSSpecials::kUndefined);
    jsval_null_ = JSSpecials::NewWithoutHandle(this, JSSpecials::kNull);
  }
}

#define ISOLATE_SHAPE_GETTER(NAME, Name, p) \
  Shape* Isolate::p() const {               \
    return root_maps_->p();                 \
  }
  OBJECT_TYPES(ISOLATE_SHAPE_GETTER)
#undef ISOLATE_SHAPE_GETTER
}  // namespace lux
