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

#ifndef SRC_HEAP_H_
#define SRC_HEAP_H_

#include <vector>
#include "./utils.h"

namespace lux {
class HandleScope;
template <typename T>
class Handle;

class HeapObject {
};
class HandleScopeList {
 public:
  void Push(HandleScope* scope) {
    scope_list_.push_back(scope);
  }
  void Pop() {
    scope_list_.pop_back();
  }
  LUX_INLINE HandleScope* last() { return scope_list_.back(); }
 private:
  std::vector<HandleScope*> scope_list_;
};

class HandleScope: private StackObject {
 public:
  HandleScope() {
    current_scope_list()->Push(this);
  }
  ~HandleScope() {
    current_scope_list()->Pop();
  }
  LUX_INLINE static void register_handle(Handle<HeapObject> handle);
 private:
  LUX_INLINE static HandleScope* current_handle_scope() {
    return current_scope_list()->last();
  }
  LUX_INLINE static HandleScopeList* current_scope_list() {
    static thread_local HandleScopeList scope_list;
    return &scope_list;
  }

  static std::vector<Handle<HeapObject>> handle_list_;
};

template <typename T>
class Handle {
 public:
  explicit Handle(T** ptr)
      : ptr_(ptr) {
    HandleScope::register_handle(*this);
  }

  T& operator->() {
    return **ptr_;
  }
 private:
  T** ptr_;
};

class Heap {
 public:
  explicit Heap(size_t default_size_ = kDefaultSize);
  Address Allocate(size_t size);
  static Heap* GetHeap();

  static const size_t kDefaultSize = 1 MB;

 private:
  void Grow();

  size_t size_;
  size_t used_;
  Address arena_;
  static Heap* heap_;
};
}  // namespace lux

#endif  // SRC_HEAP_H_
