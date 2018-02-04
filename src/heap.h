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

#include <type_traits>
#include <vector>
#include <unordered_set>
#include "./utils.h"
#include "./objects/shape.h"
#include "./objects/heap_object.h"

namespace lux {
template <typename T>
class Handle {
  template <typename U>
  friend class Handle;

  template <typename U>
  friend Handle<U> make_handle(U* obj);

 public:
  static_assert(std::is_convertible<T, HeapObject>::value,
                "Handle accept only HeapObject.");

  template <typename U>
  static Handle<T> New(U* ptr);

  template <typename U>
  Handle(const Handle<U>& h)
      : ptr_(h.ptr_) {}


  template <typename U>
  Handle(Handle<U>&& h)
      : ptr_(h.ptr_) {}


  template <typename U>
  Handle<U>& operator=(const Handle<U>& h) const {
    ptr_ = h.ptr;
    return *this;
  }

  template <typename U>
  bool operator==(const Handle<U>& h) const {
    return this->operator*() == h.operator*();
  }

  T* operator*() const {
    if (ptr_->shape()->IsForwardingPointer()) {
      return reinterpret_cast<T*>(
          ptr_->shape()->ForwardingPointer());
    }
    return ptr_;
  }

  T* operator->() const {
    return this->operator*();
  }

  template <typename U>
  operator Handle<U>() {
    return Handle<U>(ptr_);
  }

  template <typename U>
  static Handle<T> Cast(const Handle<U>& obj) {
    return *reinterpret_cast<Handle<T>*>(
        reinterpret_cast<uintptr_t>(&obj));
  }

 private:
  template <typename U>
  explicit Handle(U* ptr)
      : ptr_(ptr) {}

  T* ptr_;
};

template <typename T>
LUX_INLINE Handle<T> make_handle(T* obj) {
  return Handle<T>::New(obj);
}

class HandleScope;
class HandleScopeList {
 public:
  using ScopeList = std::vector<HandleScope*>;
  using iterator = ScopeList::iterator;
  using reverse_iterator = ScopeList::reverse_iterator;
  void Push(HandleScope* scope) {
    scope_list_.push_back(scope);
  }

  void Pop() {
    scope_list_.pop_back();
  }

  reverse_iterator Prev() {
    auto rb = scope_list_.rbegin();
    auto prev = rb + 1;
    return prev;
  }

  iterator end() {
    return scope_list_.end();
  }

  reverse_iterator rend() {
    return scope_list_.rend();
  }

  size_t size() const {
    return scope_list_.size();
  }
  LUX_INLINE HandleScope* last() { return scope_list_.back(); }

 private:
  ScopeList scope_list_;
};

class HandleScope: private StackObject {
  template <typename T>
  friend class Handle;
 public:
  HandleScope() {
    current_scope_list()->Push(this);
  }
  ~HandleScope() {
    current_scope_list()->Pop();
  }

  template <typename T>
  Handle<T> Return(Handle<T> h);

 private:
  typedef std::unordered_set<Handle<HeapObject>> HandleSet;
  LUX_INLINE static void register_handle(
      const Handle<HeapObject>& handle);

  LUX_INLINE void append_handle(
      const Handle<HeapObject>& handle) {
    HandleScope::register_handle(handle);
  }

  LUX_INLINE static HandleScope* current_handle_scope() {
    return current_scope_list()->last();
  }
  LUX_INLINE static HandleScopeList* current_scope_list() {
    static thread_local HandleScopeList scope_list;
    return &scope_list;
  }

  static HandleSet handle_set_;
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

namespace std {
template<typename T>
class hash<lux::Handle<T>> {
 public:
  uintptr_t operator ()(const lux::Handle<lux::HeapObject>& h) const {
    return reinterpret_cast<uintptr_t>(*h);
  }
};
}  // namespace std

#include "./heap-inl.h"

#endif  // SRC_HEAP_H_
