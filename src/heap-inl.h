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

#ifndef SRC_HEAP_INL_H_
#define SRC_HEAP_INL_H_

#include <type_traits>

namespace lux {
template <typename T>
template <typename U>
Handle<T> Handle<T>::New(U* ptr)  {
  static_assert(std::is_convertible<T, HeapObject>::value,
                "Handle accept only HeapObject.");
  Handle<T> h(ptr);
  HandleScope::register_handle(h);
  return h;
}

void HandleScope::register_handle(const Handle<HeapObject>& handle) {
  PRECONDITION(current_scope_list()->size() > 0,
             "Before Allocate Object, you must create HandleScope.");
  handle_set_.insert(handle);
}

template <typename T>
Handle<T> HandleScope::Return(Handle<T> h) {
  auto it = handle_set_.find(h);
  PRECONDITION_ASSERT(it != handle_set_.end());
  auto handle = *it;
  auto list = current_scope_list();
  auto p = list->Prev();
  PRECONDITION(p != list->rend(), "No more scope exists.\n"
               "Maybe you need to create parent HandleScope "
               "before call Handle::Return.");
  (*p)->append_handle(handle);
  return Handle<T>::Cast(handle);
}
}  // namespace lux

#endif  // SRC_HEAP_INL_H_
