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

#ifndef SRC_ALLOC_H_
#define SRC_ALLOC_H_

#include <memory>
#include "./utils.h"

#ifdef DEBUG
#include "./debug/stacktrace.h"
#define PRINT(name, p)                          \
  USE(p);                                       \
  //  printf(#name" %p\n", p);                  \
  //  debug::StackTrace st; st.Print();
#else
#undef PRINT
#endif

namespace lux {
template <typename T, typename... Args>
LUX_INLINE T* Alloc(Args... args) {
  auto v = new T(args...);
  PRINT('alloc', v)
  return v;
}

template <typename T>
LUX_INLINE T* AllocArray(size_t size) {
  auto v = new T[size];
  PRINT("alloc-array", v);
  return v;
}

template <typename T, typename... Args>
LUX_INLINE Shared<T> AllocShared(Args... args) {
  auto v = std::make_shared<T>(args...);
  auto p = v.get();
  PRINT("shared", p);
  return v;
}

template <typename T>
LUX_INLINE Shared<T> AllocSharedArray(size_t size) {
  auto v = std::shared_ptr<T>(AllocArray<T>(size),
                              [](T* p) {
                                delete[] p;
                              });
  auto p = v.get();
  PRINT("shared-array", p);
  return v;
}
}  // namespace lux

#endif  // SRC_ALLOC_H_
