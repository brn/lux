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

#ifndef _I6_SRC_UTILS_H_
#define _I6_SRC_UTILS_H_

#include <stdint.h>

#include <memory>

namespace i6 {

#define KB * 1024
#define MB KB * 1024

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;

#ifdef DEBUG
void Invalidate(bool cond, const char* message) {
  if (!cond) {
    printf("%s is not valid\n", message);
    debug::StackTrace st;
    st.Print();
    exit(1);
  }
}
#define INVALIDATE(cond) Invalidate(cond, #cond)
#else
#define INVALIDATE(cond)
#endif

template <typename T>
using Shared = std::shared_ptr<T>;

typedef char byte;

typedef char* Address;

template <int LowerBits, typename Type = uint32_t>
class Bitmask {
 public:
  static const Type full = ~(Type(0));
  static const Type upper = ~((Type(1) << LowerBits) - 1);
  static const Type lower = (Type(1) << LowerBits) - 1;
};
}  // namespace i6

#endif  // _I6_SRC_UTILS_H_
