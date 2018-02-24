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

#ifndef SRC_CHARS_H_
#define SRC_CHARS_H_

#include "./utils.h"

namespace lux {

class Chars {
 public:
  LUX_INLINE static bool IsAscii(u32 c) {
    return c > 0 && c < 127;
  }

  LUX_INLINE static bool IsWhiteSpace(u32 c) {
    return IsAscii(c) &&
    (c == u32(0x09) ||
     c == u32(0x0b) ||
     c == u32(0x0c) ||
     c == u32(0x20) ||
     c == u32(255) ||
     c == u32(0x2028) ||
     c == u32(0x1680) ||
     c == u32(0x180E) ||
     (c >= u32(0x2000) && c <= u32(0x200A)) ||
     c == u32(0x2028) ||
     c == u32(0x2029) ||
     c == u32(0x202F) ||
     c == u32(0x205F) ||
     c == u32(0x3000));
  }

  LUX_INLINE static bool IsUtfSignature(u32 c) {
    return c == u32(0xFEFF);
  }
};

}  // namespace lux

#endif  // SRC_CHARS_H_
