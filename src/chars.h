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

#include <vector>
#include "./utils.h"

namespace lux {

class Chars {
 public:
  static const u32 kLF = 0x00A;
  static const u32 kCR = 0x00D;
  static const u32 kLS = 0x2028;
  static const u32 kPS = 0x2029;
  LUX_INLINE static u32 ToLower(u32 u) {
    if (IsAlpha(u) && u < 97) {
      return u + 32;
    }
    return u;
  }

  LUX_INLINE static bool IsAlpha(u32 u) {
    return (u >= 65 && u <= 90) || (u >= 97 && u <= 122);
  }

  LUX_INLINE static bool IsCtrlSuccessor(u32 u) {
    return IsAlpha(u) || u == ']' || u == '[' || u == '^' || u == '_' ||
           u == '?';
  }

  LUX_INLINE static bool IsAscii(u32 c) { return c > 0 && c < 127; }

  // http://www.ecma-international.org/ecma-262/9.0/index.html#sec-runtime-semantics-wordcharacters-abstract-operation
  LUX_INLINE static bool IsWordChar(u32 c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
           (c >= '0' && c <= '9') || c == '_';
  }

  // http://www.ecma-international.org/ecma-262/9.0/index.html#sec-white-space
  LUX_INLINE static bool IsWhiteSpace(u32 c) {
    return c == 0x0009 || c == 0x000B || c == 0x000C || c == 0x00A0 ||
           c == 0xFEFF || c == 0x0020 || c == 0x3000 || c == 0x1680 ||
           (c >= 0x2000 && c <= 0x2006) || (c >= 0x2008 && c <= 0x200A) ||
           c == 0x205F || c == 0x00A0 || c == 0x2007 || c == 0x202F;
  }

  LUX_INLINE static bool IsUtfSignature(u32 c) { return c == 0xFEFF; }

  // http://www.ecma-international.org/ecma-262/9.0/index.html#sec-names-and-keywords
  static bool IsIdentifierStart(u32 value);

  // http://www.ecma-international.org/ecma-262/9.0/index.html#sec-names-and-keywords
  static bool IsIdentifierPart(u32 value, bool is_unicode);

  LUX_INLINE static bool IsDecimalDigit(u32 value) {
    return value >= '0' && value <= '9';
  }

  LUX_INLINE static bool IsLF(u32 value) { return value == kLF; }

  LUX_INLINE static bool IsCR(u32 value) { return value == kCR; }

  LUX_INLINE static bool IsHexDigit(u32 value) {
    return IsDecimalDigit(value) || (value >= 'a' && value <= 'f') ||
           (value >= 'A' && value <= 'F');
  }

  LUX_INLINE static bool IsBinaryDigit(u32 value) {
    return (value >= '0' && value <= '1');
  }

  LUX_INLINE static bool IsOctalDigit(u32 value) {
    return (value >= '0' && value <= '7');
  }

  LUX_INLINE static bool IsStartUnicodeEscapeSequence(u32 u) {
    return u == 'u';
  }

  LUX_INLINE static bool IsStartAsciiEscapeSequence(u32 u) { return u == 'x'; }

  LUX_INLINE static bool IsStartEscapeSequence(u32 u) {
    return IsStartUnicodeEscapeSequence(u) || IsStartAsciiEscapeSequence(u);
  }

  LUX_INLINE static u8 GetAsciiCtrlCodeFromWord(u32 word) {
    static const char kCtrlMap[] = {'\a', '\b', 0, 0,    0, '\f', 0, 0,
                                    0,    0,    0, 0,    0, '\n', 0, 0,
                                    0,    '\r', 0, '\t', 0, '\v'};
    if (word < 97 && word > 118) {
      return 0;
    }

    return kCtrlMap[word - 97];
  }

  LUX_INLINE static u8 GetAsciiCodeFromCarretWord(u32 word) {
    static const int kCtrlMap[] = {0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
                                   0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
                                   0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15,
                                   0x16, 0x17, 0x18, 0x19, 0x1a};
    static const int kCtrlSymbolMap[] = {0x1b, 0x1c, 0x1d, 0x1e, 0x1f};

    if (!IsCtrlSuccessor(word)) {
      return 0;
    }

    if ((word >= 91 && word <= 95)) {
      return kCtrlSymbolMap[word - 91];
    }
    if (word == 63) {
      return 0x07F;
    }

    return kCtrlMap[ToLower(word) - 97];
  }

  static u32 ToHexValue(u32 uchar);
};

}  // namespace lux

#endif  // SRC_CHARS_H_
