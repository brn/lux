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

#ifndef _I6_SRC_UNICODE_H_
#define _I6_SRC_UNICODE_H_

#include <iterator>
#include "./utils.h"
#include "./isolate.h"

namespace i6 {
class Utf16CodePoint {
 public:
  explicit Utf16CodePoint(u32 u)
      : u_(u) {}

  Utf16CodePoint()
      : u_(0) {}

  u16 code() const {return u_;}

  operator u32() const {return u_;}

  bool IsValid() {
    return u_ != 0;
  }

  bool IsSurrogatePair() {
    return u_ > 0x10000;
  }

  u16 HightSurrogate() {
    u16 ud = u_ - 0x10000;
    return 0xD800 | (ud & (~0xFF));
  }

  u16 LowSurrogate() {
    u16 ud = u_ - 0x10000;
    return 0xDC00 | (ud & 0xFF);
  }

 private:
  u32 u_;
};

class Utf16String;

class Utf16StringIterator:
      public std::iterator<std::forward_iterator_tag, Utf16CodePoint> {
  friend class Utf16String;
 public:
  Utf16StringIterator()
    : index_(0), utf16_codepoint_(nullptr) {}

  Utf16StringIterator(Utf16CodePoint* utf16_codepoint, int32_t size)
      : index_(0), size_(size), utf16_codepoint_(utf16_codepoint) {}

  Utf16StringIterator(const Utf16StringIterator& it)
      : index_(it.index_), size_(it.size_),
        utf16_codepoint_(it.utf16_codepoint_) {}

  const Utf16CodePoint& operator*() const;

  const Utf16CodePoint* operator->() const;

  Utf16StringIterator& operator =(const Utf16StringIterator& it) {
    index_ = it.index_;
    utf16_codepoint_ = it.utf16_codepoint_;
    size_ = it.size_;
    return *this;
  }

  const bool operator ==(const Utf16StringIterator& it) const {
    return index_ == it.index_ && it.utf16_codepoint_ == utf16_codepoint_;
  }

  const bool operator !=(const Utf16StringIterator& it) const {
    return !this->operator==(it);
  }

  Utf16StringIterator& operator++() {
    if (index_ < size_) {
      index_++;
    }
    return *this;
  }

  Utf16StringIterator operator++(int value) {
    return Utf16StringIterator(utf16_codepoint_, index_ + value, size_);
  }

  Utf16StringIterator operator+(int value) {
    return Utf16StringIterator(utf16_codepoint_, index_ + value, size_);
  }

  Utf16StringIterator& operator+=(int value) {
    index_ += value;
    return *this;
  }

 private:
  Utf16StringIterator(
      Utf16CodePoint* utf16_codepoint, int32_t index, int32_t size)
    : index_(index), utf16_codepoint_(utf16_codepoint) {}

  int32_t index_;
  int32_t size_;
  Utf16CodePoint* utf16_codepoint_;
};

class Utf16String {
 public:
  using iterator = Utf16StringIterator;
  explicit Utf16String(Utf16CodePoint* utf16_codepoint, size_t size)
      : size_(size), utf16_codepoint_(utf16_codepoint) {}

  Utf16StringIterator begin() {
    return Utf16StringIterator(utf16_codepoint_, size_);
  }

  Utf16StringIterator end() {
    return Utf16StringIterator(utf16_codepoint_, size_, size_);
  }

  bool IsAsciiEqual(const char* ascii) const;

  int32_t size() const {return size_;}

 private:
  static Utf16StringIterator kEnd;

  int32_t size_;
  Utf16CodePoint* utf16_codepoint_;
};

class Unicode {
 public:
  static const Utf16CodePoint InvalidCodePoint() {
    static Utf16CodePoint invalid(0);
    return invalid;
  }

  static const Utf16String ConvertUtf8StringToUtf16String(
      Isolate* isolate, const char*);
};
}  // namespace i6

#endif 
