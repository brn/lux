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

#ifndef SRC_UNICODE_H_
#define SRC_UNICODE_H_

#include <array>
#include <iterator>
#include <string>
#include "./utils.h"
#include "./isolate.h"

namespace lux {

namespace unicode {
static const u32 kSurrogateBits = 10;
static const u32 kHighSurrogateMin = 0xD800;
static const u32 kHighSurrogateMax = 0xDBFF;
static const u32 kHighSurrogateMask = (1 << kSurrogateBits) - 1;
static const u32 kHighSurrogateOffset = kHighSurrogateMin - (0x10000 >> 10);

static const u32 kLowSurrogateMin = 0xDC00;
static const u32 kLowSurrogateMax = 0xDFFF;
static const u32 kLowSurrogateMask = (1 << kSurrogateBits) - 1;
static const u32 kSurrogateMin = kHighSurrogateMin;
static const u32 kSurrogateMax = kLowSurrogateMax;
static const u32 kSurrogateMask = (1 << (kSurrogateBits + 1)) - 1;

using Utf8Bytes = std::array<char, 5>;
static const u32 kUtf16Max = 0xFFFF;
static const u32 kUnicodeMin = 0x000000;
static const u32 kUnicodeMax = 0x10FFFF;

static const u8 kAsciiMax = 0x7F;

static const uint8_t kAsciiNumericStart = 48;
static const uint8_t kAsciiNumericEnd = 57;
}  // namespace unicode

/**
 * The utility class of the utf-8 char.
 */
namespace utf8 {
/**
 * Return byte size of the utf-8 sequence.
 * @param uc utf-8 byte.
 * @return The byte size of utf-8 sequence.
 */
inline size_t GetByteCount(u8 u) {
  static const std::array<u8, UINT8_MAX + 1> kLengthMap = { {
      // 00000000 -> 00011111
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      // 00100000 -> 00111111
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      // 01000000 -> 01011111
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      // 01100000 -> 01111111  ASCII range end
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      // 10000000 -> 10011111  invalid
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      // 10100000 -> 10111111  invalid
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      // 11000000 -> 11011111  2 bytes range
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
      // 11100000 -> 11101111  3 bytes range
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      // 11110000 -> 11110111  4 bytes range
      4, 4, 4, 4, 4, 4, 4, 4,
      // 11111000 -> 11111111  invalid
      0, 0, 0, 0, 0, 0, 0, 0
    } };
  return kLengthMap[u];
}

/**
 * Check whether utf-8 byte is null or not.
 * @param uc utf-8 byte
 * @return true(if utf-8 byte is not null) false(if utf-8 byte is null)
 */
LUX_INLINE bool IsNotNull(u8 uc) {
  return uc != '\0';
}


/**
 * Checking whether utf-8 char is valid or not.
 * 1. Checking null char(\0)
 * 2. Checking the utf-8 byte that within the ascii range is not expressed as
 * more than one byte sequence.
 * @param uc utf-8 byte
 * @return true(if utf-8 byte is valid) false(if utf-8 byte is invalid)
 */
LUX_INLINE bool IsValidSequence(u8 uc) {
  return IsNotNull(uc) && (uc & 0xC0) == 0x80;
}


/**
 * Check whether utf-8 byte is within the ascii range.
 * @param uc utf-8 byte or char or int.
 * @return true(if utf-8 byte is ascii) false(if utf-8 byte is not ascii)
 */
template <typename T>
LUX_INLINE bool IsAscii(T uc) {return uc >= 0 && uc < unicode::kAsciiMax;}
}  // namespace utf8

/**
 * Utility class of the utf-16 char.
 */
namespace utf16 {
/**
 * Check whether a utf-32 char is surrogate pair or not.
 * @param uc utf-32 byte.
 * @return true(if surrogate pair) false(if not surrogate pair).
 */
LUX_INLINE bool IsSurrogatePairUC32(u32 uc) {
  return uc > unicode::kUtf16Max;
}

/**
 * Check whether a utf-16 char is surrogate pair or not.
 * @param uc utf-16 byte.
 * @return true(if surrogate pair) false(if not surrogate pair).
 */
LUX_INLINE bool IsSurrogatePairUC16(u16 uc) {
  return (uc & ~unicode::kSurrogateMask)
    == unicode::kSurrogateMin;
}

/**
 * Convert to high surrogate pair byte from a utf-32 surrogate pair byte.
 * @param uc utf-32 byte.
 * @return u16 high surrogate pair byte expression.
 */
LUX_INLINE u16 ToHighSurrogateu32(u32 uc) {
  return static_cast<u16>(
      (uc >> unicode::kSurrogateBits)
      + unicode::kHighSurrogateOffset);
}

/**
 * Convert to low surrogate pair byte from a utf-32 surrogate pair byte.
 * @param uc utf-32 byte.
 * @return u16 low surrogate pair byte expression.
 */
LUX_INLINE u16 ToLowSurrogateu32(u32 uc) {
  return static_cast<u16>(
      (uc & unicode::kLowSurrogateMask)
      + unicode::kLowSurrogateMin);
}


/**
 * Check whether a utf-32 char is not surrogate pair or not.
 * @param uc utf-32 byte.
 * @return true(if not surrogate pair) false(if surrogate pair)
 */
LUX_INLINE bool IsOutOfSurrogateRange(u32 uc) {
  return uc < unicode::kHighSurrogateMin
              || unicode::kLowSurrogateMax < uc;
}


/**
 * Check whether a utf-16 char is high surrogate pair or not.
 * @param uc utf-16 byte.
 * @return true(if high surrogate pair) false(if not high surrogate pair or not surrogate pair)
 */
LUX_INLINE bool IsHighSurrogateu16(u16 uc) {
  if (!IsSurrogatePairUC16(uc)) return false;
  return (uc & ~unicode::kHighSurrogateMask)
    == unicode::kHighSurrogateMin;
}


/**
 * Check whether a utf-16 char is low surrogate pair or not.
 * @param uc utf-16 byte.
 * @return true(if low surrogate pair) false(if not low surrogate pair or not surrogate pair)
 */
LUX_INLINE bool IsLowSurrogateu16(u16 uc) {
  if (!IsSurrogatePairUC16(uc)) return false;
  return (uc & ~unicode::kLowSurrogateMask)
    == unicode::kLowSurrogateMin;
}

LUX_INLINE bool IsNumericRange(u16 ch) {
  return ch >= unicode::kAsciiNumericStart && ch <= unicode::kAsciiNumericEnd;
}
}  // namespace utf16

class Utf16CodePoint {
 public:
  explicit Utf16CodePoint(u32 u)
      : u_(u) {}

  Utf16CodePoint()
      : u_(0) {}

  std::string ToUtf8String() const;

  LUX_INLINE u8 ToAscii() const {
    return static_cast<u8>(u_);
  }

  LUX_INLINE u16 code() const {return u_;}

  LUX_INLINE u32 ucs4_code() const {return u_;}

  LUX_INLINE operator u32() const {return u_;}

  LUX_INLINE bool IsValid() const {
    return u_ != 0;
  }

  LUX_INLINE bool IsAscii() const {
    return u_ > 0 && u_ <= 127;
  }

  LUX_INLINE bool IsSurrogatePair() const {
    return u_ > 0x10000;
  }

  LUX_INLINE u16 ToHighSurrogate() const {
    return (u_ >> unicode::kSurrogateBits) + unicode::kHighSurrogateOffset;
  }

  LUX_INLINE u16 ToLowSurrogate() const {
    return (u_ & unicode::kLowSurrogateMask) + unicode::kLowSurrogateMin;
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

  Utf16StringIterator(const Utf16CodePoint* utf16_codepoint, int32_t size)
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

  LUX_INLINE const bool operator ==(const Utf16StringIterator& it) const {
    return index_ == it.index_ && it.utf16_codepoint_ == utf16_codepoint_;
  }

  LUX_INLINE const bool operator !=(const Utf16StringIterator& it) const {
    return !this->operator==(it);
  }

  Utf16StringIterator& operator++() {
    if (index_ < size_) {
      index_++;
    }

    return *this;
  }

  LUX_INLINE Utf16StringIterator operator++(int value) {
    Utf16StringIterator tmp = *this;
    ++*this;
    return tmp;
  }

  LUX_INLINE Utf16StringIterator operator+(int value) {
    return Utf16StringIterator(utf16_codepoint_, index_ + value, size_);
  }

  LUX_INLINE Utf16StringIterator& operator+=(int value) {
    index_ += value;
    return *this;
  }

  LUX_INLINE uint32_t current_position() const {
    return index_;
  }

 private:
  Utf16StringIterator(
      const Utf16CodePoint* utf16_codepoint, int32_t index, int32_t size)
    : index_(index), utf16_codepoint_(utf16_codepoint) {}

  uint32_t index_;
  uint32_t size_;
  const Utf16CodePoint* utf16_codepoint_;
};

class Utf16String {
 public:
  class ParseIntResult {
   public:
    explicit ParseIntResult(uint64_t ret)
        : result_(ret) {}

    bool IsNaN() const {
      return (result_ & 0x1) == 1;
    }
    uint64_t value() const {
      return result_ >> 1;
    }
    static ParseIntResult Failure() {
      static ParseIntResult a(1);
      return a;
    }
   private:
    uint64_t result_;
  };

  using iterator = Utf16StringIterator;
  Utf16String()
      : size_(0), utf16_codepoint_(nullptr) {}

  explicit Utf16String(const Utf16CodePoint* utf16_codepoint, size_t size)
      : size_(size), utf16_codepoint_(utf16_codepoint) {}

  LUX_INLINE Utf16StringIterator begin() {
    return Utf16StringIterator(utf16_codepoint_, size_);
  }

  LUX_INLINE Utf16StringIterator end() {
    return Utf16StringIterator(utf16_codepoint_, size_, size_);
  }

  LUX_INLINE Utf16StringIterator begin() const {
    return Utf16StringIterator(utf16_codepoint_, size_);
  }

  LUX_INLINE Utf16StringIterator end() const {
    return Utf16StringIterator(utf16_codepoint_, size_, size_);
  }

  LUX_INLINE Utf16StringIterator cbegin() const {
    return begin();
  }

  LUX_INLINE Utf16StringIterator cend() const {
    return end();
  }

  bool IsAsciiEqual(const char* ascii) const;

  LUX_INLINE int32_t size() const {return size_;}

  std::string ToUtf8String() const;

  ParseIntResult ParseInt() const {
    static const std::array<int, 10> kAsciiNumericArray = {{
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9
      }};

    uint64_t acc = 0;
    bool parsed = false;
    int i = 0;
    for (auto &ch : *this) {
      if (utf16::IsNumericRange(ch.code())) {
        acc += PowerOf2<uint64_t>(10, i++)
          * kAsciiNumericArray[ch.code() - unicode::kAsciiNumericStart];
        parsed = true;
      } else {
        if (!parsed) {
          return ParseIntResult(1);
        }
        return ParseIntResult(acc << 1);
        break;
      }
    }

    return ParseIntResult(acc << 1);
  }

 private:
  static Utf16StringIterator kEnd;

  int32_t size_;
  const Utf16CodePoint* utf16_codepoint_;
};

class Unicode: private Static {
 public:
  static const Utf16CodePoint InvalidCodePoint() {
    static Utf16CodePoint invalid(0);
    return invalid;
  }

  static const Utf16String ConvertUtf8StringToUtf16String(
      Isolate* isolate, const char*);

  static unicode::Utf8Bytes ConvertUC32ToUC8(u32 uc);
};

}  // namespace lux

#endif  // SRC_UNICODE_H_
