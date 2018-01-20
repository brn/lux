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

#include "./heap.h"
#include "./isolate.h"
#include "./unicode.h"
#include "./utils.h"

namespace lux {
template<std::size_t N, typename CharT>
inline CharT Mask(CharT ch) {
  return Bitmask<N, u32>::lower & ch;
}

std::string Utf16CodePoint::ToUtf8String() const {
  return std::string(Unicode::ConvertUC32ToUC8(u_).data());
}

class Utf8CodePoint {
 public:
  LUX_INLINE static bool IsNotNull(u8 c) {
    return c != 0;
  }

  explicit Utf8CodePoint(u8 u)
      : u_(u) {}

  Utf8CodePoint(): u_(0) {}

  bool IsValid() {
    return u_ != 0;
  }

  u8 code() const {return u_;}

  operator u8() const {
    return u_;
  }

 private:
  u8 u_;
};

class Utf8Iterator {
 public:
  explicit Utf8Iterator(const char* str):
      str_(str), len_(strlen(str)), index_(0) {}

  Utf8CodePoint Next() {
    if (HasMore()) {
      return Utf8CodePoint(str_[index_++]);
    }
    return Utf8CodePoint(0);
  }

  Utf8CodePoint Current() {
    return Utf8CodePoint(str_[index_ > 0? index_ - 1: index_]);
  }

  Utf8CodePoint Peek(int n = 0) {
    if (index_ + n < len_) {
      return Utf8CodePoint(str_[index_ + n]);
    }
    return Utf8CodePoint(0);
  }

  bool HasMore() {
    return index_ < len_;
  }

 private:
  const char* str_;
  size_t len_;
  int32_t index_;
};

u32 Convert2ByteUtf8ToUC32(Utf8Iterator* it) {
  auto c = it->Current().code();
  if (Utf8CodePoint::IsNotNull(c)) {
    u32 ret = Mask<5>(it->Current().code()) << 6;
    c = it->Next().code();
    if (utf8::IsValidSequence(c)) {
      ret |= Mask<6>(c);
      return ret;
    }
  }
  return 0;
}

u32 Convert3ByteUtf8ToUC32(Utf8Iterator* it) {
  const int kMinimumRange = 0x00000800;
  auto c = it->Current().code();
  if (Utf8CodePoint::IsNotNull(c)) {
    u32 ret = Mask<4>(c) << 12;
    c = it->Next().code();
    if (utf8::IsValidSequence(c)) {
      ret |= Mask<6>(c) << 6;
      c = it->Next().code();
      if (utf8::IsValidSequence(c)) {
        ret |= Mask<6>(c);
        if (ret > kMinimumRange && utf16::IsOutOfSurrogateRange(ret)) {
          return ret;
        }
      }
    }
  }
  return 0;
}

u32 Convert4ByteUtf8ToUC32(Utf8Iterator* it) {
  const int kMinimumRange = 0x000010000;
  auto c = it->Current().code();
  if (Utf8CodePoint::IsNotNull(c)) {
    u32 next = Mask<3>(c) << 18;
    c = it->Next().code();
    if (utf8::IsValidSequence(c)) {
      next |= Mask<6>(c) << 12;
      c = it->Next().code();
      if (utf8::IsValidSequence(c)) {
        next |= Mask<6>(c) << 6;
        c = it->Next().code();
        if (utf8::IsValidSequence(c)) {
          next |= Mask<6>(c);
          if (next >= kMinimumRange && next <= 0x10FFFF) {
            return next;
          }
        }
      }
    }
  }
  return 0;
}

u32 ConverUtf8ToUC32(Utf8Iterator* it) {
  int len = utf8::GetByteCount(it->Next().code());
  switch (len) {
    case 1:
      return Mask<8>(it->Current().code());
    case 2:
      return Convert2ByteUtf8ToUC32(it);
    case 3:
      return Convert3ByteUtf8ToUC32(it);
    case 4:
      return Convert4ByteUtf8ToUC32(it);
    default:
      return 0;
  }
}

bool ConvertUtf8ToUtf16(Utf8Iterator* it, Utf16CodePoint* buf, int32_t* index) {
  u32 u = ConverUtf8ToUC32(it);
  if (u > 0) {
    buf[(*index)++] = Utf16CodePoint(u);
    return true;
  }
  return false;
}

const Utf16CodePoint& Utf16StringIterator::operator*() const {
  return utf16_codepoint_[index_];
}

const Utf16CodePoint* Utf16StringIterator::operator->() const {
  return &utf16_codepoint_[index_];
}

bool Utf16String::IsAsciiEqual(const char* ascii) const {
  auto ascii_len = strlen(ascii);
  if (size_ != ascii_len) {
    return false;
  }

  for (int i = 0; i < size_; i++) {
    if (utf16_codepoint_[i] != ascii[i]) {
      return false;
    }
  }

  return true;
}

std::string Utf16String::ToUtf8String() const {
  std::stringstream st;
  for (int i = 0; i < size_; i++) {
    st << utf16_codepoint_[i].ToUtf8String();
  }
  return st.str();
}

static const size_t kUtf16CodePointSize = sizeof(Utf16CodePoint);

const Utf16String Unicode::ConvertUtf8StringToUtf16String(
    Isolate* isolate, const char* str) {
  auto buf = reinterpret_cast<Utf16CodePoint*>(
      isolate->heap()->Allocate((strlen(str) * kUtf16CodePointSize)
                                + (kUtf16CodePointSize * 2)));
  Utf8Iterator it(str);
  int index = 0;
  while (it.HasMore()) {
    if (!ConvertUtf8ToUtf16(&it, buf, &index)) {
      break;
    }
  }
  buf[index + 1] = Utf16CodePoint();
  return Utf16String(buf, index);
}

unicode::Utf8Bytes Unicode::ConvertUC32ToUC8(u32 uc) {
  unicode::Utf8Bytes b;
#define U8(v) static_cast<u8>(v)
  if (uc < 0x80) {
    // 0000 0000-0000 007F | 0xxxxxxx
    b[0] = uc;
    b[1] = '\0';
  } else if (uc < 0x800) {
    // 0000 0080-0000 07FF | 110xxxxx 10xxxxxx
    b[0] = (uc >> 6) | 0xC0;
    b[1] = (uc & 0x3F) | 0x80;
    b[2] = '\0';
  } else if (uc < 0x10000) {
    // 0000 0800-0000 FFFF | 1110xxxx 10xxxxxx 10xxxxxx
    b[0] = (uc >> 12) | 0xE0;
      b[1] = ((uc >> 6) & 0x3F) | 0x80;
    b[2] = (uc & 0x3F) | 0x80;
    b[3] = '\0';
  } else {
    INVALIDATE(uc <= unicode::kUnicodeMax);
    // 0001 0000-0010 FFFF | 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    b[0] = U8((uc >> 18) | 0xF0);
    b[1] = U8(((uc >> 12) & 0x3F) | 0x80);
    b[2] = U8(((uc >> 6) & 0x3F) | 0x80);
    b[3] = U8((uc & 0x3F) | 0x80);
    b[4] = '\0';
  }
#undef U8
  return b;
}
}  // namespace lux
