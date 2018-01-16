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

namespace i6 {
template<std::size_t N, typename CharT>
inline CharT Mask(CharT ch) {
  return Bitmask<N, u32>::lower & ch;
}

class Utf8CodePoint {
 public:
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
      str_(str), len_(strlen(str)) {}

  Utf8CodePoint Next() {
    if (HasMore()) {
      return Utf8CodePoint(str_[index_++]);
    }
    return Utf8CodePoint(0);
  }

  Utf8CodePoint Current() {
    return Utf8CodePoint(str_[index_]);
  }

  bool HasMore() {
    return index_ < len_;
  }

 private:
  const char* str_;
  size_t len_;
  int32_t index_;
};

int8_t GetU8Length(u8 u) {
  auto length_bit = Mask<8>(u);
  if (length_bit < 0x80) {
    return 1;
  } else if ((length_bit >> 5) == 0x6) {
    return 2;
  } else if ((length_bit >> 4) == 0xE) {
    return 3;
  } else if ((length_bit >> 4) == 0x1E) {
    return 4;
  }
  return 0;
}

u32 ConvertUtf8ToUC32Case2(Utf8Iterator* it) {
  u32 ret = Mask<5>(it->Current().code()) << 6;
  ret |= Mask<6>(it->Next().code());
  return ret;
}

u32 ConvertUtf8ToUC32Case3(Utf8Iterator* it) {
  u32 ret = Mask<4>(it->Current().code()) << 12;
  ret |= Mask<6>(it->Next().code()) << 6;
  ret |= Mask<6>(it->Next().code());
  return ret;
}

u32 ConvertUtf8ToUC32Case4(Utf8Iterator* it) {
  u32 ret = Mask<3>(it->Current().code()) << 18;
  ret |= Mask<6>(it->Next().code()) << 12;
  ret |= Mask<6>(it->Next().code()) << 6;
  ret |= Mask<6>(it->Next().code());
  return ret;
}

u32 ConverUtf8ToUC32(Utf8Iterator* it) {
  int len = GetU8Length(it->Next().code());
  switch (len) {
    case 1:
      return Mask<8>(it->Current().code());
    case 2:
      return ConvertUtf8ToUC32Case2(it);
    case 3:
      return ConvertUtf8ToUC32Case3(it);
    case 4:
      return ConvertUtf8ToUC32Case4(it);
    default:
      return 0;
  }
}

void ConvertUtf8ToUtf16(Utf8Iterator* it, Utf16CodePoint* buf, int32_t* index) {
  u32 u = ConverUtf8ToUC32(it);
  if (u > 0) {
    buf[(*index)++] = Utf16CodePoint(u);
  }
}

const Utf16CodePoint* Unicode::ConvertUtf8StringToUtf16String(
    Isolate* isolate, const char* str) {
  auto buf = reinterpret_cast<Utf16CodePoint*>(
      isolate->heap()->Allocate(strlen(str) * sizeof(Utf16CodePoint)));
  Utf8Iterator it(str);
  int index = 0;
  while (it.HasMore()) {
    ConvertUtf8ToUtf16(&it, buf, &index);
  }
  return buf;
}
}  // namespace i6
