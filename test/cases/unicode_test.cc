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

#include <gtest/gtest.h>
#include "../utils/compare_string.h"
#include "../utils/isolate_setup.h"
#include "../utils/readfile.h"
#include "../../src/unicode.h"
#include "../../src/platform/os.h"

class UnicodeTest: public lux::IsolateSetup {
 protected:
  void SetUp() {
    lux::IsolateSetup::SetUp();
    lux::SPrintf(&valid_utf8_txt_target_path_, false, "%s/%s",
            UNICODE_TEXTS_DIR, "valid-utf8.txt");
    lux::SPrintf(&valid_utf8_txt_expectation_path_, false, "%s/%s",
            UNICODE_TEXTS_DIR, "valid-utf8.result.txt");
    lux::SPrintf(&valid_surrogate_txt_target_path_, false, "%s/%s",
            UNICODE_TEXTS_DIR, "valid-surrogate-pair.txt");
    lux::SPrintf(&valid_surrogate_txt_expectation_path_, false, "%s/%s",
            UNICODE_TEXTS_DIR, "valid-surrogate-pair.result.txt");
  }

  std::string valid_utf8_txt_target_path_;
  std::string valid_utf8_txt_expectation_path_;
  std::string valid_surrogate_txt_target_path_;
  std::string valid_surrogate_txt_expectation_path_;
};

namespace {
::testing::AssertionResult Failed(const lux::Utf16CodePoint& cp,
                                  const lux::Utf16String::iterator& it) {
  return ::testing::AssertionFailure()
    << "Invalid unicode charactor. at: "
    << it.current_position()
    << " value: " << cp.code();
}

::testing::AssertionResult IsValid(const lux::Utf16CodePoint& cp,
                                   const lux::Utf16String::iterator& it) {
  if (cp.IsValid()) {
    return ::testing::AssertionSuccess();
  }
  return Failed(cp, it);
}

inline void RunConvertTest(const char* input,
                           const char* expected,
                           size_t expected_size) {
  auto source = lux::testing::ReadFile(input);
  auto result = lux::testing::ReadFile(expected);
  std::string buffer;
  std::string utf8_buffer;
  static const char* kFormat = "%#019x";
  auto utf16_string =
    lux::Unicode::ConvertUtf8StringToUtf16String(source.c_str());
  int index = 0;
  size_t size = 0;
  auto end = utf16_string.end();
  for (auto it = utf16_string.begin(); it != end; ++it) {
    const lux::Utf16CodePoint cp = *it;
    ASSERT_TRUE(IsValid(cp, it));
    if (cp.IsAscii()) {
      buffer.append(1, cp.ToAscii());
    } else {
      if (!cp.IsSurrogatePair()) {
        lux::SPrintf(&buffer, true, kFormat, cp.code());
      } else {
        lux::SPrintf(&buffer, true, kFormat, cp.ToHighSurrogate());
        lux::SPrintf(&buffer, true, kFormat, cp.ToLowSurrogate());
      }
    }
    utf8_buffer.append(cp.ToUtf8String());
    size += cp.IsSurrogatePair()? 2: 1;
    index++;
  }
  ASSERT_EQ(expected_size, size);
  lux::testing::CompareBuffer(buffer, result);
  lux::testing::CompareBuffer(utf8_buffer, source);
}

TEST_F(UnicodeTest, ConvertUtf8AsciiToUtf16) {
  const char* asciis = "abcdefghijklmnopqrstuvwxyz"
    "ABCDEFGHIJKLMNOPQRSTUVWXY"
    "Z0123456789"
    "\"'`!@#$%^&*_+|~()[]{}";
  auto str = lux::Unicode::ConvertUtf8StringToUtf16String(asciis);
  int i = 0;
  for (auto &it : str) {
    EXPECT_EQ(it.code(), asciis[i++]);
  }
}

TEST_F(UnicodeTest, ConvertMultiByte) {
  RunConvertTest(valid_utf8_txt_target_path_.c_str(),
                 valid_utf8_txt_expectation_path_.c_str(),
                 148);
}

TEST_F(UnicodeTest, ConvertSurrogatePair) {
  RunConvertTest(valid_surrogate_txt_target_path_.c_str(),
                 valid_surrogate_txt_expectation_path_.c_str(),
                 676);
}
}  // namespace
