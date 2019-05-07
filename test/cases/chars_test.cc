/**
 * The MIT License (MIT)
 * Copyright (c) Taketoshi Aono
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 * @fileoverview
 * @author Taketoshi Aono
 */

#include "../../src/chars.h"
#include <gtest/gtest.h>
#include "../../src/platform/os.h"
#include "../../src/unicode.h"
#include "../../src/utils.h"
#include "../utils/readfile.h"

class CharsTest : public ::testing::Test {
 protected:
  void SetUp() {
    std::string identifier_start;
    lux::SPrintf(&identifier_start, false, "%s/%s", UNICODE_TEXTS_DIR,
                 "identifier_start.txt");
    identifier_start_ = lux::testing::ReadFile(identifier_start.c_str());
    identifier_start_ =
        identifier_start_.substr(0, identifier_start_.size() - 1);

    std::string identifier_part;
    lux::SPrintf(&identifier_part, false, "%s/%s", UNICODE_TEXTS_DIR,
                 "identifier_part.txt");
    identifier_part_ = lux::testing::ReadFile(identifier_start.c_str());
    identifier_part_ = identifier_part_.substr(0, identifier_part_.size() - 1);

    std::string whitespaces;
    lux::SPrintf(&whitespaces, false, "%s/%s", UNICODE_TEXTS_DIR,
                 "whitespaces.txt");
    whitespaces_ = lux::testing::ReadFile(whitespaces.c_str());
    whitespaces_ = whitespaces_.substr(0, whitespaces_.size() - 1);
  }

  std::string identifier_start_;
  std::string identifier_part_;
  std::string whitespaces_;
};

TEST_F(CharsTest, IdentifierStartTest) {
  auto utf16_string =
      lux::Unicode::ConvertUtf8StringToUtf16String(identifier_start_.c_str());
  for (auto &cp : utf16_string) {
    ASSERT_TRUE(lux::Chars::IsIdentifierStart(cp.code()))
        << "Char code " << std::hex << cp.code()
        << " should be Identifier start.";
  }
}

TEST_F(CharsTest, IdentifierPartTest) {
  auto utf16_string =
      lux::Unicode::ConvertUtf8StringToUtf16String(identifier_part_.c_str());
  for (auto &cp : utf16_string) {
    ASSERT_TRUE(lux::Chars::IsIdentifierPart(cp.code(), false))
        << "Char code " << std::hex << cp.code()
        << " should be Identifier part.";
  }
}

TEST_F(CharsTest, IsAscii) {
  ASSERT_TRUE(lux::Chars::IsAscii(1)) << "Char code 1  should be ASCII.";
  ASSERT_TRUE(lux::Chars::IsAscii(126)) << "Char code 126  should be ASCII.";
}

TEST_F(CharsTest, IsWhiteSpace) {
  auto utf16_string =
      lux::Unicode::ConvertUtf8StringToUtf16String(whitespaces_.c_str());
  for (auto &cp : utf16_string) {
    ASSERT_TRUE(lux::Chars::IsWhiteSpace(cp.code()))
        << "Char code " << std::hex << cp.code() << " should be Whitespace.";
  }
}

TEST_F(CharsTest, IsDecimalDigit) {
  std::string nums("0123456789");
  for (auto &ch : nums) {
    ASSERT_TRUE(lux::Chars::IsDecimalDigit(ch))
        << "Char code " << std::hex << ch << " should be DeciamlDigit.";
  }
}

TEST_F(CharsTest, IsLF) {
  ASSERT_TRUE(lux::Chars::IsLF('\n')) << "Char code " << std::hex << '\n'
                                      << " should be ASCII.";
}

TEST_F(CharsTest, IsCR) {
  ASSERT_TRUE(lux::Chars::IsCR('\r'))
      << "Char code " << std::hex << '\r' << " should be ASCII.";
}

TEST_F(CharsTest, IsHexDigit) {
  std::string nums("0123456789abcdefABCDEF");
  for (auto &ch : nums) {
    ASSERT_TRUE(lux::Chars::IsHexDigit(ch))
        << "Char code " << std::hex << ch << " should be HexDigit.";
  }
  ASSERT_FALSE(lux::Chars::IsHexDigit('G'))
      << "Char code " << std::hex << 'G' << " should not be HexDigit.";
  ASSERT_FALSE(lux::Chars::IsHexDigit('g'))
      << "Char code " << std::hex << 'g' << " should not be HexDigit.";
}

TEST_F(CharsTest, IsBinaryDigit) {
  ASSERT_TRUE(lux::Chars::IsBinaryDigit('0'))
      << "Char code " << std::hex << '0' << " should be BinaryDigit.";
  ASSERT_TRUE(lux::Chars::IsBinaryDigit('1'))
      << "Char code " << std::hex << '0' << " should be BinaryDigit.";
  ASSERT_FALSE(lux::Chars::IsBinaryDigit('2'))
      << "Char code " << std::hex << '2' << " should not be BinaryDigit.";
}

TEST_F(CharsTest, IsOctalDigit) {
  std::string nums("012345678");
  for (auto &ch : nums) {
    ASSERT_TRUE(lux::Chars::IsDecimalDigit(ch))
        << "Char code " << std::hex << ch << " should be DeciamlDigit.";
  }
  ASSERT_FALSE(lux::Chars::IsOctalDigit('9'))
      << "Char code " << std::hex << '9' << " should not be OctalDigit.";
}

TEST_F(CharsTest, IsStartUnicodeEscapeSequence) {
  ASSERT_TRUE(lux::Chars::IsStartUnicodeEscapeSequence('u'))
      << "Char code " << std::hex << 'u'
      << " should be StartUnicodeEscapeSequence.";
}

TEST_F(CharsTest, IsStartAsciiEscapeSequence) {
  ASSERT_TRUE(lux::Chars::IsStartAsciiEscapeSequence('x'))
      << "Char code " << std::hex << 'x'
      << " should be StartUnicodeEscapeSequence.";
}

TEST_F(CharsTest, IsStartEscapeSequence) {
  ASSERT_TRUE(lux::Chars::IsStartEscapeSequence('x'))
      << "Char code " << std::hex << 'x' << " should be StartEscapeSequence.";
  ASSERT_TRUE(lux::Chars::IsStartEscapeSequence('u'))
      << "Char code " << std::hex << 'u' << " should be StartEscapeSequence.";
}
