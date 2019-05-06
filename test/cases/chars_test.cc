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
    std::string identifier_part;
    lux::SPrintf(&identifier_part, false, "%s/%s", UNICODE_TEXTS_DIR,
                 "identifier_part.txt");
    identifier_part_ = lux::testing::ReadFile(identifier_start.c_str());
  }

  std::string identifier_start_;
  std::string identifier_part_;
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
