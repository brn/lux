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

#include <gtest/gtest.h>
#include "../utils/compare_node.h"
#include "../utils/isolate_setup.h"
#include "../../src/regexp.h"
#include "../../src/unicode.h"

class RegExpTest: public lux::IsolateSetup {
 protected:
  template <bool error = false, bool show_error = false>
  void RunTest(const char* regexp, int line, const char* expected,
               lux::SourcePosition source_position = lux::SourcePosition()) {
    lux::ErrorReporter er;
    lux::SourcePosition sp;
    auto s = lux::Unicode::ConvertUtf8StringToUtf16String(regexp);
    lux::regexp::Parser p(&er, &sp, s);
    p.Parse();
    if (error) {
      ASSERT_TRUE(er.HasPendingError());
      for (auto &e : er) {
        EXPECT_TRUE(e->source_position() == source_position);
      }
      if (show_error) {
        er.PrintError();
      }
    } else {
      auto a = p.node()->ToString();
      lux::testing::CompareNode(line, a.c_str(), expected);
    }
  }
};


namespace {
TEST_F(RegExpTest, Simple) {
  RunTest("(aa*|bb*)+", 13,
          "[Root]\n"
          "  [Conjunction]\n"
          "    [Repeat more than 1]\n"
          "      [Group type = CAPTURE]\n"
          "        [Alternate]\n"
          "          [Repeat more than 0]\n"
          "            [Conjunction]\n"
          "              [Char 'a']\n"
          "              [Char 'a']\n"
          "          [Repeat more than 0]\n"
          "            [Conjunction]\n"
          "              [Char 'b']\n"
          "              [Char 'b']\n");
}

TEST_F(RegExpTest, Alternate) {
  RunTest("(aa*|bb*|cc*|dd+)+", 23,
          "[Root]\n"
          "  [Conjunction]\n"
          "    [Repeat more than 1]\n"
          "      [Group type = CAPTURE]\n"
          "        [Alternate]\n"
          "          [Repeat more than 0]\n"
          "            [Conjunction]\n"
          "              [Char 'a']\n"
          "              [Char 'a']\n"
          "          [Alternate]\n"
          "            [Repeat more than 0]\n"
          "              [Conjunction]\n"
          "                [Char 'b']\n"
          "                [Char 'b']\n"
          "            [Alternate]\n"
          "              [Repeat more than 0]\n"
          "                [Conjunction]\n"
          "                  [Char 'c']\n"
          "                  [Char 'c']\n"
          "              [Repeat more than 1]\n"
          "                [Conjunction]\n"
          "                  [Char 'd']\n"
          "                  [Char 'd']\n");
}

TEST_F(RegExpTest, Range) {
  RunTest("(aa*|bb*){1,3}", 13,
          "[Root]\n"
          "  [Conjunction]\n"
          "    [RepeatRange more than 1, less than 3]\n"
          "      [Group type = CAPTURE]\n"
          "        [Alternate]\n"
          "          [Repeat more than 0]\n"
          "            [Conjunction]\n"
          "              [Char 'a']\n"
          "              [Char 'a']\n"
          "          [Repeat more than 0]\n"
          "            [Conjunction]\n"
          "              [Char 'b']\n"
          "              [Char 'b']\n");
}

TEST_F(RegExpTest, CharClass) {
  RunTest("[abcdefg,{}()?]", 16,
          "[Root]\n"
          "  [Conjunction]\n"
          "    [CharClass exclude = false]\n"
          "      [Char 'a']\n"
          "      [Char 'b']\n"
          "      [Char 'c']\n"
          "      [Char 'd']\n"
          "      [Char 'e']\n"
          "      [Char 'f']\n"
          "      [Char 'g']\n"
          "      [Char ',']\n"
          "      [Char '{']\n"
          "      [Char '}']\n"
          "      [Char '(']\n"
          "      [Char ')']\n"
          "      [Char '?']\n");
}

TEST_F(RegExpTest, CharClassExclude) {
  RunTest("[^abcdefg,{}()?]", 16,
          "[Root]\n"
          "  [Conjunction]\n"
          "    [CharClass exclude = true]\n"
          "      [Char 'a']\n"
          "      [Char 'b']\n"
          "      [Char 'c']\n"
          "      [Char 'd']\n"
          "      [Char 'e']\n"
          "      [Char 'f']\n"
          "      [Char 'g']\n"
          "      [Char ',']\n"
          "      [Char '{']\n"
          "      [Char '}']\n"
          "      [Char '(']\n"
          "      [Char ')']\n"
          "      [Char '?']\n");
}

TEST_F(RegExpTest, Question) {
  RunTest("(aa*|bb*)?", 13,
          "[Root]\n"
          "  [Conjunction]\n"
          "    [RepeatRange more than 0, less than 1]\n"
          "      [Group type = CAPTURE]\n"
          "        [Alternate]\n"
          "          [Repeat more than 0]\n"
          "            [Conjunction]\n"
          "              [Char 'a']\n"
          "              [Char 'a']\n"
          "          [Repeat more than 0]\n"
          "            [Conjunction]\n"
          "              [Char 'b']\n"
          "              [Char 'b']\n");
}

TEST_F(RegExpTest, Uncapture) {
  RunTest("(aa*|bb*)(?:aaa)", 17,
          "[Root]\n"
          "  [Conjunction]\n"
          "    [Group type = CAPTURE]\n"
          "      [Alternate]\n"
          "        [Repeat more than 0]\n"
          "          [Conjunction]\n"
          "            [Char 'a']\n"
          "            [Char 'a']\n"
          "        [Repeat more than 0]\n"
          "          [Conjunction]\n"
          "            [Char 'b']\n"
          "            [Char 'b']\n"
          "    [Group type = UNCAPTURE]\n"
          "      [Conjunction]\n"
          "        [Char 'a']\n"
          "        [Char 'a']\n"
          "        [Char 'a']\n");
}

TEST_F(RegExpTest, PositionLookahead) {
  RunTest("(aa*|bb*)(?=aaa)", 17,
          "[Root]\n"
          "  [Conjunction]\n"
          "    [Group type = CAPTURE]\n"
          "      [Alternate]\n"
          "        [Repeat more than 0]\n"
          "          [Conjunction]\n"
          "            [Char 'a']\n"
          "            [Char 'a']\n"
          "        [Repeat more than 0]\n"
          "          [Conjunction]\n"
          "            [Char 'b']\n"
          "            [Char 'b']\n"
          "    [Group type = POSITIVE_LOOKAHEAD]\n"
          "      [Conjunction]\n"
          "        [Char 'a']\n"
          "        [Char 'a']\n"
          "        [Char 'a']\n");
}

TEST_F(RegExpTest, NegativeLookahead) {
  RunTest("(aa*|bb*)(?!aaa)", 17,
          "[Root]\n"
          "  [Conjunction]\n"
          "    [Group type = CAPTURE]\n"
          "      [Alternate]\n"
          "        [Repeat more than 0]\n"
          "          [Conjunction]\n"
          "            [Char 'a']\n"
          "            [Char 'a']\n"
          "        [Repeat more than 0]\n"
          "          [Conjunction]\n"
          "            [Char 'b']\n"
          "            [Char 'b']\n"
          "    [Group type = NEGATIVE_LOOKAHEAD]\n"
          "      [Conjunction]\n"
          "        [Char 'a']\n"
          "        [Char 'a']\n"
          "        [Char 'a']\n");
}

TEST_F(RegExpTest, GroupParenError) {
  RunTest<true>("(aa*|bb*", 0, nullptr, lux::SourcePosition(8, 8, 0, 0));
}

TEST_F(RegExpTest, CharClassBracketError) {
  RunTest<true>("[aaa", 0, nullptr, lux::SourcePosition(4, 4, 0, 0));
}

TEST_F(RegExpTest, RangeRepeatBraceError) {
  RunTest<true>("aaa{1", 0, nullptr, lux::SourcePosition(5, 5, 0, 0));
}

TEST_F(RegExpTest, RangeRepeatFirstNaNError) {
  RunTest<true>("aaa{x", 0, nullptr, lux::SourcePosition(3, 4, 0, 0));
}

TEST_F(RegExpTest, RangeRepeatRequiredError) {
  RunTest<true>("aaa{x,}", 0, nullptr, lux::SourcePosition(3, 4, 0, 0));
}

TEST_F(RegExpTest, RangeRepeatSecondNaNError) {
  RunTest<true>("aaa{1,x}", 0, nullptr, lux::SourcePosition(5, 6, 0, 0));
}

TEST_F(RegExpTest, NothingToRepeat) {
  RunTest<true>("+", 0, nullptr, lux::SourcePosition(0, 1, 0, 0));
}

TEST_F(RegExpTest, NothingToRepeat2) {
  RunTest<true>("*", 0, nullptr, lux::SourcePosition(0, 1, 0, 0));
}

TEST_F(RegExpTest, NothingToRepeat3) {
  RunTest<true>("{", 0, nullptr, lux::SourcePosition(0, 1, 0, 0));
}
}  // namespace
