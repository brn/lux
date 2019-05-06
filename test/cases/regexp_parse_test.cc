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
#include "../../src/objects/jsobject.h"
#include "../../src/regexp.h"
#include "../utils/compare_node.h"
#include "../utils/isolate_setup.h"

class RegExpTest : public lux::IsolateSetup {
 protected:
  template <bool error = false, bool show_error = false>
  void RunTest(const char* regexp, const char* expected,
               lux::SourcePosition source_position = lux::SourcePosition()) {
    lux::HandleScope scope;
    lux::ErrorReporter er;
    lux::SourcePosition sp;
    auto s = lux::JSString::New(isolate_, regexp);
    lux::regexp::Parser p(isolate_, &er, &sp, s);
    p.Parse();
    if (error) {
      ASSERT_TRUE(er.HasPendingError());
      for (auto& e : er) {
        EXPECT_PRED2(
            [&](auto a, auto b) {
              return a == b ? ::testing::AssertionSuccess()
                            : ::testing::AssertionFailure()
                                  << "Expected source position is " << b;
            },
            e->source_position(), source_position);
      }
      if (show_error) {
        er.PrintError();
      }
    } else {
      auto a = p.node()->ToString();
      ASSERT_TRUE(lux::testing::CompareNode(regexp, a.c_str(), expected));
    }
  }
};

namespace {
TEST_F(RegExpTest, Simple) {
  RunTest("(aa*|bb*)+",
          "[Root]\n"
          "  [Conjunction]\n"
          "    [Repeat more than 1]\n"
          "      [Group type = CAPTURE]\n"
          "        [Alternate]\n"
          "          [Conjunction]\n"
          "            [Char 'a']\n"
          "            [Repeat more than 0]\n"
          "              [Char 'a']\n"
          "          [Conjunction]\n"
          "            [Char 'b']\n"
          "            [Repeat more than 0]\n"
          "              [Char 'b']\n");
}

TEST_F(RegExpTest, Alternate) {
  RunTest("(aa*|bb*|cc*|dd+)+",
          "[Root]\n"
          "  [Conjunction]\n"
          "    [Repeat more than 1]\n"
          "      [Group type = CAPTURE]\n"
          "        [Alternate]\n"
          "          [Conjunction]\n"
          "            [Char 'a']\n"
          "            [Repeat more than 0]\n"
          "              [Char 'a']\n"
          "          [Alternate]\n"
          "            [Conjunction]\n"
          "              [Char 'b']\n"
          "              [Repeat more than 0]\n"
          "                [Char 'b']\n"
          "            [Alternate]\n"
          "              [Conjunction]\n"
          "                [Char 'c']\n"
          "                [Repeat more than 0]\n"
          "                  [Char 'c']\n"
          "              [Conjunction]\n"
          "                [Char 'd']\n"
          "                [Repeat more than 1]\n"
          "                  [Char 'd']\n");
}

TEST_F(RegExpTest, Range) {
  RunTest("(aa*|bb*){1,3}",
          "[Root]\n"
          "  [Conjunction]\n"
          "    [RepeatRange more than 1, less than 3]\n"
          "      [Group type = CAPTURE]\n"
          "        [Alternate]\n"
          "          [Conjunction]\n"
          "            [Char 'a']\n"
          "            [Repeat more than 0]\n"
          "              [Char 'a']\n"
          "          [Conjunction]\n"
          "            [Char 'b']\n"
          "            [Repeat more than 0]\n"
          "              [Char 'b']\n");
}

TEST_F(RegExpTest, CharClass) {
  RunTest("[abcdefg,{}()?]",
          "[Root]\n"
          "  [Conjunction]\n"
          "    [CharClass exclude = false <abcdefg,{}()?>]\n");
}

TEST_F(RegExpTest, CharClassExclude) {
  RunTest("[^abcdefg,{}()?]",
          "[Root]\n"
          "  [Conjunction]\n"
          "    [CharClass exclude = true <abcdefg,{}()?>]\n");
}

TEST_F(RegExpTest, Question) {
  RunTest("(aaa*|bbb*)?",
          "[Root]\n"
          "  [Conjunction]\n"
          "    [RepeatRange more than 0, less than 1]\n"
          "      [Group type = CAPTURE]\n"
          "        [Alternate]\n"
          "          [Conjunction]\n"
          "            [CharSequence <aa>]\n"
          "            [Repeat more than 0]\n"
          "              [Char 'a']\n"
          "          [Conjunction]\n"
          "            [CharSequence <bb>]\n"
          "            [Repeat more than 0]\n"
          "              [Char 'b']\n");
}

TEST_F(RegExpTest, Uncapture) {
  RunTest("(aa*|bb*)(?:aaa)",
          "[Root]\n"
          "  [Conjunction]\n"
          "    [Group type = CAPTURE]\n"
          "      [Alternate]\n"
          "        [Conjunction]\n"
          "          [Char 'a']\n"
          "          [Repeat more than 0]\n"
          "            [Char 'a']\n"
          "        [Conjunction]\n"
          "          [Char 'b']\n"
          "          [Repeat more than 0]\n"
          "            [Char 'b']\n"
          "    [Group type = UNCAPTURE]\n"
          "      [CharSequence <aaa>]\n");
}

TEST_F(RegExpTest, PositionLookahead) {
  RunTest("(aa*|bb*)(?=aaa)",
          "[Root]\n"
          "  [Conjunction]\n"
          "    [Group type = CAPTURE]\n"
          "      [Alternate]\n"
          "        [Conjunction]\n"
          "          [Char 'a']\n"
          "          [Repeat more than 0]\n"
          "            [Char 'a']\n"
          "        [Conjunction]\n"
          "          [Char 'b']\n"
          "          [Repeat more than 0]\n"
          "            [Char 'b']\n"
          "    [Group type = POSITIVE_LOOKAHEAD]\n"
          "      [CharSequence <aaa>]\n");
}

TEST_F(RegExpTest, NegativeLookahead) {
  RunTest("(aa*|bb*)(?!aaa)",
          "[Root]\n"
          "  [Conjunction]\n"
          "    [Group type = CAPTURE]\n"
          "      [Alternate]\n"
          "        [Conjunction]\n"
          "          [Char 'a']\n"
          "          [Repeat more than 0]\n"
          "            [Char 'a']\n"
          "        [Conjunction]\n"
          "          [Char 'b']\n"
          "          [Repeat more than 0]\n"
          "            [Char 'b']\n"
          "    [Group type = NEGATIVE_LOOKAHEAD]\n"
          "      [CharSequence <aaa>]\n");
}

TEST_F(RegExpTest, GroupSpecifierName) {
  RunTest("(?<aaa>aaa)",
          "[Root]\n"
          "  [Conjunction]\n"
          "    [Group type = CAPTURE name = aaa]\n"
          "      [CharSequence <aaa>]\n");
}

TEST_F(RegExpTest, GroupParenError) {
  RunTest<true>("(aa*|bb*", nullptr, lux::SourcePosition(8, 8, 0, 0));
}

TEST_F(RegExpTest, CharClassBracketError) {
  RunTest<true>("[aaa", nullptr, lux::SourcePosition(4, 4, 0, 0));
}

TEST_F(RegExpTest, RangeRepeatBraceError) {
  RunTest<true>("aaa{1", nullptr, lux::SourcePosition(5, 5, 0, 0));
}

TEST_F(RegExpTest, RangeRepeatFirstNaNError) {
  RunTest<true>("aaa{x", nullptr, lux::SourcePosition(3, 4, 0, 0));
}

TEST_F(RegExpTest, RangeRepeatSecondNaNError) {
  RunTest<true>("aaa{1,x}", nullptr, lux::SourcePosition(5, 6, 0, 0));
}

TEST_F(RegExpTest, NothingToRepeat) {
  RunTest<true>("+", nullptr, lux::SourcePosition(1, 1, 0, 0));
}

TEST_F(RegExpTest, NothingToRepeat2) {
  RunTest<true>("*", nullptr, lux::SourcePosition(1, 1, 0, 0));
}

TEST_F(RegExpTest, NothingToRepeat3) {
  RunTest<true>("{", nullptr, lux::SourcePosition(1, 1, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_1_A1_T1) {
  RunTest<true>("a**", nullptr, lux::SourcePosition(3, 3, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_1_A1_T2) {
  RunTest<true>("a***", nullptr, lux::SourcePosition(3, 3, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_1_A1_T3) {
  RunTest<true>("a++", nullptr, lux::SourcePosition(3, 3, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_1_A1_T4) {
  RunTest<true>("a++", nullptr, lux::SourcePosition(3, 3, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_1_A1_T5) {
  RunTest<true>("a???", nullptr, lux::SourcePosition(4, 4, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_1_A1_T6) {
  RunTest<true>("a????", nullptr, lux::SourcePosition(4, 4, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_1_A1_T7) {
  RunTest<true>("*a", nullptr, lux::SourcePosition(1, 1, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_1_A1_T8) {
  RunTest<true>("**a", nullptr, lux::SourcePosition(1, 1, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_1_A1_T9) {
  RunTest<true>("+a", nullptr, lux::SourcePosition(1, 1, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_1_A1_T10) {
  RunTest<true>("++a", nullptr, lux::SourcePosition(1, 1, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_1_A1_T11) {
  RunTest<true>("?a", nullptr, lux::SourcePosition(1, 1, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_1_A1_T12) {
  RunTest<true>("??a", nullptr, lux::SourcePosition(1, 1, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_1_A1_T13) {
  RunTest<true>("x{1}{1,}", nullptr, lux::SourcePosition(5, 5, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_1_A1_T14) {
  RunTest<true>("x{1,2}{1}", nullptr, lux::SourcePosition(7, 7, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_1_A1_T15) {
  RunTest<true>("x{1,}{1}", nullptr, lux::SourcePosition(6, 6, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_1_A1_T16) {
  RunTest<true>("x{0,1}{1,}", nullptr, lux::SourcePosition(7, 7, 0, 0));
}
}  // namespace
