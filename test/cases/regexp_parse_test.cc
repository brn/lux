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
      auto e = *(er.begin());
      EXPECT_PRED2(
          [&](auto a, auto b) {
            return a == b ? ::testing::AssertionSuccess()
                          : ::testing::AssertionFailure()
                                << "Expected source position is " << b;
          },
          e->source_position(), source_position);
      if (show_error) {
        er.PrintError();
      }
    } else {
      ASSERT_TRUE(!er.HasPendingError()) << "Error occured " << er.ToString();
      auto a = p.node()->ToString();
      ASSERT_TRUE(lux::testing::CompareNode(regexp, a.c_str(), expected));
    }
  }
};

namespace {
TEST_F(RegExpTest, Simple) {
  RunTest("(aa*|bb*)+",
          "[Root]\n"
          "  [MatchRoot]\n"
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
          "  [MatchRoot]\n"
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
          "  [MatchRoot]\n"
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
          "  [MatchRoot]\n"
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
  RunTest("[^abcdefg,{}()?]",
          "[Root]\n"
          "  [MatchRoot]\n"
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
  RunTest("(aaa*|bbb*)?",
          "[Root]\n"
          "  [MatchRoot]\n"
          "    [RepeatRange more than 0, less than 1]\n"
          "      [Group type = CAPTURE]\n"
          "        [Alternate]\n"
          "          [Conjunction]\n"
          "            [Char 'a']\n"
          "            [Char 'a']\n"
          "            [Repeat more than 0]\n"
          "              [Char 'a']\n"
          "          [Conjunction]\n"
          "            [Char 'b']\n"
          "            [Char 'b']\n"
          "            [Repeat more than 0]\n"
          "              [Char 'b']\n");
}

TEST_F(RegExpTest, Uncapture) {
  RunTest("(aa*|bb*)(?:aaa)",
          "[Root]\n"
          "  [MatchRoot]\n"
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
          "    [Group type = SKIP_CAPTURING]\n"
          "      [CharSequence]\n"
          "        [Char 'a']\n"
          "        [Char 'a']\n"
          "        [Char 'a']\n");
}

TEST_F(RegExpTest, PositionLookahead) {
  RunTest("(aa*|bb*)(?=aaa)",
          "[Root]\n"
          "  [MatchRoot]\n"
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
          "      [CharSequence]\n"
          "        [Char 'a']\n"
          "        [Char 'a']\n"
          "        [Char 'a']\n");
}

TEST_F(RegExpTest, NegativeLookahead) {
  RunTest("(aa*|bb*)(?!aaa)",
          "[Root]\n"
          "  [MatchRoot]\n"
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
          "      [CharSequence]\n"
          "        [Char 'a']\n"
          "        [Char 'a']\n"
          "        [Char 'a']\n");
}

TEST_F(RegExpTest, GroupSpecifierName) {
  RunTest("(?<aaa>aaa)",
          "[Root]\n"
          "  [MatchRoot]\n"
          "    [Group type = CAPTURE name = aaa]\n"
          "      [CharSequence]\n"
          "        [Char 'a']\n"
          "        [Char 'a']\n"
          "        [Char 'a']\n");
}

TEST_F(RegExpTest, GroupSpecifierUnicodeName) {
  RunTest("(?<\\u0041\\u0042\\u0043\\u0044\\u0045>aaa)",
          "[Root]\n"
          "  [MatchRoot]\n"
          "    [Group type = CAPTURE name = ABCDE]\n"
          "      [CharSequence]\n"
          "        [Char 'a']\n"
          "        [Char 'a']\n"
          "        [Char 'a']\n");
}

TEST_F(RegExpTest, GroupSpecifierUnicodeBraceName) {
  RunTest("(?<\\u{0041}\\u{0042}\\u{0043}\\u{0044}\\u{0045}>aaa)",
          "[Root]\n"
          "  [MatchRoot]\n"
          "    [Group type = CAPTURE name = ABCDE]\n"
          "      [CharSequence]\n"
          "        [Char 'a']\n"
          "        [Char 'a']\n"
          "        [Char 'a']\n");
}

TEST_F(RegExpTest, UnGroupedRepeat) {
  RunTest("\\d{3}|[a-z]{4}",
          "[Root]\n"
          "  [MatchRoot]\n"
          "    [Alternate]\n"
          "      [RepeatRange more than 3, less than 3]\n"
          "        [EscapeSequence type = 'DIGIT']\n"
          "      [RepeatRange more than 4, less than 4]\n"
          "        [CharClass exclude = false]\n"
          "          [CharRange from = '97 to = 122']\n");
}

TEST_F(RegExpTest, Malicious) {
  RunTest("(aa*)?(aa*)?ac",
          "[Root]\n"
          "  [MatchRoot]\n"
          "    [RepeatRange more than 0, less than 1]\n"
          "      [Group type = CAPTURE]\n"
          "        [Repeat more than 0 type = GREEDY]\n"
          "          [Char 'a']\n"
          "    [RepeatRange more than 0, less than 1]\n"
          "      [Group type = CAPTURE]\n"
          "        [Repeat more than 0 type = GREEDY]\n"
          "          [Char 'a']\n"
          "    [CharSequence]\n"
          "      [Char 'a']\n"
          "      [Char 'c']\n");
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
  RunTest<true>("aaa{x", nullptr, lux::SourcePosition(4, 4, 0, 0));
}

TEST_F(RegExpTest, RangeRepeatSecondNaNError) {
  RunTest<true>("aaa{1,x}", nullptr, lux::SourcePosition(6, 6, 0, 0));
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
  RunTest<true>("a???", nullptr, lux::SourcePosition(3, 3, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_1_A1_T6) {
  RunTest<true>("a????", nullptr, lux::SourcePosition(3, 3, 0, 0));
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

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T1) {
  RunTest<true>("[b-ac-e]", nullptr, lux::SourcePosition(0, 4, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T2) {
  RunTest<true>("[a-dc-b]", nullptr, lux::SourcePosition(0, 7, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T3) {
  RunTest<true>("[\\\\db-G]", nullptr, lux::SourcePosition(0, 7, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T4) {
  RunTest<true>("[\\\\Db-G]", nullptr, lux::SourcePosition(0, 7, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T5) {
  RunTest<true>("[\\\\sb-G]", nullptr, lux::SourcePosition(0, 7, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T6) {
  RunTest<true>("[\\\\Sb-G]", nullptr, lux::SourcePosition(0, 7, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T7) {
  RunTest<true>("[\\\\wb-G]", nullptr, lux::SourcePosition(0, 7, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T8) {
  RunTest<true>("[\\\\Wb-G]", nullptr, lux::SourcePosition(0, 7, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T9) {
  RunTest<true>("[\\\\0b-G]", nullptr, lux::SourcePosition(0, 7, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T10) {
  RunTest<true>("[\\\\10b-G]", nullptr, lux::SourcePosition(0, 8, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T11) {
  RunTest<true>("[\\\\bd-G]", nullptr, lux::SourcePosition(0, 7, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T12) {
  RunTest<true>("[\\\\Bd-G]", nullptr, lux::SourcePosition(0, 7, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T13) {
  RunTest<true>("[\\\\td-G]", nullptr, lux::SourcePosition(0, 7, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T14) {
  RunTest<true>("[\\\\nd-G]", nullptr, lux::SourcePosition(0, 7, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T15) {
  RunTest<true>("[\\\\vd-G]", nullptr, lux::SourcePosition(0, 7, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T16) {
  RunTest<true>("[\\\\fd-G]", nullptr, lux::SourcePosition(0, 7, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T17) {
  RunTest<true>("[\\\\rd-G]", nullptr, lux::SourcePosition(0, 7, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T18) {
  RunTest<true>("[\\\\c0001d-G]", nullptr, lux::SourcePosition(0, 11, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T19) {
  RunTest<true>("[\\\\x0061d-G]", nullptr, lux::SourcePosition(0, 11, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T20) {
  RunTest<true>("[\\\\u0061d-G]", nullptr, lux::SourcePosition(0, 11, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T21) {
  RunTest<true>("[\\\\ad-G]", nullptr, lux::SourcePosition(0, 7, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T22) {
  RunTest<true>("[c-eb-a]", nullptr, lux::SourcePosition(0, 7, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T23) {
  RunTest<true>("[b-G\\\\d]", nullptr, lux::SourcePosition(0, 4, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T24) {
  RunTest<true>("[b-G\\\\D]", nullptr, lux::SourcePosition(0, 4, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T25) {
  RunTest<true>("[b-G\\\\s]", nullptr, lux::SourcePosition(0, 4, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T26) {
  RunTest<true>("[b-G\\\\S]", nullptr, lux::SourcePosition(0, 4, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T27) {
  RunTest<true>("[b-G\\\\w]", nullptr, lux::SourcePosition(0, 4, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T28) {
  RunTest<true>("[b-G\\\\W]", nullptr, lux::SourcePosition(0, 4, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T29) {
  RunTest<true>("[b-G\\\\0]", nullptr, lux::SourcePosition(0, 4, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T30) {
  RunTest<true>("[b-G\\\\10]", nullptr, lux::SourcePosition(0, 4, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T31) {
  RunTest<true>("[d-G\\\\b]", nullptr, lux::SourcePosition(0, 4, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T32) {
  RunTest<true>("[d-G\\\\B]", nullptr, lux::SourcePosition(0, 4, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T33) {
  RunTest<true>("[d-G\\\\t]", nullptr, lux::SourcePosition(0, 4, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T34) {
  RunTest<true>("[d-G\\\\n]", nullptr, lux::SourcePosition(0, 4, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T35) {
  RunTest<true>("[d-G\\\\v]", nullptr, lux::SourcePosition(0, 4, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T36) {
  RunTest<true>("[d-G\\\\f]", nullptr, lux::SourcePosition(0, 4, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T37) {
  RunTest<true>("[d-G\\\\r]", nullptr, lux::SourcePosition(0, 4, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T38) {
  RunTest<true>("[d-G\\\\c0001]", nullptr, lux::SourcePosition(0, 4, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T39) {
  RunTest<true>("[d-G\\\\x0061]", nullptr, lux::SourcePosition(0, 4, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T40) {
  RunTest<true>("[d-G\\\\u0061]", nullptr, lux::SourcePosition(0, 4, 0, 0));
}

TEST_F(RegExpTest, Test262_S15_10_2_15_A1_T41) {
  RunTest<true>("[d-G\\\\a]", nullptr, lux::SourcePosition(0, 4, 0, 0));
}
}  // namespace
