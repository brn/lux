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
#include "../../src/heap.h"
#include "../../src/objects/jsobject.h"
#include "../../src/platform/os.h"
#include "../../src/regexp.h"
#include "../../src/unicode.h"
#include "../../src/vm.h"
#include "../utils/compare_node.h"
#include "../utils/isolate_setup.h"

int constexpr Length(const char* str) { return *str ? 1 + Length(str + 1) : 0; }

class RegExpBytecodeTest : public lux::IsolateSetup {
 protected:
  template <bool error = false, bool show_error = false>
  void RunTest(const char* regexp, const char* input, uint8_t flag,
               const char* expectation) {
    lux::HandleScope scope;
    lux::ErrorReporter er;
    lux::SourcePosition sp;
    lux::regexp::Compiler compiler(isolate_, &er, &sp);
    auto jsregexp = compiler.Compile(regexp, flag);
    printf("%s\n", jsregexp->code()->ToString().c_str());
    auto ret = jsregexp->Match(isolate_, *lux::JSString::New(isolate_, input));
    ASSERT_TRUE(lux::testing::CompareNode(regexp, ret->ToString().c_str(),
                                          expectation));
    //    printf("%s\n", ret->ToString().c_str());
  }

  void NotMatch(const char* regexp, const char* input, uint8_t flag) {
    lux::HandleScope scope;
    lux::ErrorReporter er;
    lux::SourcePosition sp;
    lux::regexp::Compiler compiler(isolate_, &er, &sp);
    auto jsregexp = compiler.Compile(regexp, flag);
    printf("%s\n", jsregexp->code()->ToString().c_str());
    auto ret = jsregexp->Match(isolate_, *lux::JSString::New(isolate_, input));
    ASSERT_TRUE(lux::JSSpecials::IsNull(ret)) << "Got " << ret->ToString();
  }
};

namespace {
TEST_F(RegExpBytecodeTest, Simple) {
  RunTest("abc(abc)", "abcabcabc", lux::regexp::Flag::kNone,
          "JSArray[String(\"abcabc\"), String(\"abc\")]");
}

TEST_F(RegExpBytecodeTest, SimpleAlternate) {
  RunTest("(abc)|(efg)", "abcdefg", lux::regexp::Flag::kNone,
          "JSArray[String(\"abc\"), String(\"abc\"), undefined]");
}

TEST_F(RegExpBytecodeTest, SimpleAlternateGlobal) {
  RunTest("(abc)|(efg)", "abcdefg", lux::regexp::Flag::kGlobal,
          "JSArray[String(\"abc\"), String(\"efg\")]");
}

TEST_F(RegExpBytecodeTest, SimpleAlternateGlobal2) {
  RunTest("(abc)|(efg)", "abcdefgdabc", lux::regexp::Flag::kGlobal,
          "JSArray[String(\"abc\"), String(\"efg\"), String(\"abc\")]");
}

TEST_F(RegExpBytecodeTest, SimpleCharClass) {
  RunTest("([ae][bf][cg])", "abcdefgdabc", lux::regexp::Flag::kNone,
          "JSArray[String(\"abc\"), String(\"abc\")]");
}

TEST_F(RegExpBytecodeTest, SimpleCharClassGlobal) {
  RunTest("([ae][bf][cg])", "abcdefgdabc", lux::regexp::Flag::kGlobal,
          "JSArray[String(\"abc\"), String(\"efg\"), String(\"abc\")]");
}

TEST_F(RegExpBytecodeTest, SimpleCharClassGlobal2) {
  RunTest("([ae][bx][cg])", "abcdefgdabc", lux::regexp::Flag::kGlobal,
          "JSArray[String(\"abc\"), String(\"abc\")]");
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_10_A1_1_T1) {
  RunTest("\\t", "	", lux::regexp::Flag::kNone,
          "JSArray[String(\"	\")]");
  RunTest("\\t\\t", "a		b", lux::regexp::Flag::kNone,
          "JSArray[String(\"		\")]");
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_10_A1_2_T1) {
  RunTest("\\n", "\n", lux::regexp::Flag::kNone, "JSArray[String(\"\n\")]");
  RunTest("\\n\\n", "a\n\nb", lux::regexp::Flag::kNone,
          "JSArray[String(\"\n\n\")]");
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_10_A1_3_T1) {
  RunTest("\\v", "", lux::regexp::Flag::kNone, "JSArray[String(\"\")]");
  RunTest("\\v\\v", "ab", lux::regexp::Flag::kNone,
          "JSArray[String(\"\")]");
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_10_A1_4_T1) {
  RunTest("\\f", "", lux::regexp::Flag::kNone, "JSArray[String(\"\")]");
  RunTest("\\f\\f", "ab", lux::regexp::Flag::kNone,
          "JSArray[String(\"\")]");
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_10_A1_5_T1) {
  RunTest("\\r", "\r", lux::regexp::Flag::kNone, "JSArray[String(\"\r\")]");
  RunTest("\\r\\r", "a\r\rb", lux::regexp::Flag::kNone,
          "JSArray[String(\"\r\r\")]");
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_10_A2_1_T1) {
  for (int i = 0x0041; i <= 0x005a; i++) {
    auto alpha = i;
    std::string str;
    lux::SPrintf(&str, false, "%c", alpha % 32);
    std::string reg;
    lux::SPrintf(&reg, false, "\\c%c", alpha);
    std::string result;
    printf("%d\n", str.at(0));
    lux::SPrintf(&result, false, "JSArray[String(\"%s\")]", str.c_str());
    RunTest(reg.c_str(), str.c_str(), lux::regexp::Flag::kNone, result.c_str());
  }
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_10_A2_1_T2) {
  for (int i = 0x0061; i <= 0x007A; i++) {
    auto alpha = i;
    std::string str;
    lux::SPrintf(&str, false, "%c", alpha % 32);
    std::string reg;
    lux::SPrintf(&reg, false, "\\c%c", alpha);
    std::string result;
    printf("%d\n", str.at(0));
    lux::SPrintf(&result, false, "JSArray[String(\"%s\")]", str.c_str());
    RunTest(reg.c_str(), str.c_str(), lux::regexp::Flag::kNone, result.c_str());
  }
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_10_A3_1_T1) {
  RunTest("\\x01", u8"'\U00000001'", lux::regexp::Flag::kNone,
          u8"JSArray[String(\"\U00000001\")]");
  RunTest("\\x0A", u8"'\U0000000A'", lux::regexp::Flag::kNone,
          u8"JSArray[String(\"\U0000000A\")]");
  RunTest("\\xFF", u8"'\U000000FF'", lux::regexp::Flag::kNone,
          u8"JSArray[String(\"\U000000FF\")]");
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_10_A3_1_T2) {
  static const std::array<const char*, 26> kHex = {
      {"\\x41", "\\x42", "\\x43", "\\x44", "\\x45", "\\x46", "\\x47",
       "\\x48", "\\x49", "\\x4A", "\\x4B", "\\x4C", "\\x4D", "\\x4E",
       "\\x4F", "\\x50", "\\x51", "\\x52", "\\x53", "\\x54", "\\x55",
       "\\x56", "\\x57", "\\x58", "\\x59", "\\x5A"}};
  static const std::array<const char*, 26> kCharacter = {
      {"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
       "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"}};

  for (int i = 0; i < kHex.size(); i++) {
    std::stringstream ss;
    ss << "JSArray[String(\"" << kCharacter[i] << "\")]";
    std::string s = ss.str();
    RunTest(kHex[i], kCharacter[i], lux::regexp::Flag::kNone, s.c_str());
  }
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_10_A3_1_T2_2) {
  static const std::array<const char*, 26> kHex = {
      {"\\x61", "\\x62", "\\x63", "\\x64", "\\x65", "\\x66", "\\x67",
       "\\x68", "\\x69", "\\x6A", "\\x6B", "\\x6C", "\\x6D", "\\x6E",
       "\\x6F", "\\x70", "\\x71", "\\x72", "\\x73", "\\x74", "\\x75",
       "\\x76", "\\x77", "\\x78", "\\x79", "\\x7A"}};
  static const std::array<const char*, 26> kCharacter = {
      {"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
       "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"}};

  for (int i = 0; i < kHex.size(); i++) {
    std::stringstream ss;
    ss << "JSArray[String(\"" << kCharacter[i] << "\")]";
    std::string s = ss.str();
    RunTest(kHex[i], kCharacter[i], lux::regexp::Flag::kNone, s.c_str());
  }
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_10_A4_1_T1) {
  RunTest("\\u0001", u8"'\U00000001'", lux::regexp::Flag::kNone,
          u8"JSArray[String(\"\U00000001\")]");
  RunTest("\\u000A", u8"'\U0000000A'", lux::regexp::Flag::kNone,
          u8"JSArray[String(\"\U0000000A\")]");
  RunTest("\\u00FF", u8"'\U000000FF'", lux::regexp::Flag::kNone,
          u8"JSArray[String(\"\U000000FF\")]");
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_10_A4_1_T2) {
  static const std::array<const char*, 26> kHex = {
      {"\\u0041", "\\u0042", "\\u0043", "\\u0044", "\\u0045", "\\u0046",
       "\\u0047", "\\u0048", "\\u0049", "\\u004A", "\\u004B", "\\u004C",
       "\\u004D", "\\u004E", "\\u004F", "\\u0050", "\\u0051", "\\u0052",
       "\\u0053", "\\u0054", "\\u0055", "\\u0056", "\\u0057", "\\u0058",
       "\\u0059", "\\u005A"}};
  static const std::array<const char*, 26> kCharacter = {
      {"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
       "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"}};

  for (int i = 0; i < kHex.size(); i++) {
    std::stringstream ss;
    ss << "JSArray[String(\"" << kCharacter[i] << "\")]";
    std::string s = ss.str();
    RunTest(kHex[i], kCharacter[i], lux::regexp::Flag::kNone, s.c_str());
  }
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_10_A4_1_T2_2) {
  static const std::array<const char*, 26> kHex = {
      {"\\u0061", "\\u0062", "\\u0063", "\\u0064", "\\u0065", "\\u0066",
       "\\u0067", "\\u0068", "\\u0069", "\\u006A", "\\u006B", "\\u006C",
       "\\u006D", "\\u006E", "\\u006F", "\\u0070", "\\u0071", "\\u0072",
       "\\u0073", "\\u0074", "\\u0075", "\\u0076", "\\u0077", "\\u0078",
       "\\u0079", "\\u007A"}};
  static const std::array<const char*, 26> kCharacter = {
      {"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
       "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"}};

  for (int i = 0; i < kHex.size(); i++) {
    std::stringstream ss;
    ss << "JSArray[String(\"" << kCharacter[i] << "\")]";
    std::string s = ss.str();
    RunTest(kHex[i], kCharacter[i], lux::regexp::Flag::kNone, s.c_str());
  }
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_10_A4_1_T3) {
  static const std::array<const char*, 33> kHex = {
      {"\\u0410", "\\u0411", "\\u0412", "\\u0413", "\\u0414", "\\u0415",
       "\\u0416", "\\u0417", "\\u0418", "\\u0419", "\\u041A", "\\u041B",
       "\\u041C", "\\u041D", "\\u041E", "\\u041F", "\\u0420", "\\u0421",
       "\\u0422", "\\u0423", "\\u0424", "\\u0425", "\\u0426", "\\u0427",
       "\\u0428", "\\u0429", "\\u042A", "\\u042B", "\\u042C", "\\u042D",
       "\\u042E", "\\u042F", "\\u0401"}};
  static const std::array<const char*, 39> kCharacter = {
      {u8"\U00000410", u8"\U00000411", u8"\U00000412", u8"\U00000413",
       u8"\U00000414", u8"\U00000415", u8"\U00000416", u8"\U00000417",
       u8"\U00000418", u8"\U00000419", u8"\U0000041A", u8"\U0000041B",
       u8"\U0000041C", u8"\U0000041D", u8"\U0000041E", u8"\U0000041F",
       u8"\U00000420", u8"\U00000421", u8"\U00000422", u8"\U00000423",
       u8"\U00000424", u8"\U00000425", u8"\U00000426", u8"\U00000427",
       u8"\U00000428", u8"\U00000429", u8"\U0000042A", u8"\U0000042B",
       u8"\U0000042C", u8"\U0000042D", u8"\U0000042E", u8"\U0000042F",
       u8"\U00000401"}};

  for (int i = 0; i < kHex.size(); i++) {
    std::stringstream ss;
    ss << "JSArray[String(\"" << kCharacter[i] << "\")]";
    std::string s = ss.str();
    RunTest(kHex[i], kCharacter[i], lux::regexp::Flag::kNone, s.c_str());
  }
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_10_A4_1_T3_2) {
  static const std::array<const char*, 33> kHex = {
      {"\\u0430", "\\u0431", "\\u0432", "\\u0433", "\\u0434", "\\u0435",
       "\\u0436", "\\u0437", "\\u0438", "\\u0439", "\\u043A", "\\u043B",
       "\\u043C", "\\u043D", "\\u043E", "\\u043F", "\\u0440", "\\u0441",
       "\\u0442", "\\u0443", "\\u0444", "\\u0445", "\\u0446", "\\u0447",
       "\\u0448", "\\u0449", "\\u044A", "\\u044B", "\\u044C", "\\u044D",
       "\\u044E", "\\u044F", "\\u0451"}};
  static const std::array<const char*, 39> kCharacter = {
      {u8"\U00000430", u8"\U00000431", u8"\U00000432", u8"\U00000433",
       u8"\U00000434", u8"\U00000435", u8"\U00000436", u8"\U00000437",
       u8"\U00000438", u8"\U00000439", u8"\U0000043A", u8"\U0000043B",
       u8"\U0000043C", u8"\U0000043D", u8"\U0000043E", u8"\U0000043F",
       u8"\U00000440", u8"\U00000441", u8"\U00000442", u8"\U00000443",
       u8"\U00000444", u8"\U00000445", u8"\U00000446", u8"\U00000447",
       u8"\U00000448", u8"\U00000449", u8"\U0000044A", u8"\U0000044B",
       u8"\U0000044C", u8"\U0000044D", u8"\U0000044E", u8"\U0000044F",
       u8"\U00000451"}};

  for (int i = 0; i < kHex.size(); i++) {
    std::stringstream ss;
    ss << "JSArray[String(\"" << kCharacter[i] << "\")]";
    std::string s = ss.str();
    RunTest(kHex[i], kCharacter[i], lux::regexp::Flag::kNone, s.c_str());
  }
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_10_A5_1_T1) {
  static constexpr const char* kNonIdent = "~`!@#$%^&*()-+={[}]|\\:;'<,>./?";
  static constexpr const size_t N = Length(kNonIdent);
  for (int i = 0; i < N; i++) {
    std::stringstream mss;
    mss << "\\" << kNonIdent[i];
    std::string ms = mss.str();

    std::stringstream tss;
    tss << kNonIdent[i];
    std::string ts = tss.str();

    std::stringstream ss;
    ss << "JSArray[String(\"" << kNonIdent[i] << "\")]";
    std::string s = ss.str();
    RunTest(ms.c_str(), ts.c_str(), lux::regexp::Flag::kGlobal, s.c_str());
  }
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_11_A1_1_T4) {
  RunTest("(A)\\1", "AA", lux::regexp::Flag::kNone,
          u8"JSArray[String(\"AA\"), String(\"A\")]");
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_11_A1_1_T5) {
  RunTest("\\1(A)", "AA", lux::regexp::Flag::kNone,
          u8"JSArray[String(\"A\"), String(\"A\")]");
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_11_A1_1_T6) {
  RunTest("(A)\\1(B)\\2", "AABB", lux::regexp::Flag::kNone,
          u8"JSArray[String(\"AABB\"), String(\"A\"), String(\"B\")]");
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_11_A1_1_T7) {
  RunTest("\\1(A)(B)\\2", "ABB", lux::regexp::Flag::kNone,
          u8"JSArray[String(\"ABB\"), String(\"A\"), String(\"B\")]");
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_11_A1_1_T8) {
  RunTest("((((((((((A))))))))))\\1\\2\\3\\4\\5\\6\\7\\8\\9\\10", "AAAAAAAAAAA",
          lux::regexp::Flag::kNone,
          "JSArray[String(\"AAAAAAAAAAA\"), String(\"A\"), String(\"A\"), "
          "String(\"A\"), String(\"A\"), String(\"A\"), String(\"A\"), "
          "String(\"A\"), String(\"A\"), String(\"A\"), String(\"A\")]");
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_11_A1_1_T9) {
  RunTest("((((((((((A))))))))))\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1", "AAAAAAAAAAA",
          lux::regexp::Flag::kNone,
          "JSArray[String(\"AAAAAAAAAAA\"), String(\"A\"), String(\"A\"), "
          "String(\"A\"), String(\"A\"), String(\"A\"), String(\"A\"), "
          "String(\"A\"), String(\"A\"), String(\"A\"), String(\"A\")]");
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_11_A3_T5) {
  NotMatch("\\w", "\f\n\r\t\v~`!@#$%^&*()-+={[}]|\\:;'<,>./? ",
           lux::regexp::Flag::kNone);
  RunTest("\\w+",
          "_0123456789_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
          lux::regexp::Flag::kNone,
          "JSArray[String(\"_0123456789_"
          "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ\")]");
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_11_A4_T5) {
  RunTest("\\W+", "\f\n\r\t\v~`!@#$%^&*()-+={[}]|\\:;'<,>./?\" ",
          lux::regexp::Flag::kNone,
          "JSArray[String(\"\f\n\r\t\v~`!@#$%^&*()-+={[}]|\\:;'<,>./?\" \")]");
  NotMatch("\\W",
           "_0123456789_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
           lux::regexp::Flag::kNone);
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_13_A1_T1) {
  NotMatch("[]a", "0a0a", lux::regexp::Flag::kNone);
}

TEST_F(RegExpBytecodeTest, Test262_S15_10_2_13_A1_T10) {
  RunTest("[a-c\\d]+", "\n\n\abc324234\n", lux::regexp::Flag::kNone,
          "JSArray[String(\"abc324234\")]");
}

}  // namespace
