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
#include "../../src/heap.h"
#include "../utils/isolate_setup.h"
#include "../../src/regexp.h"
#include "../../src/unicode.h"
#include "../../src/vm.h"

class RegExpBytecodeTest: public lux::IsolateSetup {
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
    ASSERT_TRUE(
        lux::testing::CompareNode(regexp,
                                  ret->ToString().c_str(), expectation));
    //    printf("%s\n", ret->ToString().c_str());
  }
};


namespace {
TEST_F(RegExpBytecodeTest, Simple) {
  RunTest("abc(abc)", "abcabcabc", lux::regexp::Flag::kNone,
          "JSArray[String(\"abcabc\"), String(\"abc\")]");
}

TEST_F(RegExpBytecodeTest, SimpleAlternate) {
  RunTest("(abc)|(efg)", "abcdefg", lux::regexp::Flag::kNone,
          "JSArray[String(\"abc\"), String(\"abc\")]");
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
}
