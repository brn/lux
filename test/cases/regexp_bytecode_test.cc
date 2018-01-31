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

class RegExpNFATest: public lux::IsolateSetup {
 protected:
  template <bool error = false, bool show_error = false>
  void RunTest(const char* regexp) {
    lux::ErrorReporter er;
    lux::SourcePosition sp;
    auto s = lux::Unicode::ConvertUtf8StringToUtf16String(regexp);
    lux::regexp::Parser p(&er, &sp, s);
    p.Parse();
    lux::ZoneAllocator z;
    lux::BytecodeBuilder b(isolate_, &z);
    lux::regexp::Visitor v(&b, &z);
    p.node()->Visit(&v, nullptr);
    auto arr = b.flush();
    printf("%s\n", arr->ToString().c_str());
  }
};


namespace {
TEST_F(RegExpNFATest, Simple) {
  RunTest("(aa*|bb*)+");
}
}
