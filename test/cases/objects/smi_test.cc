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
#include "../../utils/isolate_setup.h"
#include "../../../src/utils.h"
#include "../../../src/objects/object.h"

namespace {

class SmiTest: public lux::IsolateSetup {};

TEST_F(SmiTest, SimpleTest) {
  auto smi = lux::Smi::FromInt(100);
  ASSERT_EQ(smi->raw_value(), 200);
  ASSERT_EQ(smi->value(), 100);
}

TEST_F(SmiTest, MaxValueTest) {
  ASSERT_TRUE(!lux::Smi::IsFit(~lux::smi_t(0)));
  ASSERT_TRUE(lux::Smi::IsFit(~lux::smi_t(0) >> 1));
}

TEST_F(SmiTest, Cast) {
  auto smi = lux::Smi::FromInt(100);
  lux::Object* o = smi;
  ASSERT_TRUE(o->IsSmi());
  ASSERT_TRUE(lux::Object::IsSmi(smi));
}

}  // namespace
