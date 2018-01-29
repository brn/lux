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
#include <string.h>
#include "../../utils/isolate_setup.h"
#include "../../../src/utils.h"
#include "../../../src/objects/jsobject.h"

namespace {
class JSStringTest: public lux::IsolateSetup {};

static constexpr const char* value = "test value";
static size_t length = strlen(value);

TEST_F(JSStringTest, Base) {
  lux::HandleScope scope;
  auto jsstr = lux::JSString::New(isolate_, value);
  ASSERT_EQ(jsstr->length(), length);
}

TEST_F(JSStringTest, Utf8Value) {
  lux::HandleScope scope;
  auto jsstr = lux::JSString::New(isolate_, value);
  lux::JSString::Utf8String utf8(*jsstr);
  ASSERT_EQ(strcmp(utf8.buffer(), value), 0);
}

}  // namespace
