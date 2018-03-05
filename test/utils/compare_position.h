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

#ifndef TEST_UTILS_COMPARE_POSITION_H_
#define TEST_UTILS_COMPARE_POSITION_H_

#include "../../src/source_position.h"

namespace lux {
namespace testing {
::testing::AssertionResult CompareSourcePosition(
    const SourcePosition& actual, const SourcePosition& expected) {
  if (actual.start_col() != expected.start_col()) {
    return ::testing::AssertionFailure()
           << "start_col => "
           << "actual: " << actual.start_col()
           << " expected: " << expected.start_col();
  }
  if (actual.end_col() != expected.end_col()) {
    return ::testing::AssertionFailure() << "end_col => "
                                         << "actual: " << actual.end_col()
                                         << " expected: " << expected.end_col();
  }
  if (actual.start_line_number() != expected.start_line_number()) {
    return ::testing::AssertionFailure()
           << "start_line_number => "
           << "actual: " << actual.start_line_number()
           << " expected: " << expected.start_line_number();
  }
  if (actual.end_line_number() != expected.end_line_number()) {
    return ::testing::AssertionFailure()
           << "end_line_number => "
           << "actual: " << actual.end_line_number()
           << " expected: " << expected.end_line_number();
  }
  return ::testing::AssertionSuccess();
}
}  // namespace testing
}  // namespace lux

#endif  // TEST_UTILS_COMPARE_POSITION_H_
