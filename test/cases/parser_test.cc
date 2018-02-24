/**
 * The MIT License (MIT)
 * Copyright (c) Taketoshi Aono
 *  
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *  
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *  
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * @fileoverview 
 * @author Taketoshi Aono
 */

#include <gtest/gtest.h>
#include "../utils/compare_node.h"
#include "../utils/compare_position.h"
#include "../utils/isolate_setup.h"
#include "../../src/unicode.h"
#include "../../src/reporter.h"
#include "../../src/parser.h"

class ParserTest: public lux::IsolateSetup {
 protected:
  void ExpressionTest(const char* code, int line, const char* expected) {
    auto x = lux::Unicode::ConvertUtf8StringToUtf16String(code);
    lux::ErrorReporter r;
    lux::Parser parser(&x, &r);
    parser.ParseDirectivePrologue();
    auto exp = parser.Parse(lux::Parser::SCRIPT);
    if (exp) {
      exp >>= [&](auto expr) {
        lux::testing::CompareNode(line, expr->ToStringTree().c_str(), expected);
      };
    } else {
      FAIL() << "Parsing code '" << code << "' failed.";
    }
  }

  void ExpressionSyntaxErrorTest(const char* code,
                                 lux::SourcePosition source_position,
                                 bool show_error = false) {
    auto x = lux::Unicode::ConvertUtf8StringToUtf16String(code);
    lux::ErrorReporter r;
    lux::Parser parser(&x, &r);
    parser.ParseDirectivePrologue();
    auto a = parser.Parse(lux::Parser::SCRIPT);
    EXPECT_TRUE(!a) << "Code '" << code << "' not failed.";
    EXPECT_TRUE(r.HasPendingError())
      << "Code '" << code << "' not generate error.";
    for (auto &e : r) {
      EXPECT_TRUE(lux::testing::CompareSourcePosition(e->source_position(),
                                                      source_position))
        << "Code '" << code << "' error position is not valid";
    }
    if (show_error) {
      r.PrintError();
    }
  }
};

namespace {
TEST_F(ParserTest, NumericLiteral) {
  ExpressionTest("1", 1,
                 "[Statements]\n"
                 "  [ExpressionStatement]\n"
                 "    [Literal type = NUMERIC_LITERAL value = 1]\n");
  ExpressionTest("1024", 1,
                 "[Statements]\n"
                 "  [ExpressionStatement]\n"
                 "    [Literal type = NUMERIC_LITERAL value = 1024]\n");
  ExpressionTest(
      "13e+10", 1,
      "[Statements]\n"
      "  [ExpressionStatement]\n"
      "    [Literal type = NUMERIC_LITERAL value = 13e+10]\n");
  ExpressionTest("0.12", 1,
                 "[Statements]\n"
                 "  [ExpressionStatement]\n"
                 "    [Literal type = NUMERIC_LITERAL value = 0.12]\n");
  ExpressionTest(".12", 1,
                 "[Statements]\n"
                 "  [ExpressionStatement]\n"
                 "    [Literal type = NUMERIC_LITERAL value = .12]\n");
  ExpressionTest(
      "0xabcdef1234567890", 1,
      "[Statements]\n"
      "  [ExpressionStatement]\n"
      "    [Literal type = NUMERIC_LITERAL value = 0xabcdef1234567890]\n");
  ExpressionTest("0b0101", 1,
                 "[Statements]\n"
                 "  [ExpressionStatement]\n"
                 "    [Literal type = NUMERIC_LITERAL value = 0b0101]\n");
  ExpressionTest("0o777", 1,
                 "[Statements]\n"
                 "  [ExpressionStatement]\n"
                 "    [Literal type = NUMERIC_LITERAL value = 0o777]\n");
  ExpressionTest("0777", 1,
                 "[Statements]\n"
                 "  [ExpressionStatement]\n"
                 "    [Literal type = NUMERIC_LITERAL value = 0777]\n");
  ExpressionTest("0788", 1,
                 "[Statements]\n"
                 "  [ExpressionStatement]\n"
                 "    [Literal type = NUMERIC_LITERAL value = 0788]\n");
}

TEST_F(ParserTest, NumericLiteralError) {
  ExpressionSyntaxErrorTest("0x_", lux::SourcePosition(2, 3, 0, 0));
  ExpressionSyntaxErrorTest("0b_", lux::SourcePosition(2, 3, 0, 0));
  ExpressionSyntaxErrorTest("0o_", lux::SourcePosition(2, 3, 0, 0));
  ExpressionSyntaxErrorTest("13e", lux::SourcePosition(2, 4, 0, 0));
  ExpressionSyntaxErrorTest("13e+", lux::SourcePosition(2, 5, 0, 0));
}

TEST_F(ParserTest, ImplicitOctalError) {
  ExpressionSyntaxErrorTest("'use strict';0777",
                            lux::SourcePosition(14, 19, 0, 0));
}
}  // namespace
