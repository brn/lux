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
  void ParseTest(const char* code, int line, const char* expected) {
    auto x = lux::Unicode::ConvertUtf8StringToUtf16String(code);
    lux::ErrorReporter r;
    lux::Parser parser(&x, &r);
    auto exp = parser.Parse(lux::Parser::SCRIPT);
    if (exp) {
      exp >>= [&](auto expr) {
        lux::testing::CompareNode(line, expr->ToStringTree().c_str(), expected);
      };
    } else {
      FAIL() << "Parsing code '" << code << "' failed.";
    }
  }

  void SyntaxErrorTest(const char* code,
                       lux::SourcePosition source_position,
                       bool show_error = false) {
    auto x = lux::Unicode::ConvertUtf8StringToUtf16String(code);
    lux::ErrorReporter r;
    lux::Parser parser(&x, &r);
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
  ParseTest(
      "1;PARSER_SENTINEL", 5,
      "[Statements [0,0,0,18]]\n"
      "  [ExpressionStatement [0,0,0,1]]\n"
      "    [Literal type = NUMERIC_LITERAL value = 1 [0,0,0,1]]\n"
      "  [ExpressionStatement [0,0,2,18]]\n"
      "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,2,18]]\n");
  ParseTest(
      "1024;PARSER_SENTINEL", 5,
      "[Statements [0,0,0,21]]\n"
      "  [ExpressionStatement [0,0,0,4]]\n"
      "    [Literal type = NUMERIC_LITERAL value = 1024 [0,0,0,4]]\n"
      "  [ExpressionStatement [0,0,5,21]]\n"
      "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,5,21]]\n");
  ParseTest(
      "13e+10;PARSER_SENTINEL", 5,
      "[Statements [0,0,0,23]]\n"
      "  [ExpressionStatement [0,0,0,6]]\n"
      "    [Literal type = NUMERIC_LITERAL value = 13e+10 [0,0,0,6]]\n"
      "  [ExpressionStatement [0,0,7,23]]\n"
      "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,7,23]]\n");
  ParseTest(
      "0.12;PARSER_SENTINEL", 5,
      "[Statements [0,0,0,21]]\n"
      "  [ExpressionStatement [0,0,0,4]]\n"
      "    [Literal type = NUMERIC_LITERAL value = 0.12 [0,0,0,4]]\n"
      "  [ExpressionStatement [0,0,5,21]]\n"
      "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,5,21]]\n");
  ParseTest(
      ".12;PARSER_SENTINEL", 5,
      "[Statements [0,0,0,20]]\n"
      "  [ExpressionStatement [0,0,0,3]]\n"
      "    [Literal type = NUMERIC_LITERAL value = .12 [0,0,0,3]]\n"
      "  [ExpressionStatement [0,0,4,20]]\n"
      "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,4,20]]\n");
  ParseTest(
      "0xabcdef1234567890;PARSER_SENTINEL", 5,
      "[Statements [0,0,0,35]]\n"
      "  [ExpressionStatement [0,0,0,18]]\n"
      "    [Literal type = NUMERIC_LITERAL value = 0xabcdef1234567890 "
      "[0,0,0,18]]\n"
      "  [ExpressionStatement [0,0,19,35]]\n"
      "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,19,35]]\n");
  ParseTest(
      "0b0101;PARSER_SENTINEL", 5,
      "[Statements [0,0,0,23]]\n"
      "  [ExpressionStatement [0,0,0,6]]\n"
      "    [Literal type = NUMERIC_LITERAL value = 0b0101 [0,0,0,6]]\n"
      "  [ExpressionStatement [0,0,7,23]]\n"
      "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,7,23]]\n");
  ParseTest(
      "0o777;PARSER_SENTINEL", 5,
      "[Statements [0,0,0,22]]\n"
      "  [ExpressionStatement [0,0,0,5]]\n"
      "    [Literal type = NUMERIC_LITERAL value = 0o777 [0,0,0,5]]\n"
      "  [ExpressionStatement [0,0,6,22]]\n"
      "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,6,22]]\n");
  ParseTest(
      "0777;PARSER_SENTINEL", 5,
      "[Statements [0,0,0,21]]\n"
      "  [ExpressionStatement [0,0,0,4]]\n"
      "    [Literal type = NUMERIC_LITERAL value = 0777 [0,0,0,4]]\n"
      "  [ExpressionStatement [0,0,5,21]]\n"
      "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,5,21]]\n");
  ParseTest(
      "0788;PARSER_SENTINEL", 5,
      "[Statements [0,0,0,21]]\n"
      "  [ExpressionStatement [0,0,0,4]]\n"
      "    [Literal type = NUMERIC_LITERAL value = 0788 [0,0,0,4]]\n"
      "  [ExpressionStatement [0,0,5,21]]\n"
      "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,5,21]]\n");
}

TEST_F(ParserTest, NumericLiteralError) {
  SyntaxErrorTest("0x_", lux::SourcePosition(0, 2, 0, 0));
  SyntaxErrorTest("0b_", lux::SourcePosition(0, 2, 0, 0));
  SyntaxErrorTest("0o_", lux::SourcePosition(0, 2, 0, 0));
  SyntaxErrorTest("13e", lux::SourcePosition(0, 3, 0, 0));
  SyntaxErrorTest("13e+", lux::SourcePosition(0, 4, 0, 0));
}

TEST_F(ParserTest, ImplicitOctalError) {
  SyntaxErrorTest("'use strict';0777",
                            lux::SourcePosition(13, 17, 0, 0));
}

TEST_F(ParserTest, StringLiteral) {
  ParseTest(
      "'test value';PARSER_SENTINEL", 3,
      "[Statements [0,0,0,29]]\n"
      "  [ExpressionStatement [0,0,0,12]]\n"
      "    [Literal type = STRING_LITERAL value = test value [0,0,0,12]]\n"
      "  [ExpressionStatement [0,0,13,29]]\n"
      "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,13,29]]\n");
  ParseTest(
      "'test \\'value';PARSER_SENTINEL", 3,
      "[Statements [0,0,0,31]]\n"
      "  [ExpressionStatement [0,0,0,14]]\n"
      "    [Literal type = STRING_LITERAL value = test 'value [0,0,0,14]]\n"
      "  [ExpressionStatement [0,0,15,31]]\n"
      "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,15,31]]\n");

  ParseTest(
      "'test\\\\ value';PARSER_SENTINEL", 3,
      "[Statements [0,0,0,31]]\n"
      "  [ExpressionStatement [0,0,0,14]]\n"
      "    [Literal type = STRING_LITERAL value = test\\ value [0,0,0,14]]\n"
      "  [ExpressionStatement [0,0,15,31]]\n"
      "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,15,31]]\n");
}

TEST_F(ParserTest, StringLiteralUnicodeEscapeSequence) {
  ParseTest(
      "'\\u0041_\\u0042_\\u0043_\\u0044';PARSER_SENTINEL", 3,
      "[Statements [0,0,0,38]]\n"
      "  [ExpressionStatement [0,0,0,21]]\n"
      "    [Literal type = STRING_LITERAL value = A_B_C_D [0,0,0,21]]\n"
      "  [ExpressionStatement [0,0,22,38]]\n"
      "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,22,38]]\n");
  ParseTest(
      "'\\u{0041}_\\u{0042}_\\u{0043}_\\u{0044}';PARSER_SENTINEL", 3,
      "[Statements [0,0,0,46]]\n"
      "  [ExpressionStatement [0,0,0,29]]\n"
      "    [Literal type = STRING_LITERAL value = A_B_C_D [0,0,0,29]]\n"
      "  [ExpressionStatement [0,0,30,46]]\n"
      "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,30,46]]\n");
}

TEST_F(ParserTest, StringLiteralAsciiEscapeSequence) {
  ParseTest(
      "'\\x41_\\x42_\\x43_\\x44';PARSER_SENTINEL", 3,
      "[Statements [0,0,0,30]]\n"
      "  [ExpressionStatement [0,0,0,13]]\n"
      "    [Literal type = STRING_LITERAL value = A_B_C_D [0,0,0,13]]\n"
      "  [ExpressionStatement [0,0,14,30]]\n"
      "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,14,30]]\n");
}

TEST_F(ParserTest, UnaryExpression) {
  ParseTest(
      "+1;PARSER_SENTINEL", 3,
      "[Statements [0,0,0,19]]\n"
      "  [ExpressionStatement [0,0,0,2]]\n"
      "    [UnaryExpression operand = OP_PLUS position = PRE [0,0,0,2]]\n"
      "      [Literal type = NUMERIC_LITERAL value = 1 [0,0,0,2]]\n"
      "  [ExpressionStatement [0,0,3,19]]\n"
      "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,3,19]]\n");
}
}  // namespace
