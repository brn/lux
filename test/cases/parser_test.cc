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

#include <sstream>
#include <gtest/gtest.h>
#include "../utils/compare_node.h"
#include "../utils/compare_position.h"
#include "../utils/isolate_setup.h"
#include "../../src/unicode.h"
#include "../../src/reporter.h"
#include "../../src/parser.h"

using Expectations = std::array<const char*, 3>;

class ParserTest: public lux::IsolateSetup {
 protected:
  void ParseTest(const char* envList[][2],
                 const char* code,
                 const Expectations& expectations) {
    for (int i = 0; i < 3; i++) {
      auto env = envList[i];
      if (env[0] != nullptr) {
        std::stringstream ss;
        ss << env[0] << code << env[1] << ";PARSER_SENTINEL";
        auto ret = ss.str();
        auto x = lux::Unicode::ConvertUtf8StringToUtf16String(ret.c_str());
        lux::ErrorReporter r;
        lux::Parser parser(&x, &r);
        auto exp = parser.Parse(lux::Parser::SCRIPT);
        if (exp) {
          exp >>= [&](auto expr) {
            lux::testing::CompareNode(
                expr->ToStringTree().c_str(), expectations[i]);
          };
        } else {
          FAIL() << "Parsing code '" << ret << "' failed.";
        }
      }
    }
  }

  void SyntaxErrorTest(const char* envList[][2],
                       const char* code,
                       lux::SourcePosition* source_positions,
                       bool show_error = false) {
    for (int i = 0; i < 3; i++) {
      auto env = envList[i];
      if (env[0] == nullptr) {
        continue;
      }

      std::stringstream ss;
      ss << env[0] << code << env[1] << ";PARSER_SENTINEL";
      auto ret = ss.str();
      auto x = lux::Unicode::ConvertUtf8StringToUtf16String(ret.c_str());
      lux::ErrorReporter r;
      lux::Parser parser(&x, &r);
      auto a = parser.Parse(lux::Parser::SCRIPT);
      EXPECT_TRUE(!a) << "Code '" << ret << "' not failed.";
      EXPECT_TRUE(r.HasPendingError())
        << "Code '" << ret << "' not generate error.";
      for (auto &e : r) {
        EXPECT_TRUE(lux::testing::CompareSourcePosition(e->source_position(),
                                                        source_positions[i]))
          << "Code '" << ret << "' error position is not valid";
      }
      if (show_error) {
        r.PrintError();
      }
    }
  }

  std::unique_ptr<const char*> FunctionExprWrapper(
      size_t expr_size, const char* expr) {
    std::stringstream ss;
    auto exit = 29 + expr_size;
    auto exit_expr = 12 + expr_size;
    auto func_exit = exit_expr + 2;
    ss << "[Statements [0,0,0," << exit << "]]\n"
      "  [ExpressionStatement [0,0,0," << func_exit << "]]\n"
      "    [FunctionExpression [0,0,0," << func_exit << "]]\n"
      "      [Literal type = IDENTIFIER value = X [0,0,9,9]]\n"
      "      [Expressions [0,0,10,11]]\n"
      "      [Statements [0,0,12," << exit_expr << "]]\n"
      "        [ExpressionStatement [0,0,12," << exit_expr << "]]\n"
       << "          " << expr <<
      "  [ExpressionStatement [0,0," << func_exit << "," << exit << "]]\n"
      "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,"
       << func_exit << "," << exit << "]]\n";
    auto s = ss.str();
    auto m = new char[s.size() + 2]();
    snprintf(m, s.size() + 1, "%s", s.c_str());
    return std::make_unique<const char*>(m);
  }

  void SingleExpressionTest(const char* ast_left, const char* value,
                            bool is_skip_strict_mode = false) {
    const char* env[][2] =
      {{"", ""} ,
      {(is_skip_strict_mode? nullptr: "'use strict';"), ""},
      {"function X() {", "}"}};
    auto size = strlen(value);
    std::stringstream wrapped_by_function_ss;
    wrapped_by_function_ss
      << "["  << ast_left << " [0,0,12," << 12 + size << "]]\n";
    auto wrapped_by_function = wrapped_by_function_ss.str();
    auto f = FunctionExprWrapper(size, wrapped_by_function.c_str());

    auto exit = 16 + size;
    std::stringstream normal_ss;
    normal_ss
      <<
      "[Statements [0,0,0," << exit << "]]\n"
      "  [ExpressionStatement [0,0,0," << size << "]]\n"
      "    [" << ast_left << " [0,0,0," << size << "]]\n"
      "  [ExpressionStatement [0,0," << (size + 1) << "," << exit << "]]\n"
      "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,"
                            << (size + 1) << "," << exit << "]]\n";
    auto normal = normal_ss.str();

    exit = 29 + size;
    auto expr_stmt_exit = 13 + size;
    std::stringstream strict_ss;
    strict_ss
      <<
      "[Statements [0,0,13," << exit << "]]\n"
      "  [ExpressionStatement [0,0,13," << expr_stmt_exit << "]]\n"
      "    [" << ast_left << " [0,0,13," << expr_stmt_exit << "]]\n"
      "  [ExpressionStatement [0,0," << (size + 14) << "," << exit << "]]\n"
      "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,"
                            << (size + 14) << "," << exit << "]]\n";
    auto strict = strict_ss.str();
    Expectations expected = {{
        normal.c_str(),
        strict.c_str(),
        *f
      }};
    ParseTest(env, value, expected);
  }
};

namespace {
TEST_F(ParserTest, SingleDecimalLiteral) {
  SingleExpressionTest("Literal type = NUMERIC_LITERAL value = 1", "1");
}

TEST_F(ParserTest, MultiDecimalLiteral) {
  SingleExpressionTest("Literal type = NUMERIC_LITERAL value = 1024", "1024");
}

TEST_F(ParserTest, MultiDecimalExponentLiteral) {
  SingleExpressionTest(
      "Literal type = NUMERIC_LITERAL value = 13e+10", "13e+10");
}

TEST_F(ParserTest, FloatLeadingZeroLiteral) {
  SingleExpressionTest(
      "Literal type = NUMERIC_LITERAL value = 0.12", "0.12");
}

TEST_F(ParserTest, FloatNotLeadingZeroLiteral) {
  SingleExpressionTest(
      "Literal type = NUMERIC_LITERAL value = .12", ".12");
}

TEST_F(ParserTest, HexDecimalLiteral) {
  SingleExpressionTest(
      "Literal type = NUMERIC_LITERAL value = 0xabcdef1234567890",
      "0xabcdef1234567890");
}

TEST_F(ParserTest, BinaryLiteral) {
  SingleExpressionTest(
      "Literal type = NUMERIC_LITERAL value = 0b0101",
      "0b0101");
}

TEST_F(ParserTest, OctalLiteral) {
  SingleExpressionTest(
      "Literal type = NUMERIC_LITERAL value = 0o777", "0o777");
}

TEST_F(ParserTest, ImplicitOctalLiteral) {
  SingleExpressionTest(
      "Literal type = NUMERIC_LITERAL value = 0777", "0777", true);
}

TEST_F(ParserTest, DecimalLeadingZeroLiteral) {
  SingleExpressionTest(
      "Literal type = NUMERIC_LITERAL value = 07778", "07778", true);
}

TEST_F(ParserTest, NumericLiteralError) {
  const char* env[][2] =
    {{"", ""} ,
     {"'use strict';", ""},
     {"function X() {", "}"}};
  SyntaxErrorTest(env, "0x_", lux::SourcePosition(0, 2, 0, 0));
  SyntaxErrorTest(env, "0b_", lux::SourcePosition(0, 2, 0, 0));
  SyntaxErrorTest(env, "0o_", lux::SourcePosition(0, 2, 0, 0));
  SyntaxErrorTest(env, "13e", lux::SourcePosition(0, 3, 0, 0));
  SyntaxErrorTest(env, "13e+", lux::SourcePosition(0, 4, 0, 0));
}

// TEST_F(ParserTest, ImplicitOctalError) {
//   SyntaxErrorTest("'use strict';0777",
//                             lux::SourcePosition(13, 17, 0, 0));
// }

// TEST_F(ParserTest, StringLiteral) {
//   ParseTest(
//       "'test value';PARSER_SENTINEL", 3,
//       "[Statements [0,0,0,29]]\n"
//       "  [ExpressionStatement [0,0,0,12]]\n"
//       "    [Literal type = STRING_LITERAL value = test value [0,0,0,12]]\n"
//       "  [ExpressionStatement [0,0,13,29]]\n"
//       "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,13,29]]\n");
//   ParseTest(
//       "'test \\'value';PARSER_SENTINEL", 3,
//       "[Statements [0,0,0,31]]\n"
//       "  [ExpressionStatement [0,0,0,14]]\n"
//       "    [Literal type = STRING_LITERAL value = test 'value [0,0,0,14]]\n"
//       "  [ExpressionStatement [0,0,15,31]]\n"
//       "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,15,31]]\n");

//   ParseTest(
//       "'test\\\\ value';PARSER_SENTINEL", 3,
//       "[Statements [0,0,0,31]]\n"
//       "  [ExpressionStatement [0,0,0,14]]\n"
//       "    [Literal type = STRING_LITERAL value = test\\ value [0,0,0,14]]\n"
//       "  [ExpressionStatement [0,0,15,31]]\n"
//       "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,15,31]]\n");
// }

// TEST_F(ParserTest, StringLiteralUnicodeEscapeSequence) {
//   ParseTest(
//       "'\\u0041_\\u0042_\\u0043_\\u0044';PARSER_SENTINEL", 3,
//       "[Statements [0,0,0,38]]\n"
//       "  [ExpressionStatement [0,0,0,21]]\n"
//       "    [Literal type = STRING_LITERAL value = A_B_C_D [0,0,0,21]]\n"
//       "  [ExpressionStatement [0,0,22,38]]\n"
//       "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,22,38]]\n");
//   ParseTest(
//       "'\\u{0041}_\\u{0042}_\\u{0043}_\\u{0044}';PARSER_SENTINEL", 3,
//       "[Statements [0,0,0,46]]\n"
//       "  [ExpressionStatement [0,0,0,29]]\n"
//       "    [Literal type = STRING_LITERAL value = A_B_C_D [0,0,0,29]]\n"
//       "  [ExpressionStatement [0,0,30,46]]\n"
//       "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,30,46]]\n");
// }

// TEST_F(ParserTest, StringLiteralAsciiEscapeSequence) {
//   ParseTest(
//       "'\\x41_\\x42_\\x43_\\x44';PARSER_SENTINEL", 3,
//       "[Statements [0,0,0,30]]\n"
//       "  [ExpressionStatement [0,0,0,13]]\n"
//       "    [Literal type = STRING_LITERAL value = A_B_C_D [0,0,0,13]]\n"
//       "  [ExpressionStatement [0,0,14,30]]\n"
//       "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,14,30]]\n");
// }

// TEST_F(ParserTest, UnaryExpression) {
//   ParseTest(
//       "+1;PARSER_SENTINEL", 6,
//       "[Statements [0,0,0,19]]\n"
//       "  [ExpressionStatement [0,0,0,2]]\n"
//       "    [UnaryExpression operand = OP_PLUS position = PRE [0,0,0,2]]\n"
//       "      [Literal type = NUMERIC_LITERAL value = 1 [0,0,0,2]]\n"
//       "  [ExpressionStatement [0,0,3,19]]\n"
//       "    [Literal type = IDENTIFIER value = PARSER_SENTINEL [0,0,3,19]]\n");
// }

}  // namespace
