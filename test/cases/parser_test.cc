/**
 * The MIT License (MIT)
 * Copyright (c) Taketoshi Aono
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 * @fileoverview
 * @author Taketoshi Aono
 */

#include "../../src/parser.h"
#include <gtest/gtest.h>
#include <sstream>
#include "../../src/reporter.h"
#include "../../src/unicode.h"
#include "../utils/compare_node.h"
#include "../utils/compare_position.h"
#include "../utils/isolate_setup.h"

using Expectations = std::array<const char*, 3>;

class TestableAst {
 public:
  TestableAst(const char* name, const char* attr, lux::SourcePosition pos,
              std::initializer_list<TestableAst> children = {})
      : name_(name), attr_(attr), pos_(pos), children_(children) {}

  TestableAst& operator<<(const TestableAst& ast) {
    children_.push_back(ast);
    return *this;
  }

  std::string ToString() const {
    std::string i("");
    return ToString(&i);
  }

 private:
  std::string ToString(std::string* indent) const {
    std::stringstream ss;
    int has_attr = attr_.size();
    ss << *indent << "[" << name_ << " ";
    if (has_attr) {
      ss << attr_ << " ";
    }
    ss << pos_.ToString() << "]\n";
    auto ni = (*indent) + "  ";
    for (auto& child : children_) {
      ss << child.ToString(&ni);
    }
    return ss.str();
  }

  std::string name_;
  std::string attr_;
  lux::SourcePosition pos_;
  std::vector<TestableAst> children_;
};

inline TestableAst Ast(const char* name, const char* attr,
                       lux::SourcePosition sp,
                       std::initializer_list<TestableAst> ast = {}) {
  return TestableAst(name, attr, sp, ast);
}

inline TestableAst Stmts(lux::SourcePosition sp,
                         std::initializer_list<TestableAst> ast = {}) {
  return Ast("Statements", "", sp, ast);
}

inline TestableAst ExprStmt(lux::SourcePosition sp,
                            std::initializer_list<TestableAst> ast = {}) {
  return Ast("ExpressionStatement", "", sp, ast);
}

inline TestableAst Exprs(lux::SourcePosition sp,
                         std::initializer_list<TestableAst> ast = {}) {
  return Ast("Expressions", "", sp, ast);
}

inline TestableAst FnExpr(lux::SourcePosition sp,
                          std::initializer_list<TestableAst> ast = {}) {
  return Ast("FunctionExpression", "", sp, ast);
}

inline TestableAst Unary(const char* operand, const char* position,
                         lux::SourcePosition sp, const TestableAst& ast) {
  std::stringstream ss;
  ss << "operand = " << operand << " position = " << position;
  auto s = ss.str();
  return Ast("UnaryExpression", s.c_str(), sp, {ast});
}

inline TestableAst Lit(const char* attr, lux::SourcePosition sp,
                       std::initializer_list<TestableAst> ast = {}) {
  return Ast("Literal", attr, sp, ast);
}

inline TestableAst Number(const char* value, lux::SourcePosition sp,
                          std::initializer_list<TestableAst> ast = {}) {
  std::stringstream ss;
  ss << "type = NUMERIC_LITERAL value = " << value;
  auto s = ss.str();
  return Ast("Literal", s.c_str(), sp, ast);
}

inline TestableAst Str(const char* value, lux::SourcePosition sp,
                       std::initializer_list<TestableAst> ast = {}) {
  std::stringstream ss;
  ss << "type = STRING_LITERAL value = " << value;
  auto s = ss.str();
  return Ast("Literal", s.c_str(), sp, ast);
}

inline TestableAst Ident(const char* value, lux::SourcePosition sp,
                         std::initializer_list<TestableAst> ast = {}) {
  std::stringstream ss;
  ss << "type = IDENTIFIER value = " << value;
  auto s = ss.str();
  return Ast("Literal", s.c_str(), sp, ast);
}

class ParserTest : public lux::IsolateSetup {
 protected:
  class SourcePositions {
   public:
    explicit SourcePositions(
        std::initializer_list<lux::SourcePosition> source_positions) {
      INVALIDATE(source_positions.size() == 3);
      auto i = 0;
      for (auto& s : source_positions) {
        source_positions_[i++] = s;
      }
    }

    lux::SourcePosition operator[](int index) {
      INVALIDATE(index < 3);
      return source_positions_[index];
    }

   private:
    lux::SourcePosition source_positions_[3];
  };
  void ParseTest(const char* envList[][2], const char* code,
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
            lux::testing::CompareNode(expr->ToStringTree().c_str(),
                                      expectations[i]);
          };
        } else {
          FAIL() << "Parsing code '" << ret << "' failed.\n";
        }
      }
    }
  }

  void SyntaxErrorTest(const char* envList[][2], const char* code,
                       SourcePositions source_positions,
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
      for (auto& e : r) {
        EXPECT_TRUE(lux::testing::CompareSourcePosition(e->source_position(),
                                                        source_positions[i]))
            << "Code '" << ret << "' error position is not valid\n";
      }
      if (show_error) {
        r.PrintError();
      }
    }
  }

  template <typename Fn>
  std::unique_ptr<const char*> FunctionExprWrapper(uint32_t expr_size,
                                                   Fn ast_builder) {
    std::stringstream ss;
    uint32_t exit = 29 + expr_size;
    uint32_t exit_expr = 12 + expr_size;
    uint32_t func_exit = exit_expr + 2;
    auto ast = Stmts(
        {0, exit, 0, 0},
        {ExprStmt({0, func_exit, 0, 0},
                  {FnExpr({0, func_exit, 0, 0},
                          {Ident("X", {9, 9, 0, 0}), Exprs({10, 11, 0, 0}),
                           Stmts({12, exit_expr, 0, 0},
                                 {ExprStmt({12, exit_expr, 0, 0},
                                           {ast_builder(12, exit_expr)})})})}),
         ExprStmt({func_exit, exit, 0, 0},
                  {Lit("type = IDENTIFIER value = PARSER_SENTINEL",
                       {func_exit, exit, 0, 0})})});
    auto s = ast.ToString();
    auto m = new char[s.size() + 2]();
    snprintf(m, s.size() + 1, "%s", s.c_str());
    return std::make_unique<const char*>(m);
  }

  template <typename Fn>
  void SingleExpressionTest(Fn ast_builder, const char* value,
                            bool is_skip_strict_mode = false) {
    const char* env[][2] = {
        {"", ""},
        {(is_skip_strict_mode ? nullptr : "'use strict';"), ""},
        {"function X() {", "}"}};
    uint32_t size = strlen(value);
    auto f = FunctionExprWrapper(size, ast_builder);

    uint32_t exit = 16 + size;
    auto normal =
        Stmts({0, exit, 0, 0},
              {ExprStmt({0, size, 0, 0}, {ast_builder(0, size)}),
               ExprStmt({size + 1, exit, 0, 0},
                        {Ident("PARSER_SENTINEL", {size + 1, exit, 0, 0})})})
            .ToString();
    exit = 29 + size;
    uint32_t expr_stmt_exit = 13 + size;
    auto strict =
        Stmts({13, exit, 0, 0},
              {ExprStmt({13, expr_stmt_exit, 0, 0},
                        {ast_builder(13, expr_stmt_exit)}),
               ExprStmt({size + 14, exit, 0, 0},
                        {Ident("PARSER_SENTINEL", {size + 14, exit, 0, 0})})})
            .ToString();
    Expectations expected = {{normal.c_str(), strict.c_str(), *f}};
    ParseTest(env, value, expected);
  }
};

namespace {
TEST_F(ParserTest, SingleDecimalLiteral) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Number("1", {start, end, 0, 0});
      },
      "1");
}

TEST_F(ParserTest, MultiDecimalLiteral) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Number("1024", {start, end, 0, 0});
      },
      "1024");
}

TEST_F(ParserTest, MultiDecimalExponentLiteral) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Number("13e+10", {start, end, 0, 0});
      },
      "13e+10");
}

TEST_F(ParserTest, FloatLeadingZeroLiteral) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Number("0.12", {start, end, 0, 0});
      },
      "0.12");
}

TEST_F(ParserTest, FloatNotLeadingZeroLiteral) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Number(".12", {start, end, 0, 0});
      },
      ".12");
}

TEST_F(ParserTest, HexDecimalLiteral) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Number("0xabcdef1234567890", {start, end, 0, 0});
      },
      "0xabcdef1234567890");
}

TEST_F(ParserTest, BinaryLiteral) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Number("0b0101", {start, end, 0, 0});
      },
      "0b0101");
}

TEST_F(ParserTest, OctalLiteral) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Number("0o777", {start, end, 0, 0});
      },
      "0o777");
}

TEST_F(ParserTest, ImplicitOctalLiteral) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Number("0777", {start, end, 0, 0});
      },
      "0777", true);
}

TEST_F(ParserTest, DecimalLeadingZeroLiteral) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Number("07778", {start, end, 0, 0});
      },
      "07778", true);
}

TEST_F(ParserTest, NumericLiteralError) {
  const char* env[][2] = {
      {"", ""}, {"'use strict';", ""}, {"function X() {", "}"}};
  SyntaxErrorTest(
      env, "0x_",
      SourcePositions({{0, 2, 0, 0}, {13, 15, 0, 0}, {12, 14, 0, 0}}));
  SyntaxErrorTest(
      env, "0b_",
      SourcePositions({{0, 2, 0, 0}, {13, 15, 0, 0}, {12, 14, 0, 0}}));
  SyntaxErrorTest(
      env, "0o_",
      SourcePositions({{0, 2, 0, 0}, {13, 15, 0, 0}, {12, 14, 0, 0}}));
  SyntaxErrorTest(
      env, "13e",
      SourcePositions({{0, 3, 0, 0}, {13, 16, 0, 0}, {12, 15, 0, 0}}));
  SyntaxErrorTest(
      env, "13e+",
      SourcePositions({{0, 4, 0, 0}, {13, 17, 0, 0}, {12, 16, 0, 0}}));
}

TEST_F(ParserTest, ImplicitOctalError) {
  const char* env[][2] = {
      {nullptr, nullptr}, {nullptr, nullptr}, {"'use strict';", ""}};
  SyntaxErrorTest(env, "0777", SourcePositions({{}, {}, {13, 17, 0, 0}}));
}

TEST_F(ParserTest, StringLiteralSingleQuote) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Str("test", {start, end, 0, 0});
      },
      "'test'");
}

TEST_F(ParserTest, StringLiteralDoubleQuote) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Str("test", {start, end, 0, 0});
      },
      "\"test\"");
}

TEST_F(ParserTest, StringLiteralEscapeSingleQuote) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Str("test 'value", {start, end, 0, 0});
      },
      "'test \\'value'");
}

TEST_F(ParserTest, StringLiteralEscapeDoubleQuote) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Str("test \"value", {start, end, 0, 0});
      },
      "\"test \\\"value\"");
}

TEST_F(ParserTest, StringLiteralEscapeBackSlashSingleQuote) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Str("test\\ value", {start, end, 0, 0});
      },
      "'test\\\\ value'");
}

TEST_F(ParserTest, StringLiteralEscapeBackSlashDoubleQuote) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Str("test\\ value", {start, end, 0, 0});
      },
      "\"test\\\\ value\"");
}

TEST_F(ParserTest, StringLiteralUnicodeEscapeSequence) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Str("A_B_C_D", {start, end, 0, 0});
      },
      "'\\u0041_\\u0042_\\u0043_\\u0044'", true);
}

TEST_F(ParserTest, StringLiteralAsciiEscapeSequence) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Str("A_B_C_D", {start, end, 0, 0});
      },
      "'\\x41_\\x42_\\x43_\\x44'", true);
}

TEST_F(ParserTest, UnterminatedStringLiteralError) {
  const char* env[][2] = {
      {"", ""}, {"'use strict';", ""}, {"function X() {", "}"}};
  SyntaxErrorTest(
      env, "'test",
      SourcePositions({{0, 21, 0, 0}, {13, 34, 0, 0}, {12, 34, 0, 0}}));
}

TEST_F(ParserTest, UnterminatedStringLiteralErrorWithLineBreak) {
  const char* env[][2] = {
      {"", ""}, {"'use strict';", ""}, {"function X() {", "}"}};
  SyntaxErrorTest(
      env, "'test\\n",
      SourcePositions({{0, 23, 0, 0}, {13, 36, 0, 0}, {12, 36, 0, 0}}));
}

TEST_F(ParserTest, InvalidUnicodeSequenceError) {
  const char* env[][2] = {
      {"", ""}, {"'use strict';", ""}, {"function X() {", "}"}};
  SyntaxErrorTest(
      env, "'\\u0041_\\u0042_\\u043_\\u0044'",
      SourcePositions({{0, 20, 0, 0}, {13, 33, 0, 0}, {12, 32, 0, 0}}));
}

TEST_F(ParserTest, UnaryExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Unary("OP_PLUS", "PRE", {start, end, 0, 0},
                     {Number("1", {start + 1, end, 0, 0})});
      },
      "+1");
}

}  // namespace
