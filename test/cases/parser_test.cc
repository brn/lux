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
  static const TestableAst kNull;

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
    if (name_.size() == 0) {
      return "";
    }
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

const TestableAst TestableAst::kNull = TestableAst("", "", {});

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

inline TestableAst Binary(const char* operand, lux::SourcePosition sp,
                          const TestableAst& left, const TestableAst& right) {
  std::stringstream ss;
  ss << "operand = " << operand;
  auto s = ss.str();
  return Ast("BinaryExpression", s.c_str(), sp, {left, right});
}

inline TestableAst CallExpr(const char* receiver, lux::SourcePosition sp,
                            const TestableAst& callee,
                            const TestableAst& args = TestableAst::kNull) {
  std::stringstream ss;
  ss << "receiver = " << receiver;
  auto s = ss.str();
  return Ast("CallExpression", s.c_str(), sp, {callee, args});
}

inline TestableAst NewExpr(lux::SourcePosition sp, const TestableAst& callee) {
  return Ast("NewExpression", "", sp, {callee});
}

inline TestableAst Prop(const char* type, lux::SourcePosition sp,
                        const TestableAst& callee, const TestableAst& prop) {
  std::stringstream ss;
  ss << "property_access = " << type;
  auto s = ss.str();
  return Ast("PropertyAccessExpression", s.c_str(), sp, {callee, prop});
}

inline TestableAst Lit(const char* attr, lux::SourcePosition sp,
                       std::initializer_list<TestableAst> ast = {}) {
  return Ast("Literal", attr, sp, ast);
}

inline TestableAst ArrayLit(bool has_spread, lux::SourcePosition sp,
                            std::initializer_list<TestableAst> ast = {}) {
  std::stringstream st;
  st << "type = ArrayLiteral";
  if (has_spread) {
    st << " spread = true";
  }
  auto str = st.str();
  return Ast("StructuralLiteral", str.c_str(), sp, ast);
}

inline TestableAst Number(const char* value, lux::SourcePosition sp,
                          std::initializer_list<TestableAst> ast = {}) {
  std::stringstream ss;
  ss << "type = NUMERIC_LITERAL value = " << value;
  auto s = ss.str();
  return Ast("Literal", s.c_str(), sp, ast);
}

inline TestableAst Str(const char* value, lux::SourcePosition sp) {
  std::stringstream ss;
  ss << "type = STRING_LITERAL value = " << value;
  auto s = ss.str();
  return Ast("Literal", s.c_str(), sp, {});
}

inline TestableAst Tmpl(lux::SourcePosition sp,
                        std::initializer_list<TestableAst> ast) {
  return Ast("TemplateLiteral", "", sp, ast);
}

inline TestableAst Ident(const char* value, lux::SourcePosition sp,
                         std::initializer_list<TestableAst> ast = {}) {
  std::stringstream ss;
  ss << "type = IDENTIFIER value = " << value;
  auto s = ss.str();
  return Ast("Literal", s.c_str(), sp, ast);
}

inline TestableAst Cond(lux::SourcePosition sp, TestableAst cond,
                        TestableAst then, TestableAst else_node) {
  return Ast("ConditionalExpression", "", sp, {cond, then, else_node});
}

inline TestableAst Elision(lux::SourcePosition sp) {
  return Ast("Elision", "", sp);
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
            ASSERT_TRUE(lux::testing::CompareNode(
                ret.c_str(), expr->ToStringTree().c_str(), expectations[i]))
                << parser.stack_trace();
          };
        } else {
          parser.PrintStackTrace();
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
  std::unique_ptr<const char*> FunctionExprWrapper(
      uint32_t expr_size, Fn ast_builder, uint32_t end_line_number,
      uint32_t before_line_break_col_count) {
    std::stringstream ss;

    uint32_t original_blb_cc = before_line_break_col_count;
    if (before_line_break_col_count > 0) {
      before_line_break_col_count += 14;
    }

    uint32_t exit = (31 + expr_size) - before_line_break_col_count;
    uint32_t exit_expr = (13 + expr_size) - before_line_break_col_count;
    uint32_t func_exit = exit_expr + 2;
    uint32_t base_position = 9;
    uint32_t sentinel_start =
        original_blb_cc > 0 ? (expr_size - original_blb_cc) + 2 : func_exit + 1;

    auto ast = Stmts(
        {0, exit, 0, end_line_number},
        {ExprStmt(
             {0, func_exit, 0, end_line_number},
             {FnExpr(
                 {0, func_exit, 0, end_line_number},
                 {Ident("X", {base_position, base_position + 1, 0, 0}),
                  Exprs({base_position + 1, base_position + 3, 0, 0}),
                  Stmts({base_position + 4,
                         (base_position + (7 + expr_size)) -
                             before_line_break_col_count,
                         0, end_line_number},
                        {ExprStmt(
                            {base_position + 5,
                             (base_position + (5 + expr_size)) -
                                 before_line_break_col_count,
                             0, end_line_number},
                            {ast_builder(base_position + 5,
                                         (base_position + (5 + expr_size) -
                                          before_line_break_col_count))})})})}),
         ExprStmt({sentinel_start, exit, 0, end_line_number},
                  {Lit("type = IDENTIFIER value = PARSER_SENTINEL",
                       {sentinel_start, exit, 0, end_line_number})})});
    auto s = ast.ToString();
    auto m = new char[s.size() + 2]();
    snprintf(m, s.size() + 1, "%s", s.c_str());
    return std::make_unique<const char*>(m);
  }

  template <typename Fn>
  void SingleExpressionTest(Fn ast_builder, const char* value,
                            bool is_skip_strict_mode = false,
                            uint32_t end_line_number = 0,
                            uint32_t before_line_break_col_count = 0) {
    const char* env[][2] = {
        {"", ""},
        {(is_skip_strict_mode ? nullptr : "'use strict';"), ""},
        {"function X() {", "}"}};
    uint32_t size = strlen(value);
    auto f = FunctionExprWrapper(size, ast_builder, end_line_number,
                                 before_line_break_col_count);

    uint32_t exit = (16 + size) - before_line_break_col_count;
    uint32_t expr_exit = size - before_line_break_col_count;
    auto normal =
        Stmts({0, exit, 0, end_line_number},
              {ExprStmt({0, expr_exit, 0, end_line_number},
                        {ast_builder(0, size - before_line_break_col_count)}),
               ExprStmt({(size + 1) - before_line_break_col_count, exit, 0,
                         end_line_number},
                        {Ident("PARSER_SENTINEL",
                               {(size + 1) - before_line_break_col_count, exit,
                                0, end_line_number})})})
            .ToString();
    exit = 29 + size;
    uint32_t expr_stmt_exit = (13 + size) - before_line_break_col_count;
    auto strict =
        Stmts({13, exit, 0, end_line_number},
              {ExprStmt({13, expr_stmt_exit, 0, end_line_number},
                        {ast_builder(13, expr_stmt_exit)}),
               ExprStmt({(size + 14) - before_line_break_col_count, exit, 0,
                         end_line_number},
                        {Ident("PARSER_SENTINEL",
                               {(size + 14) - before_line_break_col_count, exit,
                                0, end_line_number})})})
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
      SourcePositions({{0, 2, 0, 0}, {13, 15, 0, 0}, {14, 16, 0, 0}}));
  SyntaxErrorTest(
      env, "0b_",
      SourcePositions({{0, 2, 0, 0}, {13, 15, 0, 0}, {14, 16, 0, 0}}));
  SyntaxErrorTest(
      env, "0o_",
      SourcePositions({{0, 2, 0, 0}, {13, 15, 0, 0}, {14, 16, 0, 0}}));
  SyntaxErrorTest(
      env, "13e",
      SourcePositions({{0, 3, 0, 0}, {13, 16, 0, 0}, {14, 17, 0, 0}}));
  SyntaxErrorTest(
      env, "13e+",
      SourcePositions({{0, 4, 0, 0}, {13, 17, 0, 0}, {14, 18, 0, 0}}));
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

TEST_F(ParserTest, TemplateLiteralWithoutInterpolation) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Tmpl({start, end, 0, 0}, {Str("test", {start + 1, end, 0, 0})});
      },
      "`test`", true);
}

TEST_F(ParserTest, EscapedTemplateLiteralWithoutInterpolation) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Tmpl({start, end, 0, 0},
                    {Str("test${aaa}", {start + 1, end, 0, 0})});
      },
      "`test\\${aaa}`", true);
}

TEST_F(ParserTest, TemplateLiteralWithoutInterpolationAndLineBreak) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Tmpl({start, end, 0, 1},
                    {Str("test\ntest", {start + 1, end, 0, 1})});
      },
      "`test\ntest`", true, 1, 5);
}

TEST_F(ParserTest, TemplateLiteralWithInterpolationAndEmptySuffix) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Tmpl({start, end, 0, 0},
                    {Str("test", {start + 1, end - 6, 0, 0}),
                     Ident("test", {start + 7, end - 2, 0, 0})});
      },
      "`test${test}`", true);
}

TEST_F(ParserTest, TemplateLiteralWithInterpolationAndSuffix) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Tmpl({start, end, 0, 0},
                    {Str("foo", {start + 1, end - 8, 0, 0}),
                     Ident("bar", {start + 6, end - 5, 0, 0}),
                     Str("baz", {start + 10, end})});
      },
      "`foo${bar}baz`", true);
}

TEST_F(ParserTest, TemplateLiteralWithManyInterpolationAndSuffix) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Tmpl({start, end, 0, 0},
                    {Str("foo", {start + 1, end - 21, 0, 0}),
                     Ident("bar", {start + 6, end - 18, 0, 0}),
                     Str("baz", {start + 10, end - 12}),
                     Number("100", {start + 15, end - 9}),
                     Unary("OP_PLUS", "PRE", {start + 21, end - 2},
                           {Ident("foo", {start + 22, end - 2})})});
      },
      "`foo${bar}baz${100}${+foo}`", true);
}

TEST_F(ParserTest, NestedTemplateLiteral) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Tmpl({start, end, 0, 0},
                    {Str("foo", {start + 1, end - 13, 0, 0}),
                     Tmpl({start + 6, end - 2, 0, 0},
                          {Str("foo", {start + 7, end - 7}),
                           Ident("bar", {start + 12, end - 4})})});
      },
      "`foo${`foo${bar}`}`", true);
}

TEST_F(ParserTest, UnterminatedStringLiteralError) {
  const char* env[][2] = {
      {"", ""}, {"'use strict';", ""}, {"function X() {", "}"}};
  SyntaxErrorTest(
      env, "'test",
      SourcePositions({{0, 21, 0, 0}, {13, 34, 0, 0}, {14, 36, 0, 0}}));
}

TEST_F(ParserTest, UnterminatedStringLiteralErrorWithLineBreak) {
  const char* env[][2] = {
      {"", ""}, {"'use strict';", ""}, {"function X() {", "}"}};
  SyntaxErrorTest(
      env, "'test\\n",
      SourcePositions({{0, 23, 0, 0}, {13, 36, 0, 0}, {14, 38, 0, 0}}));
}

TEST_F(ParserTest, InvalidUnicodeSequenceError) {
  const char* env[][2] = {
      {"", ""}, {"'use strict';", ""}, {"function X() {", "}"}};
  SyntaxErrorTest(
      env, "'\\u0041_\\u0042_\\u043_\\u0044'",
      SourcePositions({{0, 20, 0, 0}, {13, 33, 0, 0}, {14, 34, 0, 0}}));
}

TEST_F(ParserTest, UnaryExpressionPlusPre) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Unary("OP_PLUS", "PRE", {start, end, 0, 0},
                     {Number("1", {start + 1, end, 0, 0})});
      },
      "+1");
}

TEST_F(ParserTest, UnaryExpressionMinusPre) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Unary("OP_MINUS", "PRE", {start, end, 0, 0},
                     {Number("1", {start + 1, end, 0, 0})});
      },
      "-1");
}

TEST_F(ParserTest, UnaryExpressionNotPre) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Unary("OP_NOT", "PRE", {start, end, 0, 0},
                     {Number("1", {start + 1, end, 0, 0})});
      },
      "!1");
}

TEST_F(ParserTest, UnaryExpressionTildePre) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Unary("OP_TILDE", "PRE", {start, end, 0, 0},
                     {Number("1", {start + 1, end, 0, 0})});
      },
      "~1");
}

TEST_F(ParserTest, UnaryExpressionDeletePre) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        auto next = start + 7;
        return Unary("DELETE", "PRE", {start, end, 0, 0},
                     {Number("1", {next, end, 0, 0})});
      },
      "delete 1");
}

TEST_F(ParserTest, UnaryExpressionTypeofPre) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        auto next = start + 7;
        return Unary("TYPEOF", "PRE", {start, end, 0, 0},
                     {Number("1", {next, end, 0, 0})});
      },
      "typeof 1");
}

TEST_F(ParserTest, UnaryExpressionVoidPre) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Unary("VOID", "PRE", {start, end, 0, 0},
                     {Number("1", {start + 5, end, 0, 0})});
      },
      "void 1");
}

TEST_F(ParserTest, UpdateExpressionPreIncrement) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Unary("OP_INCREMENT", "PRE", {start, end, 0, 0},
                     {Ident("X", {start + 2, end, 0, 0})});
      },
      "++X");
}

TEST_F(ParserTest, UpdateExpressionPreDecrement) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Unary("OP_DECREMENT", "PRE", {start, end, 0, 0},
                     {Ident("X", {start + 2, end, 0, 0})});
      },
      "--X");
}

TEST_F(ParserTest, UpdateExpressionPostIncrement) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Unary("OP_INCREMENT", "POST", {start, end, 0, 0},
                     {Ident("X", {start, end - 2, 0, 0})});
      },
      "X++");
}

TEST_F(ParserTest, UpdateExpressionPostDecrement) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Unary("OP_DECREMENT", "POST", {start, end, 0, 0},
                     {Ident("X", {start, end - 2, 0, 0})});
      },
      "X--");
}

TEST_F(ParserTest, NewExpressionNoArgs) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return NewExpr({start, end, 0, 0}, Ident("X", {start + 4, end, 0, 0}));
      },
      "new X");
}

TEST_F(ParserTest, NewExpressionWithArgs) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return NewExpr(
            {start, end, 0, 0},
            CallExpr("EXPRESSION",
                     {
                         start + 4,
                         end,
                         0,
                         0,
                     },
                     Ident("X", {start + 4, end - 3, 0, 0}),
                     Exprs({start + 5, end},
                           {Number("1", {start + 6, end - 1, 0, 0})})));
      },
      "new X(1)");
}

TEST_F(ParserTest, NewExpressionWithPropsCall) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return NewExpr(
            {start, end, 0, 0},
            CallExpr("EXPRESSION",
                     {
                         start + 4,
                         end,
                         0,
                         0,
                     },
                     Prop("dot", {start + 4, end - 3, 0, 0},
                          Ident("X", {start + 4, end - 5, 0, 0}),
                          Ident("a", {start + 6, end - 3, 0, 0})),
                     Exprs({start + 7, end, 0, 0},
                           {Number("1", {start + 8, end - 1, 0, 0})})));
      },
      "new X.a(1)");
}

TEST_F(ParserTest, NewExpressionWithElementCall) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return NewExpr(
            {start, end, 0, 0},
            CallExpr("EXPRESSION",
                     {
                         start + 4,
                         end,
                         0,
                         0,
                     },
                     Prop("element", {start + 4, end - 3, 0, 0},
                          Ident("X", {start + 4, end - 8, 0, 0}),
                          Str("a", {start + 6, end - 4, 0, 0})),
                     Exprs({start + 10, end, 0, 0},
                           {Number("1", {start + 11, end - 1, 0, 0})})));
      },
      "new X['a'](1)");
}

TEST_F(ParserTest, NewExpressionWithCallProps) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Prop(
            "dot", {start, end, 0, 0},
            NewExpr({start, end - 2, 0, 0},
                    CallExpr("EXPRESSION",
                             {
                                 start + 4,
                                 end - 2,
                                 0,
                                 0,
                             },
                             Ident("X", {start + 4, end - 5, 0, 0}),
                             Exprs({start + 5, end - 2, 0, 0},
                                   {Number("1", {start + 6, end - 3, 0, 0})}))),
            Ident("a", {start + 9, end, 0, 0}));
      },
      "new X(1).a");
}

TEST_F(ParserTest, NewExpressionWithCallElement) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Prop(
            "element", {start, end, 0, 0},
            NewExpr({start, end - 5, 0, 0},
                    CallExpr("EXPRESSION",
                             {
                                 start + 4,
                                 end - 5,
                                 0,
                                 0,
                             },
                             Ident("X", {start + 4, end - 8, 0, 0}),
                             Exprs({start + 5, end - 5, 0, 0},
                                   {Number("1", {start + 6, end - 6, 0, 0})}))),
            Str("a", {start + 9, end - 1, 0, 0}));
      },
      "new X(1)['a']");
}

TEST_F(ParserTest, NewExpressionWithPropsChain) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return NewExpr(
            {start, end, 0, 0},
            CallExpr("EXPRESSION", {start + 4, end},
                     Prop("dot", {start + 4, end - 2},
                          Prop("dot", {start + 4, end - 4},
                               Ident("a", {start + 4, end - 6, 0, 0}),
                               Ident("b", {start + 6, end - 4, 0, 0})),
                          Ident("c", {start + 8, end - 2, 0, 0})),
                     Exprs({start + 9, end})));
      },
      "new a.b.c()");
}

TEST_F(ParserTest, NewExpressionWithPropsAndElementChain) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return NewExpr(
            {start, end, 0, 0},
            CallExpr("EXPRESSION", {start + 4, end},
                     Prop("dot", {start + 4, end - 2},
                          Prop("element", {start + 4, end - 4},
                               Ident("a", {start + 4, end - 9, 0, 0}),
                               Str("b", {start + 6, end - 5, 0, 0})),
                          Ident("c", {start + 11, end - 2, 0, 0})),
                     Exprs({start + 12, end})));
      },
      "new a['b'].c()");
}

TEST_F(ParserTest, NewExpressionWithPropsCallChain) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Prop(
            "dot", {start, end},
            NewExpr({start, end - 2, 0, 0},
                    CallExpr("EXPRESSION", {start + 4, end - 2},
                             Prop("dot", {start + 4, end - 4},
                                  Ident("a", {start + 4, end - 6, 0, 0}),
                                  Ident("b", {start + 6, end - 4, 0, 0})),
                             Exprs({start + 7, end - 2}))),
            Ident("c", {start + 10, end, 0, 0}));
      },
      "new a.b().c");
}

TEST_F(ParserTest, NewExpressionWithTaggedTemplate) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return CallExpr(
            "TEMPLATE", {start, end},
            Prop("dot", {start, end - 6},
                 NewExpr({start, end - 8, 0, 0},
                         CallExpr("EXPRESSION", {start + 4, end - 8},
                                  Ident("X", {start + 4, end - 10, 0, 0}),
                                  Exprs({start + 5, end - 8}))),
                 Ident("a", {start + 8, end - 6, 0, 0})),
            Tmpl({start + 9, end}, {Str("test", {start + 10, end})}));
      },
      "new X().a`test`");
}

TEST_F(ParserTest, ExponentiationExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_POW", {start, end}, Number("1", {start, start + 1}),
                      Number("1", {start + 5, end}));
      },
      "1 ** 1");
}

TEST_F(ParserTest, MultiplicativeMulExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_MUL", {start, end}, Number("1", {start, start + 1}),
                      Number("1", {start + 4, end}));
      },
      "1 * 1");
}

TEST_F(ParserTest, MultiplicativeDivExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_DIV", {start, end}, Number("1", {start, start + 1}),
                      Number("1", {start + 4, end}));
      },
      "1 / 1");
}

TEST_F(ParserTest, MultiplicativeModExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_MOD", {start, end}, Number("1", {start, start + 1}),
                      Number("1", {start + 4, end}));
      },
      "1 % 1");
}

TEST_F(ParserTest, AdditivePlusExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_PLUS", {start, end}, Number("1", {start, start + 1}),
                      Number("1", {start + 4, end}));
      },
      "1 + 1");
}

TEST_F(ParserTest, AdditiveMinusExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_MINUS", {start, end}, Number("1", {start, start + 1}),
                      Number("1", {start + 4, end}));
      },
      "1 - 1");
}

TEST_F(ParserTest, ShiftLeftExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_SHIFT_LEFT", {start, end},
                      Ident("v", {start, start + 1}),
                      Number("1", {start + 5, end}));
      },
      "v << 1");
}

TEST_F(ParserTest, ShiftRightExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_SHIFT_RIGHT", {start, end},
                      Ident("v", {start, start + 1}),
                      Number("1", {start + 5, end}));
      },
      "v >> 1");
}

TEST_F(ParserTest, UShiftRightExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_U_SHIFT_RIGHT", {start, end},
                      Ident("v", {start, start + 1}),
                      Number("1", {start + 6, end}));
      },
      "v >>> 1");
}

TEST_F(ParserTest, InExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("IN", {start, end}, Str("a", {start, start + 3}),
                      Ident("v", {start + 7, end}));
      },
      "'a' in v");
}

TEST_F(ParserTest, InstanceofExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("INSTANCEOF", {start, end},
                      Ident("x", {start, start + 1}),
                      Ident("y", {start + 13, end}));
      },
      "x instanceof y");
}

TEST_F(ParserTest, GreaterThanExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_GREATER_THAN", {start, end},
                      Number("1", {start, start + 1}),
                      Number("1", {start + 4, end}));
      },
      "1 > 1");
}

TEST_F(ParserTest, GreaterThanOrEqExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_GREATER_THAN_OR_EQ", {start, end},
                      Number("1", {start, start + 1}),
                      Number("1", {start + 5, end}));
      },
      "1 >= 1");
}

TEST_F(ParserTest, LessThanExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_LESS_THAN", {start, end},
                      Number("1", {start, start + 1}),
                      Number("1", {start + 4, end}));
      },
      "1 < 1");
}

TEST_F(ParserTest, LessThanOrEqExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_LESS_THAN_OR_EQ", {start, end},
                      Number("1", {start, start + 1}),
                      Number("1", {start + 5, end}));
      },
      "1 <= 1");
}

TEST_F(ParserTest, EqExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_EQ", {start, end}, Number("1", {start, start + 1}),
                      Number("1", {start + 5, end}));
      },
      "1 == 1");
}

TEST_F(ParserTest, StrictEqExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_STRICT_EQ", {start, end},
                      Number("1", {start, start + 1}),
                      Number("1", {start + 6, end}));
      },
      "1 === 1");
}

TEST_F(ParserTest, NotEqExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_NOT_EQ", {start, end},
                      Number("1", {start, start + 1}),
                      Number("1", {start + 5, end}));
      },
      "1 != 1");
}

TEST_F(ParserTest, StrictNotEqExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_STRICT_NOT_EQ", {start, end},
                      Number("1", {start, start + 1}),
                      Number("1", {start + 6, end}));
      },
      "1 !== 1");
}

TEST_F(ParserTest, BitwiseANDExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_AND", {start, end}, Number("1", {start, start + 1}),
                      Number("1", {start + 4, end}));
      },
      "1 & 1");
}

TEST_F(ParserTest, BitwiseORExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_OR", {start, end}, Number("1", {start, start + 1}),
                      Number("1", {start + 4, end}));
      },
      "1 | 1");
}

TEST_F(ParserTest, BitwiseXORExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_XOR", {start, end}, Number("1", {start, start + 1}),
                      Number("1", {start + 4, end}));
      },
      "1 ^ 1");
}

TEST_F(ParserTest, LogicalANDExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_LOGICAL_AND", {start, end},
                      Number("1", {start, start + 1}),
                      Number("1", {start + 5, end}));
      },
      "1 && 1");
}

TEST_F(ParserTest, LogicalORExpression) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_LOGICAL_OR", {start, end},
                      Number("1", {start, start + 1}),
                      Number("1", {start + 5, end}));
      },
      "1 || 1");
}

TEST_F(ParserTest, OperatorPriority_1) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_OR", {start, end},
                      Binary("OP_MINUS", {start, start + 9},
                             Ident("a", {start, start + 1}),
                             Binary("OP_MUL", {start + 4, start + 9},
                                    Ident("b", {start + 4, start + 5}),
                                    Ident("c", {start + 8, start + 9}))),
                      Binary("OP_SHIFT_LEFT", {start + 12, start + 18},
                             Ident("d", {start + 12, start + 13}),
                             Binary("OP_POW", {start + 17, start + 23},
                                    Ident("e", {start + 17, start + 18}),
                                    Ident("f", {start + 22, start + 23}))));
      },
      "a - b * c | d << e ** f");
}

TEST_F(ParserTest, OperatorPriority_2) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_OR", {start, end},
                      Binary("OP_SHIFT_RIGHT", {start, start + 14},
                             Binary("OP_PLUS", {start, start + 9},
                                    Binary("OP_MUL", {start, start + 5},
                                           Ident("a", {start, start + 1}),
                                           Ident("b", {start + 4, start + 5})),
                                    Ident("c", {start + 8, start + 9})),
                             Ident("d", {start + 13, start + 14})),
                      Binary("OP_DIV", {start + 17, start + 22},
                             Ident("e", {start + 17, start + 18}),
                             Ident("f", {start + 21, start + 22})));
      },
      "a * b + c >> d | e / f");
}

TEST_F(ParserTest, ConditionalExpression_1) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Cond({start, end}, Ident("x", {start, start + 1}),
                    Number("1", {start + 3, start + 4}),
                    Number("0", {end - 1, end}));
      },
      "x? 1: 0");
}

TEST_F(ParserTest, ConditionalExpression_2) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Cond(
            {start, end},
            Binary("OP_PLUS", {start, start + 5},
                   Ident("x", {start, start + 1}),
                   Number("1", {start + 4, start + 5})),
            NewExpr({start + 7, start + 14},
                    CallExpr("EXPRESSION",
                             {
                                 start + 11,
                                 start + 14,
                             },
                             Ident("X", {start + 11, start + 12}),
                             Exprs({start + 12, start + 14}, {}))),
            Binary("OP_MINUS", {end - 5, end}, Ident("y", {end - 5, end - 4}),
                   Number("3", {end - 1, end})));
      },
      "x + 1? new X(): y - 3");
}

#define ASSIGNMENT_OP_LIST(A)        \
  A(OP_MUL_ASSIGN, "*=")             \
  A(OP_DIV_ASSIGN, "/=")             \
  A(OP_MOD_ASSIGN, "%=")             \
  A(OP_PLUS_ASSIGN, "+=")            \
  A(OP_MINUS_ASSIGN, "-=")           \
  A(OP_AND_ASSIGN, "&=")             \
  A(OP_OR_ASSIGN, "|=")              \
  A(OP_XOR_ASSIGN, "^=")             \
  A(OP_ASSIGN, "=")                  \
  A(OP_SHIFT_LEFT_ASSIGN, "<<=")     \
  A(OP_SHIFT_RIGHT_ASSIGN, ">>=")    \
  A(OP_U_SHIFT_RIGHT_ASSIGN, ">>>=") \
  A(OP_POW_ASSIGN, "**=")

#define MAKE_ASSIGNMENT_TEST(NAME, op)                                       \
  TEST_F(ParserTest, AssignmentExpression_##NAME) {                          \
    SingleExpressionTest(                                                    \
        [&](uint32_t start, uint32_t end) {                                  \
          return Binary(#NAME, {start, end}, Ident("x", {start, start + 1}), \
                        Number("1", {end - 1, end}));                        \
        },                                                                   \
        "x " op " 1");                                                       \
  }
ASSIGNMENT_OP_LIST(MAKE_ASSIGNMENT_TEST);
#undef MAKE_ASSIGNMENT_TEST
#undef ASSIGNMENT_OP_LIST

TEST_F(ParserTest, ArrayLiteral) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return ArrayLit(false, {start, end},
                        {Number("1", {start + 1, start + 2}),
                         Number("2", {end - 2, end - 1})});
      },
      "[1,2]");
}

TEST_F(ParserTest, ArrayLiteralSpread) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return ArrayLit(false, {start, end},
                        {Number("1", {start + 1, start + 2}),
                         Unary("SPREAD", "PRE", {end - 5, end - 1},
                               Ident("x", {end - 2, end - 1}))});
      },
      "[1,...x]");
}

TEST_F(ParserTest, ArrayLiteralEmpty) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return ArrayLit(
            false, {start, end},
            {Number("1", {start + 1, start + 2}), Elision({end - 2, end - 1})});
      },
      "[1,]");
}

TEST_F(ParserTest, ArrayLiteralEmpty2) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return ArrayLit(
            false, {start, end},
            {Number("1", {start + 1, start + 2}), Elision({end - 4, end - 3}),
             Elision({end - 3, end - 2}), Elision({end - 2, end - 1})});
      },
      "[1,,,]");
}

TEST_F(ParserTest, ArrayAssignmentPattern) {
  SingleExpressionTest(
      [&](uint32_t start, uint32_t end) {
        return Binary("OP_ASSIGN", {start, end},
                      ArrayLit(false, {start, end - 4},
                               {Ident("a", {start + 1, start + 2}),
                                Ident("b", {start + 3, start + 4}),
                                Ident("c", {start + 5, start + 6})}),
                      Ident("y", {end - 1, end}));
      },
      "[a,b,c] = y");
}
}  // namespace
