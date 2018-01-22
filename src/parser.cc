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

#include <iostream>
#include <sstream>
#include <type_traits>
#include <memory>
#include "./parser.h"
#include "./reporter.h"

namespace lux {
#define BASE_REPORT_SYNTAX_ERROR_(parser)                               \
  auto e = std::make_shared<lux::ErrorDescriptor>(parser->position());  \
  (reporter_->ReportSyntaxError(e))                                     \

#ifdef DEBUG
#define REPORT_SYNTAX_ERROR(parser, message)                            \
  BASE_REPORT_SYNTAX_ERROR_(parser)                                     \
  << "\n [Debug] line:" << __LINE__ << __FUNCTION__ << "\n" << message; \
  return nullptr;
#else
#define REPORT_SYNTAX_ERROR(parser, message)    \
  BASE_REPORT_SYNTAX_ERROR_(parser) << message  \
  return nullptr;
#endif

#define EXPECT_NOT_ADVANCE(parser, n, token, expect)  \
  if (n != token) {                                   \
    REPORT_SYNTAX_ERROR(                              \
        parser, "'" << expect << "' expected");       \
  }

#define EXPECT(parser, n, token, expect)        \
  EXPECT_NOT_ADVANCE(parser, n, token, expect)  \
  advance();

const uint8_t Ast::kStatementFlag;
const uint8_t Ast::kExpressionFlag;

const uint8_t PropertyAccessExpression::kAccessTypeMask;
const uint8_t PropertyAccessExpression::kReceiverTypeMask;

static Elision kElision;

Token::Type Parser::Tokenizer::Next() {
  current_buffer_ = &buffer_;
  current_position_ = &position_;
  if (lookahead_ != Token::INVALID) {
    auto ret = lookahead_;
    lookahead_ = Token::INVALID;
    return ret;
  }
  return token_ = Tokenize(Advance(true));
}

Token::Type Parser::Tokenizer::Peek() {
  current_buffer_ = &lookahead_buffer_;
  current_position_ = &lookahead_position_;
  if (lookahead_ != Token::INVALID) {
    return lookahead_;
  }
  lookahead_ = Tokenize(Advance(true));
  return lookahead_;
}

Token::Type Parser::Tokenizer::Current() {
  return token_;
}

Utf16CodePoint Parser::Tokenizer::Advance(bool beginning) {
  if (!beginning) {
    ++it_;
  }

  while (1) {
    switch (it_->code()) {
      case '\r':
        if (*(it_ + 1) == '\n') {
          ++it_;
        }
      case '\n':
        current_position_->set_end_line_number(
            position_.end_line_number() + 1);
        current_position_->set_end_col(0);
        ++it_;
        if (beginning) {
          set_linebreak_before();
        }
        break;
      case ' ':
        if (beginning) {
          unset_linebreak_before();
        }
        ++it_;
        break;
      default:
        current_position_->set_end_col(position_.end_col() + 1);
        goto OK;
    }
  }
OK:
  auto next = *(it_ + 1);
  if (next == '\n' || next == '\r') {
    set_linebreak_after();
  } else {
    unset_linebreak_after();
  }
  return *it_;
}

Parser::TokenizerRecord Parser::Tokenizer::Record() {
  return {
    it_, lookahead_, position_
  };
}

void Parser::Tokenizer::Restore(const Parser::TokenizerRecord& record) {
  position_ = record.position;
  it_ = record.cursor;
  lookahead_ = record.lookahead;
}

bool IsSucceeding(Utf16String::iterator* it, char ch) {
  (*it)++;
  return (*it)->code() == ch;
}

bool IsIdentifierStart(Utf16CodePoint value) {
  return (value >= 97 && value <= 122)
    || (value >= 65 && value <= 90)
    || (value == '_' || value == '$')
    || value == 92;
}

bool IsIdentifierChar(Utf16CodePoint value) {
  return IsIdentifierStart(value) || (value >= 48 && value <= 57);
}

bool IsNumericStart(Utf16CodePoint value) {
  return (value >= 48 && value <= 57)
    || value == 46;
}

bool IsNumeric(Utf16CodePoint value) {
  return (value >= 48 && value <= 57)
    || value == 101
    || value == 46;
}

bool IsHexDigit(Utf16CodePoint value) {
  return (value >= '0' && value <= '9')
    || (value >= 'a' && value <= 'z')
    || (value >= 'A' && value <= 'Z');
}

u32 ToHexValue(Utf16CodePoint uchar) {
  int ret = 0;
  if (uchar >= '0' && uchar <= '9') {
    ret = static_cast<int>(uchar.code() - '0');
  } else if (uchar >= 'a' && uchar.code() <= 'f') {
    ret = static_cast<int>(uchar.code() - 'a' + 10);
  } else if (uchar >= 'A' && uchar.code() <= 'F') {
    ret = static_cast<int>(uchar.code() - 'A' + 10);
  } else {
    return -1;
  }
  return ret;
}

Utf16CodePoint DecodeHexEscape(
    Utf16String::iterator* it, bool* ok, int len = 4) {
  auto unicode_hex_start = **it;
  u32 ret = 0;
  if (unicode_hex_start == '{') {
    while (**it != '}' && **it != Unicode::InvalidCodePoint()) {
      ret = ret * 16 + ToHexValue(**it);
    }
  } else {
    for (int i = 0; i < len; i++) {
      if (IsHexDigit(**it)) {
        ret = ret * 16 + ToHexValue(**it);
      } else {
        *ok = false;
        return Utf16CodePoint(0);
      }
    }
  }

  return Utf16CodePoint(ret);
}

Utf16CodePoint DecodeAsciiEscapeSequence(Utf16String::iterator* it, bool* ok) {
  auto u = DecodeHexEscape(it, ok, 2);
  if (u > 127) {
    *ok = false;
  }

  return u;
}

Token::Type Parser::Tokenizer::Tokenize(Utf16CodePoint start_value) {
  current_position_->set_start_col(current_position_->end_col() + 1);
  current_position_->set_start_line_number(
      current_position_->end_line_number());
  if (parser_state_.Is(State::IN_TEMPLATE_LITERAL)) {
    return TokenizeTemplateCharacters();
  }
  switch (start_value.code()) {
    case '(':
      return Token::LEFT_PAREN;
    case ')':
      return Token::RIGHT_PAREN;
    case '{':
      return Token::LEFT_BRACE;
    case '}':
      return Token::RIGHT_BRACE;
    case '[':
      return Token::LEFT_BRACKET;
    case ']':
      return Token::RIGHT_BRACKET;
    case '*':
      if (IsSucceeding(&it_, '=')) {
        return Token::OP_MUL_ASSIGN;
      }
      return Token::OP_MUL;
    case '/':
      if (IsSucceeding(&it_, '/')) {
        Advance();
        SkipSingleLineComment();
        return Tokenize(Advance(true));
      }
      if (IsSucceeding(&it_, '*')) {
        Advance();
        SkipMultiLineComment();
        return Tokenize(Advance(true));
      }
      if (parser_state_.Is(State::EXPECTED_BINARY_OPERATOR)) {
        if (IsSucceeding(&it_, '=')) {
          return Token::OP_DIV_ASSIGN;
        }
        return Token::OP_DIV;
      }
      return TokenizeRegExp();
    case '-':
      if (IsSucceeding(&it_, '=')) {
        return Token::OP_MINUS_ASSIGN;
      }
      return Token::OP_MINUS;
    case '+':
      if (IsSucceeding(&it_, '=')) {
        return Token::OP_PLUS_ASSIGN;
      }
      return Token::OP_PLUS;
    case '%':
      if (IsSucceeding(&it_, '=')) {
        return Token::OP_MOD_ASSIGN;
      }
      return Token::OP_MOD;
    case '<':
      if (IsSucceeding(&it_, '<')) {
        return Token::OP_SHIFT_LEFT;
      }
      return Token::OP_LESS_THAN;
    case '>':
      if (IsSucceeding(&it_, '>')) {
        if (IsSucceeding(&it_, '>')) {
          return Token::OP_U_SHIFT_RIGHT;
        }
        return Token::OP_SHIFT_RIGHT;
      }
      return Token::OP_GREATER_THAN;
    case '=':
      if (IsSucceeding(&it_, '=')) {
        if (IsSucceeding(&it_, '=')) {
          return Token::OP_STRICT_EQ;
        }
        return Token::OP_EQ;
      }
      return Token::OP_ASSIGN;
    case '|':
      if (IsSucceeding(&it_, '=')) {
        return Token::OP_OR_ASSIGN;
      }
      return Token::OP_OR;
    case '&':
      if (IsSucceeding(&it_, '=')) {
        return Token::OP_AND_ASSIGN;
      }
      return Token::OP_AND;
    case '^':
      if (IsSucceeding(&it_, '=')) {
        return Token::OP_XOR_ASSIGN;
      }
      return Token::OP_XOR;
    case '.':
      if (IsSucceeding(&it_, '.')) {
        Advance();
        if (IsSucceeding(&it_, '.')) {
          Advance();
          return Token::SPREAD;
        }
        return Token::INVALID;
      }
      return Token::DOT;
    case '!':
      return Token::OP_NOT;
    case ';':
      return Token::TERMINATE;
    case ':':
      return Token::COLON;
    case '?':
      return Token::QUESTION;
    case '`':
      return Token::BACK_QUOTE;
    case '\'':
    case '\"':
      return TokenizeStringLiteral();
    case '$':
      if (parser_state_.Is(State::IN_TEMPLATE_LITERAL)) {
        if (*(it_ + 1) == '{') {
          Advance();
          return Token::TEMPLATE_SUBSTITUTION;
        }
      }
    default:
      if (IsIdentifierStart(start_value)) {
        return TokenizeIdentifier();
      }
  }

  return Token::INVALID;
}

bool IsStartUnicodeEscapeSequence(Utf16CodePoint u) {
  return u == 'u';
}

bool IsStartAsciiEscapeSequence(Utf16CodePoint u) {
  return u == 'x';
}

bool IsStartEscapeSequence(Utf16String::iterator* it) {
  return IsStartUnicodeEscapeSequence(**it) || IsStartAsciiEscapeSequence(**it);
}

Utf16CodePoint DecodeEscapeSequence(Utf16String::iterator* it, bool* ok) {
  INVALIDATE(**it == '\\');
  (*it)++;  // Consume u or x
  auto lookahead = (*it) + 1;
  if (IsStartUnicodeEscapeSequence(*lookahead)) {
    (*it)++;
    (*it)++;
    return DecodeHexEscape(it, ok);
  } else if (IsStartAsciiEscapeSequence(*lookahead)) {
    (*it)++;
    return DecodeAsciiEscapeSequence(it, ok);
  }

  return Unicode::InvalidCodePoint();
}

Token::Type Parser::Tokenizer::TokenizeStringLiteral() {
  current_buffer_->clear();
  auto value = *it_;
  current_buffer_->push_back(value);
  u8 start = value.code();
  bool escaped = false;
  while (1) {
    Advance();
    value = *it_;
    current_buffer_->push_back(value);
    switch (value.code()) {
      case '\\':
        if (!escaped) {
          auto lookahead = it_ + 1;
          if (IsStartEscapeSequence(&lookahead)) {
            bool ok = true;
            value = DecodeEscapeSequence(&it_, &ok);
            if (!ok) {
              return Token::INVALID;
            }
          }
        }
        escaped = !escaped;
        break;
      case 0:
        return Token::INVALID;
      default:
        if (value == start) {
          if (!escaped) {
            return Token::STRING_LITERAL;
          }
        }
        // Nothing to do.
    }
  }
}

Token::Type Parser::Tokenizer::TokenizeIdentifier() {
  current_buffer_->clear();
  auto value = *it_;
  INVALIDATE(IsIdentifierStart(value));

  while (IsIdentifierChar(value)) {
    if (value == '\\') {
      std::vector<Utf16CodePoint> unicode_identifier;
      Advance();
      auto unicode_keyword = *it_;
      if (unicode_keyword == 'u') {
        Advance();
        bool ok = true;
        value = DecodeHexEscape(&it_, &ok);
        if (!ok) {
          return Token::INVALID;
        }
      }
    }
    current_buffer_->push_back(value);
    Advance();
    value = *it_;
  }

  return GetIdentifierType();
}

Token::Type Parser::Tokenizer::GetIdentifierType() {
  auto a = Utf16String(current_buffer_->data(), current_buffer_->size());
  auto maybe_keyword = a.ToUtf8String();
  const size_t input_length = maybe_keyword.size();
  const int min_length = 2;
  const int max_length = 10;
  if (input_length < min_length || input_length > max_length) {
    return Token::IDENTIFIER;
  }

  // Borrowed from v8 javascript engine.
  switch (maybe_keyword[0]) {
    default:
#define KEYWORD_GROUP_CASE(ch)                  \
      break;                                    \
    case ch:
#define KEYWORD(TOKEN, keyword)                                         \
      {                                                                 \
        const int keyword_length = sizeof(keyword) - 1;                 \
        static_assert(keyword_length >= min_length,                     \
                      "The length of the keyword must be greater than 2"); \
        static_assert(keyword_length <= max_length,                     \
                      "The length of the keyword mst be less than 10"); \
        if (input_length == keyword_length &&                           \
            maybe_keyword[1] == keyword[1] &&                           \
            (keyword_length <= 2 || maybe_keyword[2] == keyword[2]) &&  \
            (keyword_length <= 3 || maybe_keyword[3] == keyword[3]) &&  \
            (keyword_length <= 4 || maybe_keyword[4] == keyword[4]) &&  \
            (keyword_length <= 5 || maybe_keyword[5] == keyword[5]) &&  \
            (keyword_length <= 6 || maybe_keyword[6] == keyword[6]) &&  \
            (keyword_length <= 7 || maybe_keyword[7] == keyword[7]) &&  \
            (keyword_length <= 8 || maybe_keyword[8] == keyword[8]) &&  \
            (keyword_length <= 9 || maybe_keyword[9] == keyword[9])) {  \
          return Token::TOKEN;                                          \
        }                                                               \
      }
    KEYWORD_TOKEN_LIST(KEYWORD, KEYWORD_GROUP_CASE)
#undef KEYWORD
#undef KEYWORD_GROUP_CASE
  }

  return Token::IDENTIFIER;
}

Token::Type Parser::Tokenizer::TokenizeTemplateCharacters() {
  current_buffer_->clear();
  auto value = *it_;
  bool escaped = false;
  while (1) {
    auto lookahead = *(it_ + 1);
    switch (lookahead.code()) {
      case '\\':
        if (!escaped) {
          auto lookahead = it_ + 1;
          if (IsStartEscapeSequence(&lookahead)) {
            bool ok = true;
            value = DecodeEscapeSequence(&it_, &ok);
            if (!ok) {
              return Token::INVALID;
            }
          }
        }
        escaped = !escaped;
        break;
      case '$':
        if (!escaped && *(it_ + 2) == '{') {
          it_ += 2;
          return Token::TEMPLATE_CHARACTERS;
        }
        escaped = false;
        break;
      case '`':
        return Token::TEMPLATE_CHARACTERS;
    }
    current_buffer_->push_back(value);
    Advance();
    value = *it_;
  }
}

Token::Type Parser::Tokenizer::TokenizeRegExp() {
  auto value = *it_;
  bool escaped = false;
  while (1) {
    auto lookahead = *(it_ + 1);
    switch (lookahead.code()) {
      case '\\':
        escaped = !escaped;
        break;
      case '/':
        if (!escaped) {
          return Token::REGEXP_LITERAL;
        }
        continue;
    }
    Advance();
    value = *it_;
  }
}

void Parser::Tokenizer::SkipSingleLineComment() {
  while (1) {
    if (*it_ == '/') {
      auto next = *(it_ + 1);
      if (next == '\n' || next == '\r') {
        Advance();
        return;
      }
    } else if (*it_ == Unicode::InvalidCodePoint()) {
      return;
    }
    Advance();
  }
}

void Parser::Tokenizer::SkipMultiLineComment() {
  while (1) {
    if (*it_ == '*') {
      auto next = *(it_ + 1);
      if (next == '/') {
        Advance();
        return;
      }
    } else if (*it_ == Unicode::InvalidCodePoint()) {
      return;
    }
    Advance();
  }
}

bool Parser::Tokenizer::has_linebreak_before() const {
  return flag_.get(Flag::HAS_LINE_BREAK_BEFORE);
}

void Parser::Tokenizer::set_linebreak_before() {
  flag_.set(Flag::HAS_LINE_BREAK_BEFORE);
}

void Parser::Tokenizer::unset_linebreak_before() {
  flag_.set(Flag::HAS_LINE_BREAK_BEFORE);
}

bool Parser::Tokenizer::has_linebreak_after() const {
  return flag_.get(HAS_LINE_BREAK_AFTER);
}

void Parser::Tokenizer::set_linebreak_after() {
  flag_.set(HAS_LINE_BREAK_AFTER);
}

void Parser::Tokenizer::unset_linebreak_after() {
  flag_.unset(HAS_LINE_BREAK_AFTER);
}

Parser::Parser(Utf16String* sources, ErrorReporter* reporter)
    : reporter_(reporter),
      sources_(sources) {
  parser_state_();
  tokenizer_(sources, *parser_state_);
}

bool Parser::MatchStates(std::initializer_list<State> states) {
  for (auto &state : states) {
    if (parser_state_->Is(state)) {
      return true;
    }
  }

  return false;
}

void Parser::Parse(ParseType parse_type) {
  if (parse_type == ParseType::SCRIPT) {
    ParseScript();
  }
}

#ifdef DEBUG
// Logging current parse phase.
#define LOG_PHASE(name)                                                 \
  if (cur() != Token::INVALID) {                                        \
    phase_buffer_                                                       \
      << indent_                                                        \
      << "Enter "                                                       \
      << #name                                                          \
      << ": CurrentToken = "                                            \
      << ToStringCurrentToken();                                        \
  } else {                                                              \
    phase_buffer_                                                       \
      << indent_                                                        \
      << "Enter "                                                       \
      << #name                                                          \
      << ": CurrentToken = null";                                       \
  }                                                                     \
  phase_buffer_ << position().start_line_number() << '\n';              \
  indent_ += "  ";                                                      \
  auto err_size = reporter_->size();                                    \
  LUX_SCOPED([&]{                                                       \
    indent_ = indent_.substr(0, indent_.size() - 2);                    \
    if (this->cur() != Token::INVALID) {                                \
      phase_buffer_                                                     \
        << indent_                                                      \
        << "Exit "                                                      \
        << #name                                                        \
        << ": CurrentToken = "                                          \
        << ToStringCurrentToken()                                       \
        << (err_size != reporter_->size()?                              \
            "[Error!]": "");                                            \
    } else {                                                            \
      phase_buffer_                                                     \
        << indent_                                                      \
        << "Exit "                                                      \
        << #name                                                        \
        << ": CurrentToken = null"                                      \
        << (err_size != reporter_->size()?                              \
            "[Error!]": "");                                            \
    }                                                                   \
    phase_buffer_                                                       \
      << position().start_line_number() << '\n';                        \
  })

#define ENTER_PARSING                           \
  LOG_PHASE(__FUNCTION__);
#else
// Disabled.
#define LOG_PHASE(name)
#define ENTER_PARSING
#endif

Ast* Parser::ParseScript() {
  ENTER_PARSING;
  return ParseStatementList();
}

Ast* Parser::ParseStatementList() {
  ENTER_PARSING;
  auto statements = new (zone()) Statements();
  while (tokenizer_->HasMore()) {
    auto maybe_statement = ParseStatementListItem();
    INVALIDATE(maybe_statement->IsStatement());
    statements->Push(maybe_statement->ToStatement());
  }
  return statements;
}

Ast* Parser::ParseStatementListItem() {
  ENTER_PARSING;
  switch (cur()) {
    case Token::CLASS:
    case Token::CONST:
    case Token::FUNCTION:
      return ParseDeclaration();
    default:
      if (cur() == Token::IDENTIFIER) {
        auto v = value();
        if (v.IsAsciiEqual("async") && peek() == Token::FUNCTION) {
          return ParseDeclaration();
        } else if (v.IsAsciiEqual("let")) {
          return ParseDeclaration();
        }
      }
      return ParseStatement();
  }
}

Ast* Parser::ParseStatement() {
  ENTER_PARSING;
  switch (cur()) {
    case Token::LEFT_BRACE:
      return ParseBlockStatement();
    case Token::VAR:
      return ParseVariableStatement();
    case Token::IF:
      return ParseIfStatement();
    case Token::BREAK:
      return ParseBreakStatement();
    case Token::RETURN:
      return ParseReturnStatement();
    case Token::WITH:
      return ParseWithStatement();
    case Token::THROW:
      return ParseThrowStatement();
    case Token::TRY:
      return ParseTryStatement();
    case Token::DEBUGGER:
      return ParseDebuggerStatement();
    default:
      if (peek() == Token::COLON) {
        return ParseLabelledStatement();
      }
      return ParseExpressionStatement();
  }
}

Ast* Parser::ParseExpressionStatement() {
  ENTER_PARSING;
  auto ret = ParseExpression();
  if (cur() == Token::TERMINATE) {
    advance();
  }
  return ret;
}

Expression* Parser::ParseExpression() {
  ENTER_PARSING;
  auto ret = ParseAssignmentExpression();
  if (cur() == Token::COMMA) {
    auto expressions = new(zone()) Expressions();
    expressions->Push(ret->ToExpression());
    while (cur() == Token::COMMA) {
      auto assignment_expression = ParseAssignmentExpression();
      expressions->Push(assignment_expression->ToExpression());
    }
    return expressions;
  }

  return ret;
}

Expression* Parser::ParseAssignmentExpression() {
  ENTER_PARSING;
  switch (cur()) {
    case Token::YIELD:
      if (!MatchStates({IN_GENERATOR_FUNCTION, IN_ASYNC_GENERATOR_FUNCTION})) {
        REPORT_SYNTAX_ERROR(
            this, "yield only allowed in generator or async generator.");
      }
      return ParseYieldExpression();
    case Token::NEW:
      return ParseAssignmentExpressionLhs();
    case Token::AWAIT:
    case Token::DELETE:
    case Token::VOID:
    case Token::TYPEOF:
    case Token::OP_PLUS:
    case Token::OP_MINUS:
    case Token::OP_NOT:
      return ParseConditionalExpression();
    default:
      if (peek() == Token::ARROW_FUNCTION_GLYPH) {
        return ParseArrowFunction();
      } else if (value().IsAsciiEqual("async")) {
        Record();
        advance();
        auto next = peek();
        Restore();
        if (next != Token::TERMINATE) {
          return ParseAsyncArrowFunction();
        }
        return ParseIdentifier();
      } else if (cur() == Token::IDENTIFIER && peek() == Token::LEFT_PAREN) {
        return ParseAssignmentExpressionLhs();
      } else {
        switch (peek()) {
          case Token::QUESTION:
          case Token::OP_OR:
          case Token::OP_AND:
          case Token::OP_XOR:
          case Token::OP_LOGICAL_AND:
          case Token::OP_LOGICAL_OR:
          case Token::OP_EQ:
          case Token::OP_STRICT_EQ:
          case Token::OP_NOT_EQ:
          case Token::OP_STRICT_NOT_EQ:
          case Token::IN:
          case Token::INSTANCEOF:
          case Token::OP_SHIFT_LEFT:
          case Token::OP_SHIFT_RIGHT:
          case Token::OP_U_SHIFT_RIGHT:
          case Token::OP_LESS_THAN:
          case Token::OP_LESS_THAN_OR_EQ:
          case Token::OP_GREATER_THAN:
          case Token::OP_GREATER_THAN_EQ:
          case Token::OP_PLUS:
          case Token::OP_MINUS:
          case Token::OP_DIV:
          case Token::OP_MUL:
          case Token::OP_MOD:
          case Token::OP_POW:
            return ParseConditionalExpression();
          default:
            return ParseConditionalExpression();
        }
      }
  }
}

Expression* Parser::ParseAssignmentExpressionLhs() {
  ENTER_PARSING;
  auto left = ParseLeftHandSideExpression();
  if (cur() == Token::OP_ASSIGN || IsAssignmentOperator(cur())) {
    advance();
    auto right = ParseAssignmentExpression();
    return new (zone()) BinaryExpression(
        Token::OP_ASSIGN, left, right);
  }
  return left;
}

Expression* Parser::ParseConditionalExpression() {
  ENTER_PARSING;
  auto logical_or_exp = ParseLogicalORExpression();
  if (cur() == Token::QUESTION) {
    auto lhs = ParseAssignmentExpression();
    if (advance() != Token::COLON) {
      REPORT_SYNTAX_ERROR(this, "':' expected.");
    }
    auto rhs = ParseAssignmentExpression();
    return new(zone()) ConditionalExpression(
        logical_or_exp->ToExpression(),
        lhs->ToExpression(),
        rhs->ToExpression());
  }
  return logical_or_exp;
}

#define SIMPLE_BINARY_EXPRESSION_PARSER(Name, ChildName, ...) \
  Expression* Parser::Parse##Name##Expression() {               \
    ENTER_PARSING;                                              \
    auto child_exp = Parse##ChildName##Expression();            \
    if (Token::OneOf(cur(), {__VA_ARGS__})) {                   \
      auto t = cur();                                           \
      advance();                                                \
      auto rhs_exp = Parse##Name##Expression();                 \
      return new(zone()) BinaryExpression(                      \
          t, child_exp, rhs_exp);                               \
    }                                                           \
    return child_exp;                                           \
  }

SIMPLE_BINARY_EXPRESSION_PARSER(LogicalOR, LogicalAND, Token::OP_LOGICAL_OR)
SIMPLE_BINARY_EXPRESSION_PARSER(LogicalAND, BitwiseOR, Token::OP_LOGICAL_AND)
SIMPLE_BINARY_EXPRESSION_PARSER(BitwiseOR, BitwiseXOR, Token::OP_OR)
SIMPLE_BINARY_EXPRESSION_PARSER(BitwiseXOR, BitwiseAND, Token::OP_OR)
SIMPLE_BINARY_EXPRESSION_PARSER(BitwiseAND, Equality, Token::OP_AND)
SIMPLE_BINARY_EXPRESSION_PARSER(
    Equality, Relational,
    Token::OP_EQ, Token::OP_STRICT_EQ,
    Token::OP_NOT_EQ, Token::OP_STRICT_NOT_EQ)
SIMPLE_BINARY_EXPRESSION_PARSER(
    Relational, Shift,
    Token::OP_GREATER_THAN,
    Token::OP_GREATER_THAN_EQ,
    Token::OP_LESS_THAN,
    Token::OP_LESS_THAN_OR_EQ,
    Token::INSTANCEOF,
    Token::IN)
SIMPLE_BINARY_EXPRESSION_PARSER(
    Shift, Additive,
    Token::OP_SHIFT_LEFT,
    Token::OP_SHIFT_RIGHT,
    Token::OP_U_SHIFT_RIGHT)
SIMPLE_BINARY_EXPRESSION_PARSER(
    Additive, Multiplicative, Token::OP_PLUS, Token::OP_MINUS)
SIMPLE_BINARY_EXPRESSION_PARSER(
    Multiplicative, Exponentiation, Token::OP_MUL, Token::OP_DIV, Token::OP_MOD)
SIMPLE_BINARY_EXPRESSION_PARSER(Exponentiation, Unary, Token::OP_POW)

Expression* Parser::ParseUnaryExpression() {
  ENTER_PARSING;
  switch (peek()) {
    case Token::DELETE:  // FALL_THROUGH
    case Token::VOID:  // FALL_THROUGH
    case Token::TYPEOF:  // FALL_THROUGH
    case Token::OP_PLUS:  // FALL_THROUGH
    case Token::OP_MINUS:  // FALL_THROUGH
    case Token::OP_TILDE:  // FALL_THROUGH
    case Token::OP_NOT: {  // FALL_THROUGH
      advance();
      auto n = ParseUnaryExpression();
      return new(zone()) UnaryExpression(
          UnaryExpression::PRE, cur(), n);
    }
    case Token::AWAIT:
      advance();
      return ParseAwaitExpression();
    default:
      return ParseUpdateExpression();
  }
}

Expression* Parser::ParseUpdateExpression() {
  ENTER_PARSING;
  switch (peek()) {
    case Token::OP_INCREMENT:
    case Token::OP_DECREMENT: {
      advance();
      auto n = ParseLeftHandSideExpression();
      return new(zone()) UnaryExpression(
          UnaryExpression::PRE, cur(), n);
    }
    default: {
      auto n = ParseLeftHandSideExpression();
      switch (peek()) {
        case Token::OP_INCREMENT:
        case Token::OP_DECREMENT:
          advance();
          return new(zone()) UnaryExpression(
              UnaryExpression::POST, cur(), n);
        default:
          return n;
      }
    }
  }
}

Expression* Parser::ParseLeftHandSideExpression() {
  ENTER_PARSING;
  switch (peek()) {
    case Token::NEW: {
      Record();
      LUX_SCOPED([this]() { this->Restore(); });
      advance();
      if (peek() == Token::DOT) {
        return ParseCallExpression();
      }
      return ParseNewExpression();
    }
    default:
      return ParseCallExpression();
  }
}

Expression* Parser::ParseNewExpression() {
  ENTER_PARSING;
  if (peek() == Token::NEW) {
    advance();
    auto callee = ParseNewExpression();
    return new (zone()) CallExpression(
        Receiver::NEW, callee);
  }
  return ParseMemberExpression();
}

Expression* Parser::ParseCallExpression() {
  ENTER_PARSING;
  switch (peek()) {
    case Token::SUPER: {
      Record();
      advance();
      if (peek() == Token::LEFT_PAREN) {
        Restore();
        auto n = ParseSuperCall();
        return ParsePropertyAccessPostExpression(
            n, Receiver::SUPER,
            Allowance(Allowance::CALL | Allowance::TEMPLATE));
      }
      Restore();
    }
    default: {
      auto n = ParseCoverCallExpressionAndAsyncArrowHead()->ToExpressions();
      auto callexp = new(zone()) CallExpression(
          Receiver::EXPRESSION,
          n->at(0), n->at(1));
      return ParsePropertyAccessPostExpression(
          callexp, Receiver::EXPRESSION,
          Allowance(Allowance::CALL | Allowance::TEMPLATE));
    }
  }
}

Expression* Parser::ParsePropertyAccessPostExpression(
    Expression* pre, Receiver::Type receiver_type,
    Parser::Allowance allowance, bool error_if_default) {
  switch (cur()) {
    case Token::LEFT_PAREN: {
      if (!allowance.is_call_allowed()) {
        REPORT_SYNTAX_ERROR(this, "Unexpected '(' found.");
      }
      auto a = ParseArguments();
      return new(zone()) CallExpression(
          Receiver::SUPER, pre, a);
    }
    case Token::LEFT_BRACKET: {
      advance();
      auto n = ParseExpression();
      EXPECT(this, cur(), Token::RIGHT_BRACKET, ']');
      return new(zone()) PropertyAccessExpression(
          PropertyAccessExpression::AccessType::ELEMENT,
          receiver_type, pre, n);
    }
    case Token::DOT: {
      advance();
      auto n = ParseExpression();
      return new(zone()) PropertyAccessExpression(
          PropertyAccessExpression::DOT, receiver_type,
          pre, n);
    }
    case Token::BACK_QUOTE: {
      if (!allowance.is_template_allowed()) {
        REPORT_SYNTAX_ERROR(this, "Unexpected '`' found.");
      }
      auto t = ParseTemplateLiteral();
      return new(zone()) CallExpression(
          Receiver::TEMPLATE, pre, t);
    }
    default:
      if (error_if_default) {
        REPORT_SYNTAX_ERROR(
            this, "Unexpected " << ToStringCurrentToken() << " found.");
      }
      return pre;
  }
}

Expression* Parser::ParseCoverCallExpressionAndAsyncArrowHead() {
  ENTER_PARSING;
  auto n = ParseMemberExpression();
  auto a = ParseArguments();
  auto ret = new(zone()) Expressions({n->ToExpression(), a->ToExpression()});
  return ret;
}

Expression* Parser::ParseMemberExpression() {
  ENTER_PARSING;
  if (cur() == Token::SUPER) {
    auto n = ParsePropertyAccessPostExpression(
        nullptr, Receiver::SUPER, Allowance(), true);
    return ParsePropertyAccessPostExpression(
        n, Receiver::EXPRESSION,
        Allowance(Allowance::TEMPLATE));
  }

  if (cur() == Token::NEW) {
    if (peek() == Token::DOT) {
      advance();
      if (peek() == Token::IDENTIFIER) {
        if (peek_value().IsAsciiEqual("target")) {
          return new(zone()) PropertyAccessExpression(
              PropertyAccessExpression::DOT, Receiver::NEW,
              nullptr, nullptr);
        }
        REPORT_SYNTAX_ERROR(
            this, "new.target? but got " << peek_value().ToUtf8String());
      }
      REPORT_SYNTAX_ERROR(
          this, "new.target? identifier 'target' expected.");
    }
    auto m = ParseMemberExpression();
    EXPECT(this, cur(), Token::LEFT_PAREN, '(');
    auto args = ParseArguments();
    return new(zone()) CallExpression(
        Receiver::NEW, m, args);
  }

  auto n = ParsePrimaryExpression();
  return ParsePropertyAccessPostExpression(
      n, Receiver::EXPRESSION,
      Allowance(Allowance::TEMPLATE));
}

Expression* Parser::ParsePrimaryExpression() {
  ENTER_PARSING;
  switch (cur()) {
    case Token::THIS: {
      advance();
      return new(zone()) Literal(Token::THIS);
    }
    case Token::LEFT_BRACKET:
      return ParseArrayLiteral();
    case Token::LEFT_BRACE:
      return ParseObjectLiteral(Allowance());
    case Token::FUNCTION:
      if (peek() == Token::OP_MUL) {
        return ParseGeneratorExpression();
      }
      return ParseFunctionExpression();
    case Token::CLASS:
      return ParseClassExpression();
    case Token::IDENTIFIER:
      if (value().IsAsciiEqual("async")) {
        if (peek() != Token::TERMINATE) {
          return ParseAsyncFunctionExpression();
        }
      }
      return ParseIdentifierReference();
    case Token::NULL_VALUE:
    case Token::TRUE:
    case Token::FALSE:
    case Token::NUMERIC_LITERAL:
    case Token::STRING_LITERAL:
      return ParseLiteral();
    case Token::BACK_QUOTE:
      return ParseTemplateLiteral();
    case Token::OP_DIV:
      return ParseRegularExpression();
    default:
      return ParseCoverParenthesizedExpressionAndArrowParameterList();
  }
}

Expression* Parser::ParseLiteral() {
  ENTER_PARSING;
  switch (cur()) {
    case Token::NULL_VALUE:
    case Token::TRUE:
    case Token::FALSE:
    case Token::NUMERIC_LITERAL:
    case Token::STRING_LITERAL: {
      auto t = cur();
      advance();
      return new(zone()) Literal(t);
    }
    default:
      UNREACHABLE();
  }
  return nullptr;
}

Expression* Parser::ParseIdentifierReference() {
  ENTER_PARSING;
  switch (cur()) {
    case Token::IDENTIFIER:
      advance();
      return new(zone()) Literal(Token::IDENTIFIER, value());
    case Token::YIELD:
      advance();
      return new(zone()) Literal(Token::IDENTIFIER, value());
    case Token::AWAIT:
      advance();
      return new(zone()) Literal(Token::IDENTIFIER, value());
    default:
      UNREACHABLE();
  }
  return nullptr;
}

Expression* Parser::ParseArrayLiteral(Parser::Allowance allowance) {
  ENTER_PARSING;
  INVALIDATE(cur() == Token::LEFT_BRACKET);
  advance();
  auto array = new(zone()) StructuralLiteral(
      StructuralLiteral::ARRAY);
  while (!Token::OneOf(cur(), { Token::RIGHT_BRACKET, Token::INVALID })) {
    if (cur() == Token::COMMA) {
      array->Push(&kElision);
      advance();
    }
    if (cur() == Token::SPREAD) {
      array->Push(ParseSpreadElement());
    } else {
      array->Push(ParseAssignmentExpression());
    }
  }
  EXPECT(this, cur(), Token::RIGHT_BRACKET, ']');
  return array;
}

Expression* Parser::ParseSpreadElement() {
  ENTER_PARSING;
  INVALIDATE(cur() == Token::SPREAD);
  advance();
  auto exp = ParseAssignmentExpression();
  return new(zone()) UnaryExpression(
      UnaryExpression::PRE, Token::SPREAD, exp);
}

Expression* Parser::ParseObjectLiteral(Parser::Allowance allowance) {
  ENTER_PARSING;
  INVALIDATE(cur() == Token::LEFT_BRACE);
  advance();
  auto object = new(zone()) StructuralLiteral(
      StructuralLiteral::OBJECT);
  if (cur() == Token::RIGHT_BRACE) {
    return object;
  }

  while (!Token::OneOf(cur(), { Token::RIGHT_BRACE, Token::INVALID })) {
    object->Push(ParseObjectLiteralProperty(allowance));
  }

  return object;
}

Expression* Parser::ParseObjectLiteralProperty(
    Parser::Allowance allowance) {

  Expression* key = nullptr;
  bool is_computed_property_name = false;
  if (Token::OneOf(cur(), {
        Token::NUMERIC_LITERAL,
        Token::IDENTIFIER,
        Token::STRING_LITERAL,
        Token::LEFT_BRACKET })) {
    key = cur() == Token::IDENTIFIER? ParseIdentifierReference()
      : ParsePropertyName();

    if (cur() == Token::OP_ASSIGN) {
      if (!allowance.is_binding_pattern_allowed()
          || is_computed_property_name) {
        REPORT_SYNTAX_ERROR(this, "Unexpected '=' detected.");
      }
      auto key = ParseIdentifierReference();
      advance();
      auto value = ParseAssignmentExpression();
      return new(zone()) ObjectPropertyExpression(key, nullptr, value);
    } else if (Token::OneOf(
        cur(), { Token::COMMA, Token::RIGHT_BRACE })) {
      if (!allowance.is_binding_pattern_allowed()
          || is_computed_property_name) {
        REPORT_SYNTAX_ERROR(this, "':' expected.");
      }
      return ParseIdentifierReference();
    } else {
      if (allowance.is_binding_pattern_allowed()) {
        REPORT_SYNTAX_ERROR(this, "binding pattern expected.");
      }
      return ParseMethodDefinition();
    }
  } else {
    REPORT_SYNTAX_ERROR(
        this, "Property name must be one of "
        "'identifier', 'string literal', 'numeric literal', "
        "or 'computed property' but got " << ToStringCurrentToken());
  }

  if (cur() != Token::COLON) {
    if (!allowance.is_binding_pattern_allowed()
        || is_computed_property_name) {
      REPORT_SYNTAX_ERROR(this, "':' expected.");
    }
  }

  advance();
  auto value = ParseAssignmentExpression();
  if (allowance.is_binding_pattern_allowed()) {
    if (!value->IsStructuralLiteral() &&
        value->IsLiteral()) {
      REPORT_SYNTAX_ERROR(
          this, "Identifier or binding pattern expected.");
    } else if (value->IsLiteral() &&
               !value->ToLiteral()->Is(Token::IDENTIFIER)) {
      REPORT_SYNTAX_ERROR(
          this, "Identifier expected.");
    }
  }

  return new(zone()) ObjectPropertyExpression(key, value);
}

Expression* Parser::ParseMethodDefinition() {
  ENTER_PARSING;
  bool is_getter = false;
  bool is_setter = false;

  if (((is_getter = value().IsAsciiEqual("get")) ||
       (is_setter = value().IsAsciiEqual("set")))
      && Token::OneOf(peek(), { Token::IDENTIFIER, Token::LEFT_BRACKET })) {
    advance();
  }

  switch (cur()) {
    case Token::OP_MUL:
      return ParseGeneratorMethod();
    case Token::STRING_LITERAL:
    case Token::NUMERIC_LITERAL:
    case Token::LEFT_BRACKET:
    case Token::IDENTIFIER: {
      Expression* name = nullptr;
      Expression* formal_parameters;

      if (cur() == Token::IDENTIFIER
          && value().IsAsciiEqual("async")) {
        return ParseAsyncMethod();
      } else  {
        name = ParsePropertyName();
      }

      EXPECT(this, cur(), Token::LEFT_PAREN, '(');
      if (is_getter) {
        if (cur() != Token::RIGHT_PAREN) {
          REPORT_SYNTAX_ERROR(
              this, "Getter must not have any formal parameters.");
        }
        advance();
        formal_parameters = new(zone()) Expressions();
      } else if (is_setter) {
        if (cur() == Token::RIGHT_PAREN) {
          REPORT_SYNTAX_ERROR(
              this, "Setter must have exactly one formal parameter.");
        }
        formal_parameters = ParsePropertySetParameterList();
        if (cur() != Token::RIGHT_PAREN) {
          REPORT_SYNTAX_ERROR(
              this, "Setter must have exactly one formal parameter.");
        }
        advance();
      } else {
        formal_parameters = ParseFormalParameters();
      }

      EXPECT(this, cur(), Token::LEFT_BRACE, '{');
      auto b = ParseFunctionBody();
      EXPECT(this, cur(), Token::RIGHT_BRACE, '}');
      auto type = is_getter? Function::GETTER: Function::SETTER;

      auto fn = new(zone()) FunctionExpression(type, formal_parameters, b);
      return new(zone()) ObjectPropertyExpression(name, fn);
    }
    default:
      UNREACHABLE();
  }

  return nullptr;
}

Expression* Parser::ParsePropertyName() {
  ENTER_PARSING;
  switch (cur()) {
    case Token::IDENTIFIER:
      return ParseIdentifier();
    case Token::STRING_LITERAL:
    case Token::NUMERIC_LITERAL:
      return ParseLiteral();
    default: {
      EXPECT(this, cur(), Token::RIGHT_BRACKET, '[');
      auto ret = ParseAssignmentExpression();
      EXPECT(this, cur(), Token::RIGHT_BRACKET, ']');
      return ret;
    }
  }
}

Expression* Parser::ParseFormalParameters() {
  ENTER_PARSING;
  if (cur() == Token::SPREAD) {
    return ParseFunctionRestParameter();
  }
  auto p = ParseFormalParameterList();
  auto exprs = p->ToExpressions();
  if (cur() == Token::COMMA) {
    advance();
    if (cur() == Token::SPREAD) {
      exprs->Push(ParseFunctionRestParameter());
    }
  }
  return exprs;
}

Expression* Parser::ParseFormalParameterList() {
  ENTER_PARSING;
  auto exprs = new(zone()) Expressions();
  while (1) {
    switch (cur()) {
      case Token::IDENTIFIER:
      case Token::LEFT_BRACE:
      case Token::LEFT_BRACKET:
        exprs->Push(ParseBindingElement());
      case Token::COMMA:
        advance();
      default:
        return exprs;
    }
  }
}

Expression* Parser::ParseFunctionRestParameter() {
  ENTER_PARSING;
  EXPECT(this, cur(), Token::SPREAD, "...");
  advance();
  switch (cur()) {
    case Token::LEFT_BRACE:
    case Token::LEFT_BRACKET:
      return ParseBindingPattern();
    default:
      return ParseSingleNameBinding(Allowance(
          Allowance::INITIALIZER));
  }
}

Expression* Parser::ParseBindingElement() {
  ENTER_PARSING;
  switch (cur()) {
    case Token::LEFT_BRACKET:
    case Token::LEFT_BRACE:
      return ParseBindingPattern();
    default:
      return ParseSingleNameBinding(Allowance(
          Allowance::INITIALIZER));
  }
}

Expression* Parser::ParseSingleNameBinding(Allowance allowance) {
  ENTER_PARSING;
  Expression* identifier = nullptr;
  switch (cur()) {
    case Token::IDENTIFIER:
    case Token::YIELD:
    case Token::AWAIT:
      identifier = new(zone()) Literal(Token::IDENTIFIER, value());
    default:
      REPORT_SYNTAX_ERROR(this, "Identifier expected.");
  }

  INVALIDATE(identifier != nullptr);
  if (allowance.is_initializer_allowed() &&
      cur() == Token::OP_ASSIGN) {
    advance();
    auto i = ParseAssignmentExpression();
    return new(zone()) BinaryExpression(
        Token::OP_ASSIGN, identifier, i);
  }
  return identifier;
}

Expression* Parser::ParseBindingPattern() {
  ENTER_PARSING;
  switch (cur()) {
    case Token::LEFT_BRACE:
      return ParseObjectLiteral(Allowance(
          Allowance::BINDING_PATTERN));
    default:
      INVALIDATE(cur() == Token::LEFT_BRACKET);
      return ParseArrayLiteral(Allowance(
          Allowance::BINDING_PATTERN));
  }
}

Expression* Parser::ParseArguments() {
  ENTER_PARSING;
  EXPECT(this, cur(), Token::LEFT_PAREN, '(');
  auto exprs = new(zone()) Expressions();
  while (1) {
    if (cur() == Token::SPREAD) {
      advance();
      auto u = new(zone()) UnaryExpression(
          UnaryExpression::PRE, Token::SPREAD, ParseAssignmentExpression());
      exprs->Push(u);
    } else {
      exprs->Push(ParseAssignmentExpression());
    }

    if (cur() == Token::COMMA) {
      if (peek() == Token::LEFT_PAREN) {
        REPORT_SYNTAX_ERROR(this, "Extra ',' found.");
      }
      advance();
    } else {
      break;
    }
  }
  EXPECT(this, cur(), Token::LEFT_PAREN, ')');
  return exprs;
}

Expression* Parser::ParseIdentifier() {
  ENTER_PARSING;
  return new(zone()) Literal(Token::IDENTIFIER, value());
}

Expression* Parser::ParseArrowFunction() {
  ENTER_PARSING;
  auto p = ParseArrowParameters();
  if (cur() != Token::TERMINATE ||
      has_linebreak_before()) {
    EXPECT(this, cur(), Token::ARROW_FUNCTION_GLYPH, "=>");
    auto body = ParseConciseBody();
    return new(zone()) ArrowFunctionExpression(
        Function::NORMAL, p, body);
  }
  return p;
}

Expression* Parser::ParseArrowParameters() {
  ENTER_PARSING;
  switch (cur()) {
    case Token::LEFT_PAREN:
      return ParseCoverParenthesizedExpressionAndArrowParameterList();
    default: {
      auto exprs = new(zone()) Expressions();
      exprs->Push(ParseSingleNameBinding());
      return exprs;
    }
  }
}

Expression* Parser::ParseCoverParenthesizedExpressionAndArrowParameterList() {
  ENTER_PARSING;
  EXPECT(this, cur(), Token::LEFT_PAREN, '(');
  auto exprs = new(zone()) Expressions();
  if (cur() == Token::RIGHT_PAREN) {
    advance();
    return exprs;
  }

  while (1) {
    if (cur() == Token::SPREAD) {
      advance();
      Expression* e;
      if (Token::OneOf(cur(), { Token::LEFT_BRACKET, Token::LEFT_BRACE })) {
        e = ParseBindingPattern();
      } else {
        e = ParseSingleNameBinding();
      }
      auto u = new(zone()) UnaryExpression(
          UnaryExpression::PRE, Token::SPREAD, e);
      exprs->Push(u);
    } else {
      exprs->Push(ParseExpression());
    }

    if (cur() != Token::COMMA) {
      break;
    } else {
      advance();
      if (peek() == Token::RIGHT_PAREN) {
        break;
      }
    }
  }
  EXPECT(this, cur(), Token::LEFT_PAREN, ')');
  return exprs;
}

Expression* Parser::ParseAsyncFunctionExpression() {return nullptr;}

Ast* Parser::ParseConciseBody() {
  ENTER_PARSING;
  if (peek() != Token::LEFT_BRACE) {
    return ParseAssignmentExpression();
  }
  advance();
  auto n = ParseFunctionBody();
  EXPECT(this, cur(), Token::RIGHT_BRACE, '}');
  return n;
}

Ast* Parser::ParseScriptBody() {return nullptr;}
Ast* Parser::ParseModule() {return nullptr;}
Ast* Parser::ParseModuleBody() {return nullptr;}
Ast* Parser::ParseModuleItemList() {return nullptr;}
Ast* Parser::ParseModuleItem() {return nullptr;}
Ast* Parser::ParseImportDeclaration() {return nullptr;}
Ast* Parser::ParseImportClause() {return nullptr;}
Ast* Parser::ParseImportedDefaultBinding() {return nullptr;}
Ast* Parser::ParseNameSpaceImport() {return nullptr;}
Ast* Parser::ParseNamedImports() {return nullptr;}
Ast* Parser::ParseFromClause() {return nullptr;}
Ast* Parser::ParseImportsList() {return nullptr;}
Ast* Parser::ParseImportSpecifier() {return nullptr;}
Ast* Parser::ParseModuleSpecifier() {return nullptr;}
Ast* Parser::ParseImportedBinding() {return nullptr;}
Ast* Parser::ParseExportDeclaration() {return nullptr;}
Ast* Parser::ParseExportClause() {return nullptr;}
Ast* Parser::ParseExportsList() {return nullptr;}
Ast* Parser::ParseExportSpecifier() {return nullptr;}
Expression* Parser::ParseAsyncArrowBindingIdentifier() {return nullptr;}
Expression* Parser::ParseLabelIdentifier() {return nullptr;}
Expression* Parser::ParseParenthesizedExpression() {return nullptr;}
Expression* Parser::ParseElementList() {return nullptr;}
Expression* Parser::ParsePropertyDefinitionList() {return nullptr;}
Expression* Parser::ParsePropertyDefinition() {return nullptr;}
Expression* Parser::ParseCoverInitializedName() {return nullptr;}
Expression* Parser::ParseTemplateLiteral() {return nullptr;}
Expression* Parser::ParseTemplateSpans() {return nullptr;}
Expression* Parser::ParseTemplateMiddleList() {return nullptr;}
Expression* Parser::ParseNewTarget() {return nullptr;}
Expression* Parser::ParseCallMemberExpression() {return nullptr;}
Expression* Parser::ParseSuperCall() {return nullptr;}
Expression* Parser::ParseArgumentList() {return nullptr;}
Ast* Parser::ParseDeclaration() {return nullptr;}
Ast* Parser::ParseHoistableDeclaration() {return nullptr;}
Ast* Parser::ParseBreakableStatement() {return nullptr;}
Ast* Parser::ParseBlockStatement() {return nullptr;}
Ast* Parser::ParseBlock() {return nullptr;}
Ast* Parser::ParseLexicalDeclaration() {return nullptr;}
Ast* Parser::ParseLexicalBinding() {return nullptr;}
Ast* Parser::ParseVariableStatement() {return nullptr;}
Ast* Parser::ParseVariableDeclarationList() {return nullptr;}
Ast* Parser::ParseVariableDeclaration() {return nullptr;}
Ast* Parser::ParseIfStatement() {return nullptr;}
Ast* Parser::ParseIterationStatement() {return nullptr;}
Ast* Parser::ParseForDeclaration() {return nullptr;}
Ast* Parser::ParseForBinding() {return nullptr;}
Ast* Parser::ParseContinueStatement() {return nullptr;}
Ast* Parser::ParseBreakStatement() {return nullptr;}
Ast* Parser::ParseReturnStatement() {return nullptr;}
Ast* Parser::ParseWithStatement() {return nullptr;}
Ast* Parser::ParseSwitchStatement() {return nullptr;}
Ast* Parser::ParseCaseBlock() {return nullptr;}
Ast* Parser::ParseCaseClauses() {return nullptr;}
Ast* Parser::ParseCaseClause() {return nullptr;}
Ast* Parser::ParseDefaultClause() {return nullptr;}
Ast* Parser::ParseLabelledStatement() {return nullptr;}
Ast* Parser::ParseLabelledItem() {return nullptr;}
Ast* Parser::ParseThrowStatement() {return nullptr;}
Ast* Parser::ParseTryStatement() {return nullptr;}
Ast* Parser::ParseCatch() {return nullptr;}
Ast* Parser::ParseFinally() {return nullptr;}
Ast* Parser::ParseCatchParameter() {return nullptr;}
Ast* Parser::ParseDebuggerStatement() {return nullptr;}
Ast* Parser::ParseFunctionDeclaration() {return nullptr;}
Expression* Parser::ParseFunctionExpression() {return nullptr;}
Statement* Parser::ParseFunctionBody() {return nullptr;}
Ast* Parser::ParseFunctionStatementList() {return nullptr;}
Expression* Parser::ParseArrowFormalParameters() {return nullptr;}
Expression* Parser::ParseAsyncArrowFunction() {return nullptr;}
Ast* Parser::ParseAsyncConciseBody() {return nullptr;}
Ast* Parser::ParseAsyncArrowHead() {return nullptr;}
Expression* Parser::ParsePropertySetParameterList() {return nullptr;}
Expression* Parser::ParseGeneratorMethod() {return nullptr;}
Ast* Parser::ParseGeneratorDeclaration() {return nullptr;}
Expression* Parser::ParseRegularExpression() {return nullptr;}
Expression* Parser::ParseGeneratorExpression() {return nullptr;}
Ast* Parser::ParseGeneratorBody() {return nullptr;}
Expression* Parser::ParseYieldExpression() {return nullptr;}
Expression* Parser::ParseAsyncMethod() {return nullptr;}
Ast* Parser::ParseAsyncFunctionDeclaration() {return nullptr;}
Ast* Parser::ParseAsyncFunctionBody() {return nullptr;}
Expression* Parser::ParseAwaitExpression() {return nullptr;}
Ast* Parser::ParseClassDeclaration() {return nullptr;}
Expression* Parser::ParseClassExpression() {return nullptr;}
Ast* Parser::ParseClassTail() {return nullptr;}
Ast* Parser::ParseClassHeritage() {return nullptr;}
Ast* Parser::ParseClassBody() {return nullptr;}
Ast* Parser::ParseClassElementList() {return nullptr;}
Ast* Parser::ParseClassElement() {return nullptr;}
}  // namespace lux
