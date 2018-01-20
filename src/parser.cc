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

#include <iostream>
#include <sstream>
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

#define EXPECT(parser, n, token, expect)        \
  if (n != token) {                             \
    REPORT_SYNTAX_ERROR(                        \
        parser, "'" << expect << "' expected"); \
  } else {                                      \
    advance();                                  \
  }

using Token = Parser::Token;

Token Parser::Tokenizer::Next() {
  current_buffer_ = &buffer_;
  current_position_ = &position_;
  if (lookahead_ != Token::INVALID) {
    auto ret = lookahead_;
    lookahead_ = Token::INVALID;
    return ret;
  }
  return token_ = Tokenize();
}

Token Parser::Tokenizer::Peek() {
  current_buffer_ = &lookahead_buffer_;
  current_position_ = &lookahead_position_;
  if (lookahead_ != Token::INVALID) {
    return lookahead_;
  }
  lookahead_ = Tokenize();
  return lookahead_;
}

Token Parser::Tokenizer::Current() {
  return token_;
}

Utf16CodePoint Parser::Tokenizer::Advance() {
  it_++;
  switch (it_->code()) {
    case '\r':
      if (*(it_ + 1) == '\n') {
        Advance();
      }
    case '\n':
      current_position_->set_end_line_number(
          position_.end_line_number() + 1);
      current_position_->set_end_col(0);
      break;
    default:
      current_position_->set_end_col(position_.end_col() + 1);
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

Token Parser::Tokenizer::Tokenize() {
  current_position_->set_start_col(current_position_->end_col() + 1);
  current_position_->set_start_line_number(
      current_position_->end_line_number());
  auto start_value = *it_;
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

Token Parser::Tokenizer::TokenizeStringLiteral() {
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

Token Parser::Tokenizer::TokenizeIdentifier() {
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

  return Token::IDENTIFIER;
}

Token Parser::Tokenizer::TokenizeTemplateCharacters() {
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

Token Parser::Tokenizer::TokenizeRegExp() {
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

Parser::Parser(Utf16String* sources, ErrorReporter* reporter)
    : reporter_(reporter),
      sources_(sources) {
  parser_state_();
  tokenizer_(sources, *parser_state_);
}

bool Parser::OneOf(Token base, std::initializer_list<Token> candidate) {
  for (auto &t : candidate) {
    if (t == base) {
      return true;
    }
  }
  return false;
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
    case CLASS:
    case CONST:
    case FUNCTION:
      return ParseDeclaration();
    default:
      if (cur() == IDENTIFIER) {
        auto v = value();
        if (v.IsAsciiEqual("async") && peek() == FUNCTION) {
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
    case LEFT_BRACE:
      return ParseBlockStatement();
    case VAR:
      return ParseVariableStatement();
    case IF:
      return ParseIfStatement();
    case BREAK:
      return ParseBreakStatement();
    case RETURN:
      return ParseReturnStatement();
    case WITH:
      return ParseWithStatement();
    case THROW:
      return ParseThrowStatement();
    case TRY:
      return ParseTryStatement();
    case DEBUGGER:
      return ParseDebuggerStatement();
    default:
      if (peek() == COLON) {
        return ParseLabelledStatement();
      }
      return ParseExpressionStatement();
  }
}

Ast* Parser::ParseExpressionStatement() {
  ENTER_PARSING;
  auto ret = ParseExpression();
  if (cur() == TERMINATE) {
    advance();
  }
  return ret;
}

Expression* Parser::ParseExpression() {
  ENTER_PARSING;
  auto ret = ParseAssignmentExpression();
  if (cur() == COMMA) {
    auto expressions = new(zone()) Expressions();
    expressions->Push(ret->ToExpression());
    while (cur() == COMMA) {
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
    case YIELD:
      if (!MatchStates({IN_GENERATOR_FUNCTION, IN_ASYNC_GENERATOR_FUNCTION})) {
        REPORT_SYNTAX_ERROR(this,
                            "yield only allowed in generator or async generator.");
      }
      return ParseYieldExpression();
    case NEW:
      return ParseAssignmentExpressionLhs();
    case AWAIT:
    case DELETE:
    case VOID:
    case TYPEOF:
    case OP_PLUS:
    case OP_MINUS:
    case OP_NOT:
      return ParseConditionalExpression();
    default:
      if (peek() == ARROW_FUNCTION_GLYPH) {
        return ParseArrowFunction();
      } else if (value().IsAsciiEqual("async")) {
        Record();
        advance();
        auto next = peek();
        Restore();
        if (next != TERMINATE) {
          return ParseAsyncArrowFunction();
        }
        return ParseIdentifier();
      } else if (cur() == IDENTIFIER && peek() == LEFT_PAREN) {
        return ParseAssignmentExpressionLhs();
      } else {
        switch (peek()) {
          case QUESTION:
          case OP_OR:
          case OP_AND:
          case OP_XOR:
          case OP_LOGICAL_AND:
          case OP_LOGICAL_OR:
          case OP_EQ:
          case OP_STRICT_EQ:
          case OP_NOT_EQ:
          case OP_STRICT_NOT_EQ:
          case IN:
          case INSTANCEOF:
          case OP_SHIFT_LEFT:
          case OP_SHIFT_RIGHT:
          case OP_U_SHIFT_RIGHT:
          case OP_LESS_THAN:
          case OP_LESS_THAN_OR_EQ:
          case OP_GREATER_THAN:
          case OP_GREATER_THAN_EQ:
          case OP_PLUS:
          case OP_MINUS:
          case OP_DIV:
          case OP_MUL:
          case OP_MOD:
          case OP_POW:
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
        left->ToExpression(), right->ToExpression());
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
    if (OneOf(peek(), {__VA_ARGS__})) {                         \
      advance();                                                \
      auto rhs_exp = Parse##Name##Expression();                 \
      return new(zone()) BinaryExpression(                      \
          child_exp, rhs_exp);                                  \
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
        CallExpression::NEW, callee);
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
        return ParsePostCallExpression(n);
      }
      Restore();
    }
    default: {
      auto n = ParseCoverCallExpressionAndAsyncArrowHead()->ToExpressions();
      auto callexp = new(zone()) CallExpression(
          CallExpression::NORMAL,
          n->at(0), n->at(1));
      return ParsePostCallExpression(callexp);
    }
  }
}

Expression* Parser::ParsePostCallExpression(Expression* callexp) {
  switch (cur()) {
    case Token::LEFT_PAREN: {
      auto a = ParseArguments();
      return new(zone()) CallExpression(
          CallExpression::SUPER, callexp, a);
    }
    case Token::LEFT_BRACKET: {
      advance();
      auto n = ParseExpression();
      EXPECT(this, cur(), Token::RIGHT_BRACKET, ']');
      return new(zone()) PropertyAccessExpression(
          PropertyAccessExpression::ELEMENT, callexp, n);
    }
    case Token::DOT: {
      advance();
      auto n = ParseExpression();
      return new(zone()) PropertyAccessExpression(
          PropertyAccessExpression::DOT, callexp, n);
    }
    case Token::BACK_QUOTE: {
      auto t = ParseTemplateLiteral();
      return new(zone()) CallExpression(
          CallExpression::TEMPLATE, callexp, t);
    }
    default:
      return callexp;
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
  return nullptr;
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

Expression* Parser::ParseIdentifierReference() {return nullptr;}
Expression* Parser::ParseBindingIdentifier() {return nullptr;}
Expression* Parser::ParseIdentifier() {return nullptr;}
Expression* Parser::ParseAsyncArrowBindingIdentifier() {return nullptr;}
Expression* Parser::ParseLabelIdentifier() {return nullptr;}
Expression* Parser::ParsePrimaryExpression() {return nullptr;}
Expression* Parser::ParseCoverParenthesizedExpressionAndArrowParameterList() {
  return nullptr;
}
Expression* Parser::ParseParenthesizedExpression() {return nullptr;}
Expression* Parser::ParseLiteral() {return nullptr;}
Expression* Parser::ParseArrayLiteral() {return nullptr;}
Expression* Parser::ParseElementList() {return nullptr;}
Expression* Parser::ParseSpreadElement() {return nullptr;}
Expression* Parser::ParseObjectLiteral() {return nullptr;}
Expression* Parser::ParsePropertyDefinitionList() {return nullptr;}
Expression* Parser::ParsePropertyDefinition() {return nullptr;}
Expression* Parser::ParsePropertyName() {return nullptr;}
Expression* Parser::ParseLiteralPropertyName() {return nullptr;}
Expression* Parser::ParseComputedPropertyName() {return nullptr;}
Expression* Parser::ParseCoverInitializedName() {return nullptr;}
Expression* Parser::ParseInitializer() {return nullptr;}
Expression* Parser::ParseTemplateLiteral() {return nullptr;}
Expression* Parser::ParseTemplateSpans() {return nullptr;}
Expression* Parser::ParseTemplateMiddleList() {return nullptr;}
Expression* Parser::ParseSuperProperty() {return nullptr;}
Expression* Parser::ParseNewTarget() {return nullptr;}
Expression* Parser::ParseCallMemberExpression() {return nullptr;}
Expression* Parser::ParseSuperCall() {return nullptr;}
Expression* Parser::ParseArguments() {return nullptr;}
Expression* Parser::ParseArgumentList() {return nullptr;}
Expression* Parser::ParseAssignmentPattern() {return nullptr;}
Expression* Parser::ParseObjectAssignmentPattern() {return nullptr;}
Expression* Parser::ParseArrayAssignmentPattern() {return nullptr;}
Expression* Parser::ParseAssignmentPropertyList() {return nullptr;}
Expression* Parser::ParseAssignmentElementList() {return nullptr;}
Expression* Parser::ParseAssignmentElisionElement() {return nullptr;}
Expression* Parser::ParseAssignmentProperty() {return nullptr;}
Expression* Parser::ParseAssignmentElement() {return nullptr;}
Expression* Parser::ParseAssignmentRestElement() {return nullptr;}
Expression* Parser::ParseDestructuringAssignmentTarget() {return nullptr;}
Ast* Parser::ParseDeclaration() {return nullptr;}
Ast* Parser::ParseHoistableDeclaration() {return nullptr;}
Ast* Parser::ParseBreakableStatement() {return nullptr;}
Ast* Parser::ParseBlockStatement() {return nullptr;}
Ast* Parser::ParseBlock() {return nullptr;}
Ast* Parser::ParseLexicalDeclaration() {return nullptr;}
Ast* Parser::ParseBindingList() {return nullptr;}
Ast* Parser::ParseLexicalBinding() {return nullptr;}
Ast* Parser::ParseVariableStatement() {return nullptr;}
Ast* Parser::ParseVariableDeclarationList() {return nullptr;}
Ast* Parser::ParseVariableDeclaration() {return nullptr;}
Ast* Parser::ParseBindingPattern() {return nullptr;}
Ast* Parser::ParseObjectBindingPattern() {return nullptr;}
Ast* Parser::ParseArrayBindingPattern() {return nullptr;}
Ast* Parser::ParseBindingPropertyList() {return nullptr;}
Ast* Parser::ParseBindingElementList() {return nullptr;}
Ast* Parser::ParseBindingElisionElement() {return nullptr;}
Ast* Parser::ParseBindingProperty() {return nullptr;}
Ast* Parser::ParseBindingElement() {return nullptr;}
Ast* Parser::ParseSingleNameBinding() {return nullptr;}
Ast* Parser::ParseBindingRestElement() {return nullptr;}
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
Ast* Parser::ParseFunctionExpression() {return nullptr;}
Ast* Parser::ParseUniqueFormalParameters() {return nullptr;}
Ast* Parser::ParseFormalParameters() {return nullptr;}
Ast* Parser::ParseFormalParameterList() {return nullptr;}
Ast* Parser::ParseFunctionRestParameter() {return nullptr;}
Ast* Parser::ParseFormalParameter() {return nullptr;}
Ast* Parser::ParseFunctionBody() {return nullptr;}
Ast* Parser::ParseFunctionStatementList() {return nullptr;}
Expression* Parser::ParseArrowFunction() {return nullptr;}
Expression* Parser::ParseArrowParameters() {return nullptr;}
Ast* Parser::ParseConciseBody() {return nullptr;}
Expression* Parser::ParseArrowFormalParameters() {return nullptr;}
Expression* Parser::ParseAsyncArrowFunction() {return nullptr;}
Ast* Parser::ParseAsyncConciseBody() {return nullptr;}
Ast* Parser::ParseAsyncArrowHead() {return nullptr;}
Ast* Parser::ParseMethodDefinition() {return nullptr;}
Ast* Parser::ParsePropertySetParameterList() {return nullptr;}
Ast* Parser::ParseGeneratorMethod() {return nullptr;}
Ast* Parser::ParseGeneratorDeclaration() {return nullptr;}
Ast* Parser::ParseGeneratorExpression() {return nullptr;}
Ast* Parser::ParseGeneratorBody() {return nullptr;}
Expression* Parser::ParseYieldExpression() {return nullptr;}
Ast* Parser::ParseAsyncMethod() {return nullptr;}
Ast* Parser::ParseAsyncFunctionDeclaration() {return nullptr;}
Expression* Parser::ParseAsyncFunctionExpression() {return nullptr;}
Ast* Parser::ParseAsyncFunctionBody() {return nullptr;}
Expression* Parser::ParseAwaitExpression() {return nullptr;}
Ast* Parser::ParseClassDeclaration() {return nullptr;}
Ast* Parser::ParseClassExpression() {return nullptr;}
Ast* Parser::ParseClassTail() {return nullptr;}
Ast* Parser::ParseClassHeritage() {return nullptr;}
Ast* Parser::ParseClassBody() {return nullptr;}
Ast* Parser::ParseClassElementList() {return nullptr;}
Ast* Parser::ParseClassElement() {return nullptr;}

const uint8_t Ast::kStatementFlag;
const uint8_t Ast::kExpressionFlag;
}  // namespace lux
