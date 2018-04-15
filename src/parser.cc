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

#include "./parser.h"
#include <iostream>
#include <memory>
#include <sstream>
#include <type_traits>
#include "./alloc.h"
#include "./chars.h"

namespace lux {
#define BASE_REPORT_SYNTAX_ERROR_(parser)                         \
  auto e = AllocShared<lux::ErrorDescriptor>(parser->position()); \
  (reporter_->ReportSyntaxError(e))

#ifdef DEBUG
#define REPORT_TOKENIZER_ERROR_NO_RETURN(parser, message)                \
  {                                                                      \
    BASE_REPORT_SYNTAX_ERROR_(parser)                                    \
        << "\n [Debug] line:" << __LINE__ << ' ' << __FUNCTION__ << "\n" \
        << message;                                                      \
  }
#define REPORT_TOKENIZER_ERROR(parser, message) \
  BASE_REPORT_SYNTAX_ERROR_(parser);            \
  return Token::INVALID;
#define REPORT_SYNTAX_ERROR_NO_RETURN(parser, message)                   \
  {                                                                      \
    BASE_REPORT_SYNTAX_ERROR_(parser)                                    \
        << "\n [Debug] line:" << __LINE__ << ' ' << __FUNCTION__ << "\n" \
        << message;                                                      \
  }
#define REPORT_SYNTAX_ERROR(T, parser, message)  \
  ;                                              \
  REPORT_SYNTAX_ERROR_NO_RETURN(parser, message) \
  return Nothing<T>();
#else
#define REPORT_TOKENIZER_ERROR(parser, message) \
  BASE_REPORT_SYNTAX_ERROR_(parser) << message return Token::INVALID;
#define REPORT_SYNTAX_ERROR(T, parser, message) \
  BASE_REPORT_SYNTAX_ERROR_(parser) << message return Nothing<T>;
#endif

#define EXPECT_NOT_ADVANCE(T, parser, n, token, expect)            \
  if (n != token) {                                                \
    REPORT_SYNTAX_ERROR(T, parser, "'" << expect << "' expected"); \
  }

#define EXPECT_NOT_ADVANCE_ONE_OF(T, parser, n, expects, ...)       \
  if (!Token::OneOf(n, {__VA_ARGS__})) {                            \
    REPORT_SYNTAX_ERROR(T, parser, "'" << expects << "' expected"); \
  }

#define EXPECT(T, parser, n, token, expect)       \
  EXPECT_NOT_ADVANCE(T, parser, n, token, expect) \
  advance();

#define EXPECT_ONE_OF(T, parser, n, expects, ...)               \
  EXPECT_NOT_ADVANCE_ONE_OF(T, parser, n, expects, __VA_ARGS__) \
  advance();

#define EXPECT_NO_RETURN(parser, n, token, expect)                        \
  if (n != token) {                                                       \
    REPORT_SYNTAX_ERROR_NO_RETURN(parser, "'" << expect << "' expected"); \
    return;                                                               \
  }                                                                       \
  advance();

const uint8_t Ast::kStatementFlag;
const uint8_t Ast::kExpressionFlag;

const uint8_t PropertyAccessExpression::kAccessTypeMask;
const uint8_t PropertyAccessExpression::kReceiverTypeMask;

void Parser::Tokenizer::Prologue() {
  current_position_->set_start_col(current_position_->end_col());
  current_state_->Unset(TokenizerState::IMPLICIT_OCTAL);
  unset_linebreak_before();
  while (1) {
    if (SkipLineBreak()) {
      current_position_->set_start_line_number(
          current_position_->end_line_number() + skipped_);
      current_position_->set_end_col(0);
      current_position_->set_start_col(0);
      current_position_->set_end_col(0);
      set_linebreak_before();
    } else if (SkipWhiteSpace()) {
      current_position_->add_start_col(skipped_);
      current_position_->set_end_col(current_position_->start_col());
      unset_linebreak_before();
    } else {
      break;
    }
  }
}

void Parser::Tokenizer::Epilogue() {
  switch (*it_) {
    case '\n':
    case '\r':
      set_linebreak_after();
    default:
      break;
  }
}

Token::Type Parser::Tokenizer::Next() {
  current_buffer_ = &buffer_;
  current_position_ = &position_;
  current_state_ = &state_;
  if (!HasMore()) {
    return token_ = Token::END;
  }
  previous_position_ = position_;
  if (lookahead_ != Token::INVALID) {
    token_ = lookahead_;
    lookahead_ = Token::INVALID;
    buffer_ = lookahead_buffer_;
    position_ = lookahead_position_;
    state_ = lookahead_state_;
    return token_;
  }
  Prologue();
  token_ = Tokenize();
  Epilogue();
  return token_;
}

Token::Type Parser::Tokenizer::Peek() {
  current_buffer_ = &lookahead_buffer_;
  current_position_ = &lookahead_position_;
  current_state_ = &lookahead_state_;
  if (!HasMore()) {
    lookahead_position_ = position_;
    return lookahead_ = Token::END;
  }
  if (lookahead_ != Token::INVALID) {
    return lookahead_;
  }
  lookahead_position_ = position_;
  Prologue();
  lookahead_ = Tokenize();
  Epilogue();
  return lookahead_;
}

Utf16CodePoint Parser::Tokenizer::Advance() {
  ++it_;
  current_position_->add_end_col();
  return *it_;
}

Parser::TokenizerRecord Parser::Tokenizer::Record() {
  return {it_, lookahead_, position_};
}

void Parser::Tokenizer::Restore(const Parser::TokenizerRecord& record) {
  position_ = record.position;
  it_ = record.cursor;
  lookahead_ = record.lookahead;
}

bool IsSucceeding(Utf16String::iterator* it, char ch) {
  return ((*it) + 1)->code() == ch;
}

bool IsIdentifierStart(Utf16CodePoint value) {
  return (value >= 97 && value <= 122) || (value >= 65 && value <= 90) ||
         (value == '_' || value == '$') || value == 92;
}

bool IsIdentifierChar(Utf16CodePoint value) {
  return IsIdentifierStart(value) || (value >= 48 && value <= 57);
}

bool IsDecimalDigit(Utf16CodePoint value) {
  return (value >= 48 && value <= 57);
}

bool IsHexDigit(Utf16CodePoint value) {
  return (value >= '0' && value <= '9') || (value >= 'a' && value <= 'z') ||
         (value >= 'A' && value <= 'Z');
}

bool IsBinaryDigit(Utf16CodePoint value) {
  return (value >= '0' && value <= '1');
}

bool IsOctalDigit(Utf16CodePoint value) {
  return (value >= '0' && value <= '7');
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

Utf16CodePoint Parser::Tokenizer::DecodeHexEscape(bool* ok, int len) {
  auto unicode_hex_start = *it_;
  u32 ret = 0;
  if (unicode_hex_start == '{') {
    Advance();
    while (*it_ != '}' && *it_ != Unicode::InvalidCodePoint()) {
      ret = ret * 16 + ToHexValue(*it_);
      Advance();
    }
    if (*it_ != '}') {
      REPORT_TOKENIZER_ERROR_NO_RETURN(this, "'}' expected.");
    } else {
      Advance();
    }
  } else {
    for (int i = 0; i < len; i++) {
      if (IsHexDigit(*it_)) {
        ret = ret * 16 + ToHexValue(*it_);
      } else {
        *ok = false;
        return Utf16CodePoint(0);
      }
      Advance();
    }
  }

  return Utf16CodePoint(ret);
}

Utf16CodePoint Parser::Tokenizer::DecodeAsciiEscapeSequence(bool* ok) {
  auto u = DecodeHexEscape(ok, 2);
  if (u > 127) {
    *ok = false;
  }

  return u;
}

Token::Type Parser::Tokenizer::Tokenize() {
  INVALIDATE(current_position_);
  if (parser_state_->Is(State::IN_TEMPLATE_LITERAL)) {
    return TokenizeTemplateCharacters();
  } else if (parser_state_->Is(State::REGEXP_EXPECTED)) {
    return TokenizeRegExp();
  }

  switch (*it_) {
    case '(':
      Advance();
      return Token::LEFT_PAREN;
    case ')':
      Advance();
      return Token::RIGHT_PAREN;
    case '{':
      Advance();
      return Token::LEFT_BRACE;
    case '}':
      Advance();
      return Token::RIGHT_BRACE;
    case '[':
      Advance();
      return Token::LEFT_BRACKET;
    case ']':
      Advance();
      return Token::RIGHT_BRACKET;
    case ',':
      Advance();
      return Token::COMMA;
    case '*':
      Advance();
      if (*it_ == '*') {
        Advance();
        if (*it_ == '=') {
          Advance();
          return Token::OP_POW_ASSIGN;
        }
        return Token::OP_POW;
      }
      if (*it_ == '=') {
        Advance();
        return Token::OP_MUL_ASSIGN;
      }
      return Token::OP_MUL;
    case '/':
      Advance();

      if (*it_ == '/') {
        Advance();
        SkipSingleLineComment();
        return Tokenize();
      }
      if (*it_ == '*') {
        Advance();
        SkipMultiLineComment();
        return Tokenize();
      }

      if (*it_ == '=') {
        Advance();
        return Token::OP_DIV_ASSIGN;
      }
      return Token::OP_DIV;

    case '-':
      Advance();
      if (*it_ == '=') {
        Advance();
        return Token::OP_MINUS_ASSIGN;
      } else if (*it_ == '-') {
        Advance();
        return Token::OP_DECREMENT;
      }
      return Token::OP_MINUS;
    case '+':
      Advance();
      if (*it_ == '=') {
        Advance();
        return Token::OP_PLUS_ASSIGN;
      } else if (*it_ == '+') {
        Advance();
        return Token::OP_INCREMENT;
      }
      return Token::OP_PLUS;
    case '%':
      Advance();
      if (*it_ == '=') {
        Advance();
        return Token::OP_MOD_ASSIGN;
      }
      return Token::OP_MOD;
    case '<':
      Advance();
      if (*it_ == '<') {
        Advance();
        if (*it_ == '=') {
          Advance();
          return Token::OP_SHIFT_LEFT_ASSIGN;
        }
        return Token::OP_SHIFT_LEFT;
      }
      if (*it_ == '=') {
        Advance();
        return Token::OP_LESS_THAN_OR_EQ;
      }
      return Token::OP_LESS_THAN;
    case '>':
      Advance();
      if (*it_ == '>') {
        Advance();
        if (*it_ == '=') {
          Advance();
          return Token::OP_SHIFT_RIGHT_ASSIGN;
        }
        if (*it_ == '>') {
          Advance();
          if (*it_ == '=') {
            Advance();
            return Token::OP_U_SHIFT_RIGHT_ASSIGN;
          }
          return Token::OP_U_SHIFT_RIGHT;
        }
        return Token::OP_SHIFT_RIGHT;
      }
      if (*it_ == '=') {
        Advance();
        return Token::OP_GREATER_THAN_OR_EQ;
      }
      return Token::OP_GREATER_THAN;
    case '=':
      Advance();
      if (*it_ == '=') {
        Advance();
        if (*it_ == '=') {
          Advance();
          return Token::OP_STRICT_EQ;
        }
        return Token::OP_EQ;
      }
      return Token::OP_ASSIGN;
    case '|':
      Advance();
      if (*it_ == '=') {
        Advance();
        return Token::OP_OR_ASSIGN;
      }
      if (*it_ == '|') {
        Advance();
        return Token::OP_LOGICAL_OR;
      }
      return Token::OP_OR;
    case '&':
      Advance();
      if (*it_ == '=') {
        Advance();
        return Token::OP_AND_ASSIGN;
      }
      if (*it_ == '&') {
        Advance();
        return Token::OP_LOGICAL_AND;
      }
      return Token::OP_AND;
    case '~':
      Advance();
      return Token::OP_TILDE;
    case '^':
      Advance();
      if (*it_ == '=') {
        Advance();
        return Token::OP_XOR_ASSIGN;
      }
      return Token::OP_XOR;
    case '.':
      Advance();
      if (IsDecimalDigit(*it_)) {
        return TokenizeNumericLiteral(true);
      }
      if (*it_ == '.') {
        Advance();
        if (*it_ == '.') {
          Advance();
          return Token::SPREAD;
        }
        return Token::INVALID;
      }
      return Token::DOT;
    case '!':
      Advance();
      if (*it_ == '=') {
        Advance();
        if (*it_ == '=') {
          Advance();
          return Token::OP_STRICT_NOT_EQ;
        }
        return Token::OP_NOT_EQ;
      }
      return Token::OP_NOT;
    case ';':
      Advance();
      return Token::TERMINATE;
    case ':':
      Advance();
      return Token::COLON;
    case '?':
      Advance();
      return Token::QUESTION;
    case '`':
      parser_state_->PushState(State::IN_TEMPLATE_LITERAL);
      Advance();
      return Token::BACK_QUOTE;
    case '\'':
    case '\"':
      return TokenizeStringLiteral();
    case '$':
      if (parser_state_->Is(State::IN_TEMPLATE_LITERAL)) {
        if (*(it_ + 1) == '{') {
          Advance();
          return Token::TEMPLATE_SUBSTITUTION;
        }
      }
    default:
      if (IsDecimalDigit(*it_)) {
        return TokenizeNumericLiteral(false);
      }
      if (IsIdentifierStart(*it_)) {
        return TokenizeIdentifier();
      }
  }

  return Token::INVALID;
}

bool IsStartUnicodeEscapeSequence(Utf16CodePoint u) { return u == 'u'; }

bool IsStartAsciiEscapeSequence(Utf16CodePoint u) { return u == 'x'; }

bool IsStartEscapeSequence(Utf16String::iterator* it) {
  return IsStartUnicodeEscapeSequence(**it) || IsStartAsciiEscapeSequence(**it);
}

Utf16CodePoint Parser::Tokenizer::DecodeEscapeSequence(bool* ok) {
  INVALIDATE(*it_ == '\\');
  Advance();  // Consume backslash

  Utf16CodePoint result = Unicode::InvalidCodePoint();
  if (IsStartUnicodeEscapeSequence(*it_)) {
    Advance();
    result = DecodeHexEscape(ok);
  } else if (IsStartAsciiEscapeSequence(*it_)) {
    Advance();
    result = DecodeAsciiEscapeSequence(ok);
  } else {
    *ok = false;
  }

  return result;
}

Token::Type Parser::Tokenizer::TokenizeStringLiteral() {
  current_buffer_->clear();
  auto value = *it_;
  u8 start = value.code();
  Advance();

  bool escaped = false;
  while (HasMore()) {
    value = *it_;
    switch (value.code()) {
      case '\\':
        if (!escaped) {
          auto lookahead = it_ + 1;
          if (IsStartEscapeSequence(&lookahead)) {
            bool ok = true;
            value = DecodeEscapeSequence(&ok);
            if (!ok) {
              REPORT_TOKENIZER_ERROR(this,
                                     "Invalid unicode escape sequence found");
            }
          } else {
            escaped = true;
            Advance();
            break;
          }
        } else {
          Advance();
        }
        escaped = false;
        current_buffer_->push_back(value);
        break;
      case 0:
        return Token::INVALID;
      default:
        if (value == '\n' || value == '\r') {
          if (!escaped) {
            goto Label_Failed;
          }
          CollectLineBreak();
          continue;
        }
        if (value == start) {
          if (!escaped) {
            Advance();
            return Token::STRING_LITERAL;
          }
        }
        if (escaped) {
          escaped = false;
        }
        current_buffer_->push_back(value);
        Advance();
    }
  }

Label_Failed:
  REPORT_TOKENIZER_ERROR(this, "Unterminated string literal");
}

Token::Type Parser::Tokenizer::TokenizeIdentifier() {
  current_buffer_->clear();
  auto value = *it_;
  INVALIDATE(IsIdentifierStart(value));

  while (HasMore() && IsIdentifierChar(value)) {
    if (value == '\\') {
      std::vector<Utf16CodePoint> unicode_identifier;
      Advance();
      auto unicode_keyword = *it_;
      if (unicode_keyword == 'u') {
        Advance();
        bool ok = true;
        value = DecodeHexEscape(&ok);
        if (!ok) {
          return Token::INVALID;
        }
      }
    }
    AdvanceAndPushBuffer();
    value = *it_;
  }

  return GetIdentifierType();
}

void Parser::Tokenizer::AdvanceAndPushBuffer() {
  current_buffer_->push_back(*it_);
  Advance();
}

Token::Type Parser::Tokenizer::TokenizeNumericLiteral(bool period_seen) {
  current_buffer_->clear();
  auto value = *it_;
  INVALIDATE(IsDecimalDigit(value));
  bool leading_zeros = false;

  enum {
    DECIMAL,
    DECIMAL_LEADING_ZERO,
    HEX,
    BINARY,
    OCTAL,
    IMPLICIT_OCTAL
  } kind = DECIMAL;

  if (period_seen) {
    current_buffer_->push_back(Utf16CodePoint('.'));
    while (IsDecimalDigit(*it_)) {
      AdvanceAndPushBuffer();
    }
    return Token::NUMERIC_LITERAL;
  } else if (value == '0') {
    AdvanceAndPushBuffer();
    leading_zeros = true;
    if (*it_ == 'x') {
      AdvanceAndPushBuffer();
      kind = HEX;
      if (!IsHexDigit(*it_)) {
        REPORT_TOKENIZER_ERROR(this, "Expected hex digit.");
      }
      while (IsHexDigit(*it_)) {
        AdvanceAndPushBuffer();
      }
    } else if (*it_ == 'b') {
      kind = BINARY;
      AdvanceAndPushBuffer();
      if (!IsBinaryDigit(*it_)) {
        REPORT_TOKENIZER_ERROR(this, "Expected binary digit.");
      }
      while (IsBinaryDigit(*it_)) {
        AdvanceAndPushBuffer();
      }
      return Token::NUMERIC_LITERAL;
    } else if (*it_ == 'o') {
      kind = OCTAL;
      AdvanceAndPushBuffer();
      if (!IsOctalDigit(*it_)) {
        REPORT_TOKENIZER_ERROR(this, "Expected octal digit.");
      }
      while (IsOctalDigit(*it_)) {
        AdvanceAndPushBuffer();
      }
      return Token::NUMERIC_LITERAL;
    } else if (*it_ >= '0' && *it_ <= '7') {
      kind = IMPLICIT_OCTAL;
      current_state_->Set(TokenizerState::IMPLICIT_OCTAL);
      while (IsOctalDigit(*it_)) {
        AdvanceAndPushBuffer();
      }
    } else if (*it_ == '8' || *it_ == '9') {
      kind = DECIMAL_LEADING_ZERO;
      while (IsDecimalDigit(*it_)) {
        AdvanceAndPushBuffer();
      }
    }

    if (kind == IMPLICIT_OCTAL) {
      if (IsDecimalDigit(*it_) && *it_ > '7') {
        current_state_->Unset(TokenizerState::IMPLICIT_OCTAL);
        while (IsDecimalDigit(*it_)) {
          AdvanceAndPushBuffer();
        }
        kind = DECIMAL_LEADING_ZERO;
      }
    }
  }

  if (*it_ == '.' && !period_seen) {
    if (kind != DECIMAL && kind != DECIMAL_LEADING_ZERO) {
      REPORT_TOKENIZER_ERROR(this, "Unexpected token.");
    }
    AdvanceAndPushBuffer();
  }

  while (IsDecimalDigit(*it_)) {
    AdvanceAndPushBuffer();
  }

  if (*it_ == 'e' || *it_ == 'E') {
    if (kind != DECIMAL && kind != DECIMAL_LEADING_ZERO) {
      REPORT_TOKENIZER_ERROR(this, "Unexpected token.");
    }
    AdvanceAndPushBuffer();
    if (*it_ == '+' || *it_ == '-') {
      AdvanceAndPushBuffer();
      if (!IsDecimalDigit(*it_)) {
        REPORT_TOKENIZER_ERROR(this, "Expected exponent digit.");
      }
    }
    if (!IsDecimalDigit(*it_)) {
      REPORT_TOKENIZER_ERROR(this, "Expected exponent digit.");
    }
    while (IsDecimalDigit(*it_)) {
      AdvanceAndPushBuffer();
    }
  }

  return Token::NUMERIC_LITERAL;
}

Token::Type Parser::Tokenizer::GetIdentifierType() {
  auto a = Utf16String::FromVectorNonCopy(current_buffer_);
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
#define KEYWORD_GROUP_CASE(ch) \
  break;                       \
  case ch:
#define KEYWORD(TOKEN, keyword)                                             \
  {                                                                         \
    const int keyword_length = sizeof(keyword) - 1;                         \
    static_assert(keyword_length >= min_length,                             \
                  "The length of the keyword must be greater than 2");      \
    static_assert(keyword_length <= max_length,                             \
                  "The length of the keyword mst be less than 10");         \
    if (input_length == keyword_length && maybe_keyword[1] == keyword[1] && \
        (keyword_length <= 2 || maybe_keyword[2] == keyword[2]) &&          \
        (keyword_length <= 3 || maybe_keyword[3] == keyword[3]) &&          \
        (keyword_length <= 4 || maybe_keyword[4] == keyword[4]) &&          \
        (keyword_length <= 5 || maybe_keyword[5] == keyword[5]) &&          \
        (keyword_length <= 6 || maybe_keyword[6] == keyword[6]) &&          \
        (keyword_length <= 7 || maybe_keyword[7] == keyword[7]) &&          \
        (keyword_length <= 8 || maybe_keyword[8] == keyword[8]) &&          \
        (keyword_length <= 9 || maybe_keyword[9] == keyword[9])) {          \
      return Token::TOKEN;                                                  \
    }                                                                       \
  }
      KEYWORD_TOKEN_LIST(KEYWORD, KEYWORD_GROUP_CASE)
#undef KEYWORD
#undef KEYWORD_GROUP_CASE
  }

  return Token::IDENTIFIER;
}

Token::Type Parser::Tokenizer::TokenizeTemplateCharacters() {
  current_buffer_->clear();
  bool escaped = false;

  if (*it_ == '{') {
    Advance();
  }

  while (HasMore()) {
    switch (*it_) {
      case '\\': {
        auto value = *it_;
        if (!escaped) {
          auto lookahead = it_ + 1;
          if (IsStartEscapeSequence(&lookahead)) {
            bool ok = true;
            value = DecodeEscapeSequence(&ok);
            if (!ok) {
              REPORT_TOKENIZER_ERROR(this,
                                     "Invalid unicode escape sequence found");
            }
          } else {
            escaped = true;
            Advance();
            break;
          }
        }
        escaped = !escaped;
        current_buffer_->push_back(value);
        break;
      }
      case '$':
        if (!escaped && *(it_ + 1) == '{') {
          Advance();
          Advance();
          return Token::TEMPLATE_PARTS;
        }
        AdvanceAndPushBuffer();
        escaped = false;
        break;
      case '`':
        if (!escaped) {
          Advance();
          return Token::TEMPLATE_LITERAL;
        }
      case '\r':
        if (*(it_ + 1) == '\n') {
          Advance();
        }
      case '\n':
        current_position_->set_end_col(0);
        current_position_->add_end_line_number();
      default:
        AdvanceAndPushBuffer();
    }
  }

  REPORT_TOKENIZER_ERROR(this, "Unterminated template literal.");
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

void Parser::Tokenizer::CollectLineBreak() {
  if (*it_ == '\r') {
    AdvanceAndPushBuffer();
    if (*it_ == '\n') {
      AdvanceAndPushBuffer();
    }
  }
}

bool Parser::Tokenizer::SkipLineBreak() {
  skipped_ = 0;
  if (*it_ == '\r') {
    Advance();
    skipped_ = 1;
    if (*it_ == '\n') {
      Advance();
      skipped_ = 2;
    }
    return true;
  }
  return false;
}

bool Parser::Tokenizer::SkipWhiteSpace() {
  skipped_ = 0;
  bool whitespace_seen = false;
  while (Chars::IsWhiteSpace(*it_)) {
    Advance();
    ++skipped_;
    whitespace_seen = true;
  }
  return whitespace_seen;
}

void Parser::Tokenizer::SkipSingleLineComment() {
  while (HasMore()) {
    if (*it_ == '\r') {
      Advance();
      if (*it_ == '\n') {
        Advance();
        break;
      }
    }
    if (*it_ == '\n') {
      Advance();
      break;
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
    : reporter_(reporter), sources_(sources) {
  parser_state_();
  tokenizer_(sources, parser_state_.Get(), reporter_);
}

bool Parser::MatchStates(std::initializer_list<State> states) {
  for (auto& state : states) {
    if (parser_state_->Is(state)) {
      return true;
    }
  }

  return false;
}

Maybe<Ast*> Parser::Parse(ParseType parse_type) {
  ParseDirectivePrologue();
  Maybe<Ast*> result;
  if (reporter_->HasPendingError()) {
    return Nothing<Ast*>();
  }
  if (parse_type == ParseType::SCRIPT) {
    result = ParseScript();
  } else {
    INVALIDATE(parse_type == ParseType::MODULE);
    result = ParseModule();
  }
  if (reporter_->HasPendingError()) {
    return Nothing<Ast*>();
  }
  return result;
}

#ifdef DEBUG

#define ENTER_PARSING                                                      \
  auto debug_result_holder__ = Nothing<Ast*>();                            \
  static const char* kPhaseName__ = __FUNCTION__;                          \
  if (cur() != Token::INVALID) {                                           \
    phase_buffer_ << indent_ << "Enter " << kPhaseName__                   \
                  << ": CurrentToken = " << ToStringCurrentToken();        \
  } else {                                                                 \
    phase_buffer_ << indent_ << "Enter " << kPhaseName__                   \
                  << ": CurrentToken = null";                              \
  }                                                                        \
  phase_buffer_ << " start_col = " << position().start_col();              \
  phase_buffer_ << " end_col = " << position().end_col();                  \
  phase_buffer_ << " line = " << position().start_line_number() << '\n';   \
  indent_ += "  ";                                                         \
  auto err_size = reporter_->size();                                       \
  LUX_SCOPED([&] {                                                         \
    indent_ = indent_.substr(0, indent_.size() - 2);                       \
    if (this->cur() != Token::INVALID) {                                   \
      phase_buffer_ << indent_ << "Exit " << kPhaseName__ << ' '           \
                    << (debug_result_holder__.just() ? "Success"           \
                                                     : "!!Failure")        \
                    << " CurrentToken = " << ToStringCurrentToken()        \
                    << (err_size != reporter_->size() ? "[Error!]" : "");  \
    } else {                                                               \
      phase_buffer_ << indent_ << "Exit " << kPhaseName__ << ' '           \
                    << (debug_result_holder__.just() ? "Success"           \
                                                     : "!!Failure")        \
                    << ": CurrentToken = null"                             \
                    << (err_size != reporter_->size() ? "[Error!]" : "");  \
    }                                                                      \
    phase_buffer_ << " pos = " << position().start_col();                  \
    phase_buffer_ << " line = " << position().start_line_number() << '\n'; \
  });                                                                      \
  auto parsed_result__ = [&, this]() {USE(this);
#define EXIT_PARSING                       \
  }                                        \
  ();                                      \
  debug_result_holder__ = parsed_result__; \
  return parsed_result__;
#else
// Disabled.
#define ENTER_PARSING
#define EXIT_PARSING
#endif

template <typename T>
Maybe<T*> Parser::ParseTerminator(T* expr) {
  ENTER_PARSING
  if (cur() == Token::TERMINATE) {
    advance();
    return Just(expr);
  } else if (cur() != Token::END && cur() != Token::RIGHT_BRACE &&
             !has_linebreak_after()) {
    REPORT_SYNTAX_ERROR(T*, this, "';' expected.");
  }

  return Just(expr);
  EXIT_PARSING;
}

bool NoCondition(Token::Type token) { return true; }

Maybe<Ast*> Parser::ParseScript() {
  ENTER_PARSING
  return ParseStatementList(&NoCondition);
  EXIT_PARSING;
}

void Parser::ParseDirectivePrologue() {
  static const char* kUseStrict = "use strict";
  if (cur() == Token::STRING_LITERAL && value().IsAsciiEqual(kUseStrict)) {
    parser_state_->set_strict_mode();
    advance();
    if (cur() == Token::TERMINATE) {
      advance();
    } else if (cur() != Token::END && !has_linebreak_after()) {
      REPORT_SYNTAX_ERROR_NO_RETURN(this, "';' expected.");
    }
  }
}

Maybe<Statement*> Parser::ParseStatementList(bool (*condition)(Token::Type)) {
  ENTER_PARSING
  auto statements = NewNode<Statements>();
  Statement* last = nullptr;
  while (has_more() && condition(cur())) {
    ParseStatementListItem() >>= [&](auto maybe_statement) {
      INVALIDATE(maybe_statement->IsStatement());
      statements->Push(maybe_statement->ToStatement());
      last = maybe_statement;
    };
  }
  if (last != nullptr) {
    statements->set_end_positions(last->source_position());
  }
  return Just(statements);
  EXIT_PARSING;
}

Maybe<Statement*> Parser::ParseStatementListItem() {
  ENTER_PARSING
  switch (cur()) {
    case Token::CLASS:
    case Token::CONST:
    case Token::FUNCTION:
      return ParseDeclaration();
    default:
      if (cur() == Token::IDENTIFIER) {
        auto v = value();
        if (v.IsAsciiEqual("async") && peek() == Token::FUNCTION &&
            !has_linebreak_after()) {
          return ParseDeclaration();
        } else if (v.IsAsciiEqual("let")) {
          return ParseDeclaration();
        }
      }
      return ParseStatement();
  }
  EXIT_PARSING;
}

Maybe<Statement*> Parser::ParseDeclaration() {
  ENTER_PARSING
  bool async = false;
  switch (cur()) {
    case Token::IDENTIFIER: {
      if (value().IsAsciiEqual("let")) {
        return ParseLexicalDeclaration();
      }
      async = true;
    }
    case Token::FUNCTION:
      return ParseFunctionDeclaration(async);
    case Token::CLASS:
    case Token::CONST:
      return Nothing<Statement*>();
    default:
      UNREACHABLE();
      return Nothing<Statement*>();
  }
  EXIT_PARSING
}

Maybe<Statement*> Parser::ParseStatement() {
  ENTER_PARSING
  switch (cur()) {
    case Token::TERMINATE:
      advance();
      return ParseStatement();
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
  EXIT_PARSING;
}

Maybe<Statement*> Parser::ParseExpressionStatement() {
  ENTER_PARSING
  return ParseExpression().bind<Statement*>([&, this](auto expr) {
    auto exprs = NewStatement<ExpressionStatement>(expr);
    exprs->set_start_positions(expr->source_position());
    exprs->set_end_positions(expr->source_position());
    return ParseTerminator(exprs);
  });
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseExpression() {
  ENTER_PARSING
  return ParseAssignmentExpression() >>= [&](auto expr) -> Expression* {
    if (cur() == Token::COMMA) {
      advance();
      auto expressions = NewNode<Expressions>();
      expressions->Push(expr->ToExpression());
      while (cur() == Token::COMMA) {
        auto assignment_expr_ret = ParseAssignmentExpression();
        if (!assignment_expr_ret) {
          return nullptr;
        }
        assignment_expr_ret >>= [&](Expression* assignment_expr) {
          expressions->Push(assignment_expr->ToExpression());
        };
      }

      return expressions;
    }
    return expr;
  };
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseAssignmentExpression() {
  ENTER_PARSING

  switch (cur()) {
    case Token::YIELD:
      if (!MatchStates({IN_GENERATOR_FUNCTION, IN_ASYNC_GENERATOR_FUNCTION})) {
        REPORT_SYNTAX_ERROR(
            Expression*, this,
            "yield only allowed in generator or async generator.");
      }
      return ParseYieldExpression();
    case Token::NEW:
      return ParseAssignmentExpressionLhs();
    default:
      if (peek() == Token::ARROW_FUNCTION_GLYPH) {
        return ParseArrowFunction();
      } else if (value().IsAsciiEqual("async")) {
        Record();
        advance();
        auto next = peek();
        Restore();
        if (next == Token::ARROW_FUNCTION_GLYPH) {
          return ParseAsyncArrowFunction();
        }
      }
      return ParseConditionalExpression() >>= [&](auto node) {
        if (node->is_lhs_expr()) {
          if (IsAssignmentOperator(cur())) {
            if (node->IsStructuralLiteral() &&
                !node->ToStructuralLiteral()->is_valid_lhs()) {
              REPORT_SYNTAX_ERROR(Expression*, this,
                                  "Invalid left-hand-side expression.");
            }
            auto op = cur();
            advance();
            return ParseAssignmentExpression() >>= [&](auto rhs) {
              auto ret = NewExpressionWithPosition<BinaryExpression>(
                  node->source_position(), op, node, rhs);
              ret->set_end_positions(rhs->source_position());
              return ret;
            };
          }
        }
        return Just(node);
      };
  }
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseAssignmentExpressionLhs() {
  ENTER_PARSING
  return ParseLeftHandSideExpression() >>= [this](auto left) {
    if (cur() == Token::OP_ASSIGN || IsAssignmentOperator(cur())) {
      advance();
      return ParseAssignmentExpression() >>= [&, this](auto right) {
        return NewExpression<BinaryExpression>(Token::OP_ASSIGN, left, right);
      };
    }
    return Just(left);
  };
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseConditionalExpression() {
  ENTER_PARSING
  return ParseBinaryExpression() >>= [this](auto binary_expr) {
    if (cur() == Token::QUESTION) {
      advance();
      return ParseAssignmentExpression() >>= [&, this](auto lhs) {
        if (cur() != Token::COLON) {
          REPORT_SYNTAX_ERROR(Expression*, this, "':' expected.");
        }
        advance();
        return ParseAssignmentExpression() >>= [&, this](auto rhs) {
          auto ret = NewExpressionWithPosition<ConditionalExpression>(
              binary_expr->source_position(), binary_expr->ToExpression(),
              lhs->ToExpression(), rhs->ToExpression());
          ret->set_end_positions(rhs->source_position());
          return ret;
        };
      };
    }
    return Just(binary_expr);
  };
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseBinaryOperatorByPriority(
    Expression* prev_ast, Parser::OperatorPriority current_op,
    Parser::OperatorPriority prev_op) {
  ENTER_PARSING;

  auto token = cur();
  advance();

  return ParseUnaryExpression() >>= [&, this](auto expr) {
    Expression* left = nullptr;
    Expression* right = nullptr;
    if (prev_op == kNone || prev_op >= current_op) {
      left = prev_ast;
      right = expr;
      auto ret = NewExpressionWithPosition<BinaryExpression>(
          left->source_position(), token, left, right);
      ret->set_end_positions(expr->source_position());
      return ret;
    }

    INVALIDATE(prev_ast->IsBinaryExpression());
    auto maybe_bin_expr = prev_ast;
    while (maybe_bin_expr->IsBinaryExpression()) {
      auto rhs = maybe_bin_expr->ToBinaryExpression()->rhs();
      if (!rhs->IsBinaryExpression()) {
        break;
      }
      maybe_bin_expr = rhs;
    }
    auto bin_expr = maybe_bin_expr->ToBinaryExpression();
    right = expr;
    left = bin_expr->rhs();
    auto ret = NewExpressionWithPosition<BinaryExpression>(
        left->source_position(), token, left, right);
    ret->set_end_positions(expr->source_position());
    bin_expr->set_rhs(ret);
    prev_ast->set_end_positions(expr->source_position());
    return prev_ast;
  };

  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseBinaryExpression() {
  ENTER_PARSING;

  return ParseUnaryExpression() >>= [&, this](auto last) {
    OperatorPriority last_op = kNone;

    while (1) {
      switch (cur()) {
        case Token::OP_LOGICAL_OR: {
          ParseBinaryOperatorByPriority(last, kLogicalOR, last_op) >>=
              [&](auto n) {
                last = n;
                last_op = kLogicalOR;
              };
          break;
        }
        case Token::OP_LOGICAL_AND: {
          ParseBinaryOperatorByPriority(last, kLogicalAND, last_op) >>=
              [&](auto n) {
                last = n;
                last_op = kLogicalAND;
              };
          break;
        }

        case Token::OP_OR: {
          ParseBinaryOperatorByPriority(last, kBitwiseOR, last_op) >>=
              [&](auto n) {
                last = n;
                last_op = kBitwiseOR;
              };
          break;
        }

        case Token::OP_XOR: {
          ParseBinaryOperatorByPriority(last, kBitwiseXOR, last_op) >>=
              [&](auto n) {
                last = n;
                last_op = kBitwiseXOR;
              };
          break;
        }

        case Token::OP_AND: {
          ParseBinaryOperatorByPriority(last, kBitwiseAND, last_op) >>=
              [&](auto n) {
                last = n;
                last_op = kBitwiseAND;
              };
          break;
        }

        case Token::OP_EQ:
        case Token::OP_STRICT_EQ:
        case Token::OP_NOT_EQ:
        case Token::OP_STRICT_NOT_EQ: {
          ParseBinaryOperatorByPriority(last, kEquality, last_op) >>=
              [&](auto n) {
                last = n;
                last_op = kEquality;
              };
          break;
        }

        case Token::OP_GREATER_THAN:
        case Token::OP_GREATER_THAN_OR_EQ:
        case Token::OP_LESS_THAN:
        case Token::OP_LESS_THAN_OR_EQ:
        case Token::INSTANCEOF:
        case Token::IN: {
          ParseBinaryOperatorByPriority(last, kRelational, last_op) >>=
              [&](auto n) {
                last = n;
                last_op = kRelational;
              };
          break;
        }

        case Token::OP_SHIFT_LEFT:
        case Token::OP_SHIFT_RIGHT:
        case Token::OP_U_SHIFT_RIGHT: {
          ParseBinaryOperatorByPriority(last, kShift, last_op) >>= [&](auto n) {
            last = n;
            last_op = kShift;
          };
          break;
        }

        case Token::OP_PLUS:
        case Token::OP_MINUS: {
          ParseBinaryOperatorByPriority(last, kAdditive, last_op) >>=
              [&](auto n) {
                last = n;
                last_op = kAdditive;
              };
          break;
        }

        case Token::OP_MUL:
        case Token::OP_DIV:
        case Token::OP_MOD: {
          ParseBinaryOperatorByPriority(last, kMultiplicative, last_op) >>=
              [&](auto n) {
                last = n;
                last_op = kMultiplicative;
              };
          break;
        }

        case Token::OP_POW: {
          ParseBinaryOperatorByPriority(last, kExponentiation, last_op) >>=
              [&](auto n) {
                last = n;
                last_op = kExponentiation;
              };
          break;
        }

        default:
          return last;
      }
    }
  };
  EXIT_PARSING;
#undef INIT_BY_OPERATOR_ORDER
}

Maybe<Expression*> Parser::ParseUnaryExpression() {
  ENTER_PARSING;
  auto start = position();
  switch (cur()) {
    case Token::DELETE:    // FALL_THROUGH
    case Token::VOID:      // FALL_THROUGH
    case Token::TYPEOF:    // FALL_THROUGH
    case Token::OP_PLUS:   // FALL_THROUGH
    case Token::OP_MINUS:  // FALL_THROUGH
    case Token::OP_TILDE:  // FALL_THROUGH
    case Token::OP_NOT: {  // FALL_THROUGH
      auto op = cur();
      advance();
      return ParseUnaryExpression() >>= [&, this](auto rhs_exp) {
        auto unary =
            NewExpression<UnaryExpression>(UnaryExpression::PRE, op, rhs_exp);
        unary->set_start_positions(start);
        unary->set_end_positions(rhs_exp->source_position());
        return Just(unary);
      };
    }
    case Token::AWAIT:
      advance();
      return ParseAwaitExpression();
    default:
      return ParseUpdateExpression();
  }
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseUpdateExpression() {
  ENTER_PARSING;
  auto start = position();
  switch (cur()) {
    case Token::OP_INCREMENT:
    case Token::OP_DECREMENT: {
      auto op = cur();
      advance();
      return ParseLeftHandSideExpression() >>= [&, this](auto n) {
        auto update = NewExpressionWithPosition<UnaryExpression>(
            start, UnaryExpression::PRE, op, n);
        update->set_end_positions(n->source_position());
        return Just(update);
      };
    }
    default: {
      return ParseLeftHandSideExpression() >>= [&, this](auto n) {
        switch (cur()) {
          case Token::OP_INCREMENT:
          case Token::OP_DECREMENT: {
            LUX_SCOPED([&] { advance(); });
            return Just(NewExpressionWithPositions<UnaryExpression>(
                start, position(), UnaryExpression::POST, cur(), n));
          }
          default:
            return Just(n);
        }
      };
    }
  }
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseLeftHandSideExpression() {
  ENTER_PARSING;

  auto set_lhs = [&](auto node) {
    node->set_lhs_expr();
    return node;
  };

  switch (cur()) {
    case Token::NEW: {
      if (peek() == Token::DOT) {
        advance();
        return ParseCallExpression() >>= set_lhs;
      }
      return ParseNewExpression() >>= set_lhs;
    }
    default:
      return ParseCallExpression() >>= set_lhs;
  }
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseNewExpression() {
  ENTER_PARSING;
  if (cur() == Token::NEW) {
    auto start = position();
    if (peek() == Token::NEW) {
      advance();
      return ParseNewExpression() >>= [&, this](auto callee) {
        auto expr =
            NewExpressionWithPosition<lux::NewExpression>(start, callee);
        expr->set_end_positions(callee->source_position());
        return Just(expr);
      };
    } else {
      return ParseMemberExpression();
    }
  }
  return ParseMemberExpression();
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseCallExpression() {
  ENTER_PARSING;
  auto start = position();
  switch (cur()) {
    case Token::SUPER: {
      Record();
      advance();
      if (cur() == Token::LEFT_PAREN) {
        Restore();
        return ParseSuperCall() >>= [&, this](auto n) {
          return ParsePostMemberExpression(
              start, n, Receiver::SUPER,
              Allowance(Allowance::CALL | Allowance::TEMPLATE));
        };
      }
      Restore();
    }
    default: {
      return ParseCoverCallExpressionAndAsyncArrowHead() >>=
             [&, this](auto expr) {
               if (expr->IsExpressions()) {
                 auto exprs = expr->ToExpressions();
                 expr = NewNode<CallExpression>(Receiver::EXPRESSION,
                                                exprs->at(0), exprs->at(1));
               }
               return ParsePostMemberExpression(
                   start, expr, Receiver::EXPRESSION,
                   Allowance(Allowance::CALL | Allowance::TEMPLATE));
             };
    }
  }
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParsePostMemberExpression(
    const SourcePosition& start, Expression* pre, Receiver::Type receiver_type,
    Parser::Allowance allowance, bool error_if_default) {
  ENTER_PARSING;

  auto current = pre;

  while (1) {
    switch (cur()) {
      case Token::LEFT_PAREN: {
        if (!allowance.is_call_allowed()) {
          goto Label_End;
        }
        ParseArguments() >>= [&](auto a) {
          current = NewExpressionWithPosition<CallExpression>(
              start, receiver_type, current, a);
        };
        break;
      }
      case Token::LEFT_BRACKET: {
        auto pos = position();
        advance();
        ParseExpression() >>= [&, this](auto n) {
          auto end = position();
          EXPECT(Expression*, this, cur(), Token::RIGHT_BRACKET, ']');
          auto expr = NewExpressionWithPosition<PropertyAccessExpression>(
              start, PropertyAccessExpression::AccessType::ELEMENT,
              receiver_type, current, n);
          expr->set_end_positions(end);
          return Just(current = expr);
        };
        break;
      }
      case Token::DOT: {
        advance();
        ParseIdentifierReference() >>= [&](auto n) {
          auto expr = NewExpressionWithPosition<PropertyAccessExpression>(
              start, PropertyAccessExpression::DOT, receiver_type, current, n);
          expr->set_end_positions(n->source_position());
          current = expr;
        };
        break;
      }
      case Token::BACK_QUOTE: {
        if (!allowance.is_template_allowed()) {
          REPORT_SYNTAX_ERROR(Expression*, this, "Unexpected '`' found.");
        }
        ParseTemplateLiteral() >>= [&](auto t) {
          current = NewExpressionWithPosition<CallExpression>(
              start, Receiver::TEMPLATE, current, t);
          current->set_end_positions(t->source_position());
        };
        break;
      }
      default:
        if (error_if_default) {
          REPORT_SYNTAX_ERROR(
              Expression*, this,
              "Unexpected " << ToStringCurrentToken() << " found.");
        }
        return Just(current);
    }
  }

Label_End:
  return Just(current);
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseCoverCallExpressionAndAsyncArrowHead() {
  ENTER_PARSING;
  return ParseMemberExpression() >>= [&, this](auto n) {
    if (cur() == Token::LEFT_PAREN) {
      return ParseArguments() >>= [&, this](auto a) {
        return Just(reinterpret_cast<Expression*>(
            NewNode<Expressions>({n->ToExpression(), a->ToExpression()})));
      };
    }
    return Just(n);
  };
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseMemberExpression() {
  ENTER_PARSING;
  auto start = position();
  if (cur() == Token::SUPER) {
    advance();
    return ParsePostMemberExpression(start, nullptr, Receiver::SUPER,
                                     Allowance(), true);
  }

  if (cur() == Token::NEW) {
    advance();
    if (cur() == Token::DOT) {
      advance();
      if (peek() == Token::IDENTIFIER) {
        if (peek_value().IsAsciiEqual("target")) {
          return Just(NewExpression<PropertyAccessExpression>(
              PropertyAccessExpression::DOT, Receiver::NEW, nullptr, nullptr));
        }
        REPORT_SYNTAX_ERROR(
            Expression*, this,
            "new.target? but got " << peek_value().ToUtf8String());
      }
      REPORT_SYNTAX_ERROR(Expression*, this,
                          "new.target? identifier 'target' expected.");
    }
    auto start_call = position();
    return ParseMemberExpression() >>= [&, this](auto m) {
      if (cur() != Token::LEFT_PAREN) {
        auto expr = NewExpressionWithPosition<lux::NewExpression>(start, m);
        expr->set_end_positions(m->source_position());
        return Just(expr);
      }
      return ParseArguments() >>= [&, this](auto args) {
        auto call = NewExpressionWithPosition<CallExpression>(
            start_call, Receiver::EXPRESSION, m, args);
        call->set_end_positions(args->source_position());
        auto expr = NewExpressionWithPosition<lux::NewExpression>(start, call);
        expr->set_end_positions(args->source_position());
        return ParsePostMemberExpression(start, expr, Receiver::EXPRESSION,
                                         Allowance(Allowance::TEMPLATE));
      };
    };
  }

  return ParsePrimaryExpression() >>= [&, this](auto n) {
    return ParsePostMemberExpression(start, n, Receiver::EXPRESSION,
                                     Allowance(Allowance::TEMPLATE));
  };
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParsePrimaryExpression() {
  ENTER_PARSING;

  auto start = position();
  switch (cur()) {
    case Token::THIS: {
      advance();
      auto expr = NewExpressionWithPosition<Literal>(start, Token::THIS);
      return Just(expr);
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
    case Token::NUMERIC_LITERAL:
      if (tokenizer_state().Is(TokenizerState::IMPLICIT_OCTAL) &&
          parser_state_->is_strict_mode()) {
        REPORT_SYNTAX_ERROR(
            Expression*, this,
            "Implicit octal literal not allowed in strict mode.");
      }
    case Token::NULL_VALUE:
    case Token::TRUE:
    case Token::FALSE:
    case Token::STRING_LITERAL:
      return ParseLiteral();
    case Token::BACK_QUOTE:
      return ParseTemplateLiteral();
    case Token::OP_DIV:
      return ParseRegularExpression();
    case Token::LEFT_PAREN:
      return ParseCoverParenthesizedExpressionAndArrowParameterList();
    default:
      return Nothing<Expression*>();
  }
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseLiteral() {
  ENTER_PARSING;
  LUX_SCOPED([&] { advance(); });
  switch (cur()) {
    case Token::NULL_VALUE:
    case Token::TRUE:
    case Token::FALSE: {
      auto t = cur();
      return Just(NewExpression<Literal>(t));
    }
    case Token::NUMERIC_LITERAL:
    case Token::STRING_LITERAL: {
      auto t = cur();
      return Just(NewExpression<Literal>(t, value().Clone()));
    }
    default:
      UNREACHABLE();
  }
  REPORT_SYNTAX_ERROR(Expression*, this, "Literal expected.");
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseIdentifierReference() {
  ENTER_PARSING;
  LUX_SCOPED([&] { advance(); })
  auto start = position();
  switch (cur()) {
    case Token::IDENTIFIER:
    case Token::YIELD:
    case Token::AWAIT:
      return Just(NewExpressionWithPosition<Literal>(start, Token::IDENTIFIER,
                                                     value().Clone()));
    default:
      UNREACHABLE();
  }
  REPORT_SYNTAX_ERROR(Expression*, this, "Identifier expected.");
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseArrayLiteral(Parser::Allowance allowance) {
  ENTER_PARSING;
  INVALIDATE(cur() == Token::LEFT_BRACKET);
  auto array = NewNode<StructuralLiteral>(StructuralLiteral::ARRAY);
  array->set_source_position(position());
  advance();
  while (!Token::OneOf(cur(), {Token::RIGHT_BRACKET, Token::INVALID})) {
    if (cur() == Token::COMMA) {
      auto start_pos = position();
      advance();
      switch (cur()) {
        case Token::COMMA:
        case Token::RIGHT_BRACKET: {
          array->Push(NewExpressionWithPosition<Elision>(start_pos));
        }
        default: {
          while (cur() == Token::COMMA) {
            array->Push(NewExpressionWithPosition<Elision>(position()));
            advance();
          }
        }
      }
    } else {
      if (cur() == Token::SPREAD) {
        ParseSpreadElement() >>= [&](auto a) { array->Push(a); };
      } else {
        ParseAssignmentExpression() >>= [&](auto a) {
          if (a->IsLiteral() && a->ToLiteral()->Is(Token::IDENTIFIER)) {
            array->set_valid_lhs(true);
          } else {
            array->set_valid_lhs(false);
          }
          array->Push(a);
        };
      }
    }
  }
  array->set_end_positions(position());
  EXPECT(Expression*, this, cur(), Token::RIGHT_BRACKET, ']');
  return Just(array->ToExpression());
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseSpreadElement() {
  ENTER_PARSING;
  auto start_pos = position();
  INVALIDATE(cur() == Token::SPREAD);
  advance();
  return ParseAssignmentExpression() >>= [&, this](auto expr) {
    auto node = NewNodeWithPosition<UnaryExpression>(
        start_pos, UnaryExpression::PRE, Token::SPREAD, expr);
    node->set_end_positions(expr->source_position());
    return node;
  };
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseObjectLiteral(Parser::Allowance allowance) {
  ENTER_PARSING;
  INVALIDATE(cur() == Token::LEFT_BRACE);
  advance();
  auto object = NewNode<StructuralLiteral>(StructuralLiteral::OBJECT);
  if (cur() == Token::RIGHT_BRACE) {
    return Just(object);
  }

  while (!Token::OneOf(cur(), {Token::RIGHT_BRACE, Token::INVALID})) {
    ParseObjectLiteralProperty(allowance) >>= [&](auto o) { object->Push(o); };
  }

  return Just(object);
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseObjectLiteralProperty(
    Parser::Allowance allowance) {
  ENTER_PARSING
  auto key = Nothing<Expression*>();
  bool is_computed_property_name = false;
  if (Token::OneOf(cur(), {Token::NUMERIC_LITERAL, Token::IDENTIFIER,
                           Token::STRING_LITERAL, Token::LEFT_BRACKET})) {
    key = cur() == Token::IDENTIFIER ? ParseIdentifierReference()
                                     : ParsePropertyName();

    if (cur() == Token::OP_ASSIGN) {
      if (!allowance.is_binding_pattern_allowed() ||
          is_computed_property_name) {
        REPORT_SYNTAX_ERROR(Expression*, this, "Unexpected '=' detected.");
      }
      return ParseIdentifierReference() >>= [&, this](auto key) {
        advance();
        return ParseAssignmentExpression() >>= [&, this](auto value) {
          return Just(
              NewExpression<ObjectPropertyExpression>(key, nullptr, value));
        };
      };
    } else if (Token::OneOf(cur(), {Token::COMMA, Token::RIGHT_BRACE})) {
      if (!allowance.is_binding_pattern_allowed() ||
          is_computed_property_name) {
        REPORT_SYNTAX_ERROR(Expression*, this, "':' expected.");
      }
      return ParseIdentifierReference();
    } else {
      if (allowance.is_binding_pattern_allowed()) {
        REPORT_SYNTAX_ERROR(Expression*, this, "binding pattern expected.");
      }
      return ParseMethodDefinition();
    }
  } else {
    REPORT_SYNTAX_ERROR(Expression*, this,
                        "Property name must be one of "
                        "'identifier', 'string literal', 'numeric literal', "
                        "or 'computed property' but got "
                            << ToStringCurrentToken());
  }

  if (cur() != Token::COLON) {
    if (!allowance.is_binding_pattern_allowed() || is_computed_property_name) {
      REPORT_SYNTAX_ERROR(Expression*, this, "':' expected.");
    }
  }

  advance();
  return ParseAssignmentExpression() >>= [&, this](auto value) {
    if (allowance.is_binding_pattern_allowed()) {
      if (!value->IsStructuralLiteral() && value->IsLiteral()) {
        REPORT_SYNTAX_ERROR(Expression*, this,
                            "Identifier or binding pattern expected.");
      } else if (value->IsLiteral() &&
                 !value->ToLiteral()->Is(Token::IDENTIFIER)) {
        REPORT_SYNTAX_ERROR(Expression*, this, "Identifier expected.");
      }
    }

    return key >>= [&, this](auto key) {
      return Just(NewExpression<ObjectPropertyExpression>(key, value));
    };
  };
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseMethodDefinition() {
  ENTER_PARSING;
  bool is_getter = false;
  bool is_setter = false;

  if (((is_getter = value().IsAsciiEqual("get")) ||
       (is_setter = value().IsAsciiEqual("set"))) &&
      Token::OneOf(peek(), {Token::IDENTIFIER, Token::LEFT_BRACKET})) {
    advance();
  }

  switch (cur()) {
    case Token::OP_MUL:
      return ParseGeneratorMethod();
    case Token::STRING_LITERAL:
    case Token::NUMERIC_LITERAL:
    case Token::LEFT_BRACKET:
    case Token::IDENTIFIER: {
      auto maybe_name = Nothing<Expression*>();
      auto formal_parameters = Nothing<Expression*>();

      if (cur() == Token::IDENTIFIER && value().IsAsciiEqual("async")) {
        return ParseAsyncMethod();
      } else {
        maybe_name = ParsePropertyName();
      }

      EXPECT(Expression*, this, cur(), Token::LEFT_PAREN, '(');
      if (is_getter) {
        if (cur() != Token::RIGHT_PAREN) {
          REPORT_SYNTAX_ERROR(Expression*, this,
                              "Getter must not have any formal parameters.");
        }
        advance();
        formal_parameters = Just(NewNode<Expressions>());
      } else if (is_setter) {
        if (cur() == Token::RIGHT_PAREN) {
          REPORT_SYNTAX_ERROR(Expression*, this,
                              "Setter must have exactly one formal parameter.");
        }
        formal_parameters = ParsePropertySetParameterList();
        if (cur() != Token::RIGHT_PAREN) {
          REPORT_SYNTAX_ERROR(Expression*, this,
                              "Setter must have exactly one formal parameter.");
        }
        advance();
      } else {
        formal_parameters = ParseFormalParameters();
      }

      EXPECT(Expression*, this, cur(), Token::LEFT_BRACE, '{');
      return maybe_name >>= [&, this](auto name) {
        return formal_parameters >>= [&, this](auto formal_parameters) {
          auto body_ret = ParseFunctionBody();
          if (!body_ret) {
            return Nothing<Expression*>();
          }
          auto body = body_ret.value();
          EXPECT(Expression*, this, cur(), Token::RIGHT_BRACE, '}');
          auto type = is_getter ? Function::GETTER : Function::SETTER;

          auto fn = NewNode<FunctionExpression>(type, maybe_name,
                                                formal_parameters, body);
          return Just(NewExpression<ObjectPropertyExpression>(name, fn));
        };
      };
    }
    default:
      UNREACHABLE();
  }

  return Nothing<Expression*>();
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParsePropertyName() {
  ENTER_PARSING;
  switch (cur()) {
    case Token::IDENTIFIER:
      return ParseIdentifier();
    case Token::STRING_LITERAL:
    case Token::NUMERIC_LITERAL:
      return ParseLiteral();
    default: {
      EXPECT(Expression*, this, cur(), Token::RIGHT_BRACKET, '[');
      return ParseAssignmentExpression() >>= [&, this](auto ret) {
        EXPECT(Expression*, this, cur(), Token::RIGHT_BRACKET, ']');
        return Just(ret);
      };
    }
  }
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseFormalParameters() {
  ENTER_PARSING;
  if (cur() == Token::SPREAD) {
    return ParseFunctionRestParameter();
  }
  return ParseFormalParameterList() >>= [&, this](auto p) {
    auto exprs = p->ToExpressions();
    if (cur() == Token::COMMA) {
      advance();
      if (cur() == Token::SPREAD) {
        ParseFunctionRestParameter() >>= [&](auto rp) { exprs->Push(rp); };
      }
    }
    return Just(exprs->ToExpression());
  };
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseFormalParameterList() {
  ENTER_PARSING;
  auto exprs = NewNode<Expressions>();
  while (1) {
    switch (cur()) {
      case Token::IDENTIFIER:
      case Token::LEFT_BRACE:
      case Token::LEFT_BRACKET:
        ParseBindingElement() >>= [&](auto bi) { exprs->Push(bi); };
        break;
      case Token::COMMA:
        advance();
        break;
      default:
        return Just(exprs);
    }
  }
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseFunctionRestParameter() {
  ENTER_PARSING;
  EXPECT(Expression*, this, cur(), Token::SPREAD, "...");
  advance();
  switch (cur()) {
    case Token::LEFT_BRACE:
    case Token::LEFT_BRACKET:
      return ParseBindingPattern();
    default:
      return ParseSingleNameBinding(Allowance(Allowance::INITIALIZER));
  }
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseBindingElement() {
  ENTER_PARSING;
  switch (cur()) {
    case Token::LEFT_BRACKET:
    case Token::LEFT_BRACE:
      return ParseBindingPattern();
    default:
      return ParseSingleNameBinding(Allowance(Allowance::INITIALIZER));
  }
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseSingleNameBinding(Allowance allowance) {
  ENTER_PARSING;
  Expression* identifier = nullptr;
  switch (cur()) {
    case Token::IDENTIFIER:
    case Token::YIELD:
    case Token::AWAIT:
      identifier = NewNode<Literal>(Token::IDENTIFIER, value().Clone());
    default:
      REPORT_SYNTAX_ERROR(Expression*, this, "Identifier expected.");
  }

  INVALIDATE(identifier != nullptr);
  if (allowance.is_initializer_allowed() && cur() == Token::OP_ASSIGN) {
    advance();
    return ParseAssignmentExpression() >>= [&, this](auto i) -> Expression* {
      return NewNode<BinaryExpression>(Token::OP_ASSIGN, identifier, i);
    };
  }
  return Just(identifier);
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseBindingPattern() {
  ENTER_PARSING;
  switch (cur()) {
    case Token::LEFT_BRACE:
      return ParseObjectLiteral(Allowance(Allowance::BINDING_PATTERN));
    default:
      INVALIDATE(cur() == Token::LEFT_BRACKET);
      return ParseArrayLiteral(Allowance(Allowance::BINDING_PATTERN));
  }
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseArguments() {
  ENTER_PARSING;
  auto start = position();
  EXPECT(Expression*, this, cur(), Token::LEFT_PAREN, '(');
  auto exprs = NewNode<Expressions>();
  while (1) {
    if (cur() == Token::SPREAD) {
      advance();
      ParseAssignmentExpression() >>= [&, this](auto expr) {
        auto u =
            NewNode<UnaryExpression>(UnaryExpression::PRE, Token::SPREAD, expr);
        exprs->Push(u);
      };
    } else {
      ParseAssignmentExpression() >>= [&](auto expr) { exprs->Push(expr); };
    }

    if (cur() == Token::COMMA) {
      if (peek() == Token::LEFT_PAREN) {
        REPORT_SYNTAX_ERROR(Expression*, this, "Extra ',' found.");
      }
      advance();
    } else {
      break;
    }
  }
  auto end = position();
  EXPECT(Expression*, this, cur(), Token::RIGHT_PAREN, ')');
  exprs->set_start_positions(start);
  exprs->set_end_positions(end);
  return Just(exprs->ToExpression());
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseIdentifier() {
  ENTER_PARSING;
  EXPECT_NOT_ADVANCE(Expression*, this, cur(), Token::IDENTIFIER, "Identifier");
  return Just(NewExpression<Literal>(Token::IDENTIFIER, value().Clone()));
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseArrowFunction() {
  ENTER_PARSING;
  return ParseArrowParameters() >>= [&, this](auto p) {
    if (cur() != Token::TERMINATE || has_linebreak_before()) {
      EXPECT(Expression*, this, cur(), Token::ARROW_FUNCTION_GLYPH, "=>");
      auto body_ret = ParseConciseBody();
      if (body_ret) {
        auto body = body_ret.value();
        return Just(NewExpression<ArrowFunctionExpression>(
            Function::NORMAL, Nothing<Expression*>(), p, body));
      }
    }
    return Just(p);
  };
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseArrowParameters() {
  ENTER_PARSING;
  switch (cur()) {
    case Token::LEFT_PAREN:
      return ParseCoverParenthesizedExpressionAndArrowParameterList();
    default: {
      auto exprs = NewNode<Expressions>();
      ParseSingleNameBinding() >>= [&](auto p) { exprs->Push(p); };
      return Just(exprs->ToExpression());
    }
  }
  EXIT_PARSING;
}

Maybe<Expression*>
Parser::ParseCoverParenthesizedExpressionAndArrowParameterList() {
  ENTER_PARSING;
  EXPECT(Expression*, this, cur(), Token::LEFT_PAREN, '(');
  auto exprs = NewNode<Expressions>();
  if (cur() == Token::RIGHT_PAREN) {
    advance();
    return Just(exprs->ToExpression());
  }

  while (1) {
    if (cur() == Token::SPREAD) {
      advance();
      auto e = Nothing<Expression*>();
      if (Token::OneOf(cur(), {Token::LEFT_BRACKET, Token::LEFT_BRACE})) {
        e = ParseBindingPattern();
      } else {
        e = ParseSingleNameBinding();
      }
      e >>= [&, this](auto e) {
        auto u =
            NewNode<UnaryExpression>(UnaryExpression::PRE, Token::SPREAD, e);
        exprs->Push(u);
      };
    } else {
      ParseExpression() >>= [&](auto n) { exprs->Push(n); };
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
  EXPECT(Expression*, this, cur(), Token::LEFT_PAREN, ')');
  return Just(exprs->ToExpression());
  EXIT_PARSING;
}

Maybe<Expression*> Parser::ParseAsyncFunctionExpression() {
  ENTER_PARSING
  return Nothing<Expression*>();
  EXIT_PARSING;
}

Maybe<Ast*> Parser::ParseConciseBody() {
  ENTER_PARSING;
  if (peek() != Token::LEFT_BRACE) {
    auto ret = ParseAssignmentExpression();
    if (ret) {
      return Just(reinterpret_cast<Ast*>(ret.value()));
    }
    return Nothing<Ast*>();
  }
  advance();
  return ParseFunctionBody().Type<Ast*>() >>= [&, this](auto n) {
    EXPECT(Ast*, this, cur(), Token::RIGHT_BRACE, '}');
    return Just(n);
  };
  EXIT_PARSING;
}

Maybe<Statement*> Parser::ParseModule() { return ParseModuleBody(); }

Maybe<Statement*> Parser::ParseModuleBody() {
  auto statements = NewNode<Statements>();
  while (has_more()) {
    ParseModuleItem() >>= [&](auto i) { statements->Push(i); };
  }
  return Just(statements);
}

Maybe<Statement*> Parser::ParseModuleItem() {
  switch (cur()) {
    case Token::IMPORT:
      return ParseImportDeclaration();
    case Token::EXPORT:
      return ParseExportDeclaration();
    default:
      return ParseStatementListItem();
  }
}

Maybe<Statement*> Parser::ParseImportDeclaration() {
  ENTER_PARSING
  INVALIDATE(cur() == Token::IMPORT);
  auto start = position();
  advance();
  if (cur() == Token::STRING_LITERAL) {
    auto str =
        NewExpressionWithPosition<Literal>(start, cur(), value().Clone());
    return Just(NewStatementWithPosition<ImportDeclaration>(
        start, Nothing<Expression*>(), str));
  }

  auto default_binding_result = Nothing<Expression*>();
  start = position();
  if (cur() == Token::IDENTIFIER) {
    default_binding_result = ParseIdentifier();
    advance();
  }
  auto ib = NewNodeWithPosition<ImportBinding>(start, default_binding_result);

  switch (cur()) {
    case Token::LEFT_BRACE:
      ib->set_named_import_list(ParseNamedImports());
      break;
    case Token::OP_MUL:
      ib->set_namespace_import(ParseNameSpaceImport());
    default:
      REPORT_SYNTAX_ERROR(Statement*, this, "Unexpected token.");
  }

  if (cur() == Token::IDENTIFIER && value().IsAsciiEqual("from")) {
    advance();
    if (cur() == Token::STRING_LITERAL) {
      auto str = NewExpression<Literal>(cur(), value().Clone());
      if (cur() == Token::TERMINATE) {
        advance();
      } else if (!tokenizer_->has_linebreak_after()) {
        REPORT_SYNTAX_ERROR(Statement*, this, "';' expected.");
      }
      return Just(NewStatement<ImportDeclaration>(Just(ib), str));
    }
  }

  REPORT_SYNTAX_ERROR(Statement*, this, "Module specifier expected.");

  EXIT_PARSING
}

Maybe<Expression*> Parser::ParseNamedImports() { return ParseNamedList(); }

Maybe<Expression*> Parser::ParseNameSpaceImport() {
  EXPECT(Expression*, this, cur(), Token::OP_MUL, '*');
  if (cur() == Token::IDENTIFIER && value().IsAsciiEqual("as")) {
    return Just(NewExpression<ImportSpecifier>(ParseIdentifier(),
                                               Nothing<Expression*>(), true));
  }
  REPORT_SYNTAX_ERROR(Expression*, this, "'as' expected.");
}

Maybe<Statement*> Parser::ParseExportDeclaration() {
  EXPECT(Statement*, this, cur(), Token::EXPORT, "export");
  auto exp = NewNode<ExportDeclaration>();
  if (cur() == Token::DEFAULT) {
    exp->set_default_export();
    advance();
    switch (cur()) {
      case Token::CLASS:
        exp->set_export_clause(ParseClassDeclaration());
      case Token::FUNCTION:
        exp->set_export_clause(ParseFunctionDeclaration(false, true));
      case Token::IDENTIFIER:
        if (value().IsAsciiEqual("async")) {
          exp->set_export_clause(ParseAsyncArrowFunction());
        }
      default:
        exp->set_export_clause(ParseAssignmentExpression());
    }
    return Just(exp);
  }

  switch (cur()) {
    case Token::VAR:
      exp->set_export_clause(ParseVariableStatement());
    case Token::CONST:
    case Token::FUNCTION:
    case Token::CLASS:
      exp->set_export_clause(ParseDeclaration());
    case Token::IDENTIFIER:
      if (value().IsAsciiEqual("async")) {
        exp->set_export_clause(ParseAsyncArrowFunction());
      } else if (value().IsAsciiEqual("let")) {
        exp->set_export_clause(ParseDeclaration());
      }
    default:
      exp->set_export_clause(ParseExportClause());
  }

  if (cur() == Token::IDENTIFIER && value().IsAsciiEqual("from")) {
    advance();
    if (cur() == Token::STRING_LITERAL) {
      exp->set_from_clause(
          Just(NewExpression<Literal>(Token::STRING_LITERAL, value().Clone())));
    } else {
      REPORT_SYNTAX_ERROR(Statement*, this, "Module specifier expected.");
    }
  }

  if (cur() == Token::TERMINATE) {
    advance();
  } else if (!tokenizer_->has_linebreak_after()) {
    REPORT_SYNTAX_ERROR(Statement*, this, "';' expected.");
  }

  return Just(exp);
}

Maybe<Expression*> Parser::ParseExportClause() { return ParseNamedList(); }

Maybe<Expression*> Parser::ParseNamedList() {
  EXPECT(Expression*, this, cur(), Token::LEFT_BRACE, '{');

  auto list = NewNode<NamedImportList>();

  while (has_more() && cur() != Token::RIGHT_BRACE) {
    auto maybe_identifier = ParseIdentifierReference();
    if (cur() == Token::IDENTIFIER && value().IsAsciiEqual("as")) {
      advance();
      auto maybe_ref = ParseIdentifier();
      list->Push(NewNode<ImportSpecifier>(maybe_identifier, maybe_ref, false));
    } else {
      list->Push(NewNode<ImportSpecifier>(maybe_identifier,
                                          Nothing<Expression*>(), false));
    }

    if (cur() == Token::COMMA) {
      advance();
    } else if (cur() != Token::RIGHT_BRACE) {
      REPORT_SYNTAX_ERROR(Expression*, this, "'}' expected.");
    }
  }

  return Just(list);
}

Maybe<Expression*> Parser::ParseElementList() { return Nothing<Expression*>(); }
Maybe<Expression*> Parser::ParseTemplateLiteral() {
  ENTER_PARSING;
  auto start = position();
  EXPECT(Expression*, this, cur(), Token::BACK_QUOTE, '`');
  std::vector<Ast*> buffer;

  while (cur() != Token::TEMPLATE_LITERAL) {
    if (value().size() > 0) {
      buffer.push_back(NewExpressionWithPosition<Literal>(
          position(), Token::STRING_LITERAL, value()));
    }
    parser_state_->PushState(IN_TEMPLATE_INTERPOLATION);
    advance();
    ParseExpression() >>= [&](auto expr) { buffer.push_back(expr); };
    parser_state_->PopState(IN_TEMPLATE_INTERPOLATION);
    EXPECT(Expression*, this, cur(), Token::RIGHT_BRACE, '}');
  }

  if (value().size() > 0) {
    buffer.push_back(NewExpressionWithPosition<Literal>(
        position(), Token::STRING_LITERAL, value()));
  }
  auto ast = NewExpressionWithPosition<TemplateLiteral>(start, buffer);
  if (value().size() > 0) {
    ast->set_end_positions(buffer.back()->source_position());
  } else {
    ast->set_end_positions(position());
  }
  parser_state_->PopState(IN_TEMPLATE_LITERAL);
  advance();
  return Just(ast);
  EXIT_PARSING;
}
Maybe<Expression*> Parser::ParseTemplateSpans() {
  return Nothing<Expression*>();
}
Maybe<Expression*> Parser::ParseTemplateMiddleList() {
  return Nothing<Expression*>();
}

Maybe<Expression*> Parser::ParseSuperCall() {
  EXPECT(Expression*, this, cur(), Token::SUPER, "super");
  return ParseArguments() >>= [&, this](auto args) {
    return Just(NewExpression<CallExpression>(Receiver::SUPER, nullptr, args));
  };
}

Maybe<Statement*> Parser::ParseHoistableDeclaration() {
  return Nothing<Statement*>();
}
Maybe<Statement*> Parser::ParseBreakableStatement() {
  return Nothing<Statement*>();
}
Maybe<Statement*> Parser::ParseBlockStatement() {
  return Nothing<Statement*>();
}
Maybe<Statement*> Parser::ParseBlock() { return Nothing<Statement*>(); }
Maybe<Statement*> Parser::ParseLexicalDeclaration() {
  return Nothing<Statement*>();
}
Maybe<Statement*> Parser::ParseLexicalBinding() {
  return Nothing<Statement*>();
}
Maybe<Statement*> Parser::ParseVariableStatement() {
  return Nothing<Statement*>();
}
Maybe<Statement*> Parser::ParseVariableDeclarationList() {
  return Nothing<Statement*>();
}
Maybe<Statement*> Parser::ParseVariableDeclaration() {
  return Nothing<Statement*>();
}
Maybe<Statement*> Parser::ParseIfStatement() { return Nothing<Statement*>(); }
Maybe<Statement*> Parser::ParseIterationStatement() {
  return Nothing<Statement*>();
}
Maybe<Statement*> Parser::ParseForDeclaration() {
  return Nothing<Statement*>();
}
Maybe<Statement*> Parser::ParseForBinding() { return Nothing<Statement*>(); }
Maybe<Statement*> Parser::ParseContinueStatement() {
  return Nothing<Statement*>();
}
Maybe<Statement*> Parser::ParseBreakStatement() {
  return Nothing<Statement*>();
}
Maybe<Statement*> Parser::ParseReturnStatement() {
  return Nothing<Statement*>();
}
Maybe<Statement*> Parser::ParseWithStatement() { return Nothing<Statement*>(); }
Maybe<Statement*> Parser::ParseSwitchStatement() {
  return Nothing<Statement*>();
}
Maybe<Statement*> Parser::ParseCaseBlock() { return Nothing<Statement*>(); }
Maybe<Statement*> Parser::ParseCaseClauses() { return Nothing<Statement*>(); }
Maybe<Statement*> Parser::ParseCaseClause() { return Nothing<Statement*>(); }
Maybe<Statement*> Parser::ParseDefaultClause() { return Nothing<Statement*>(); }
Maybe<Statement*> Parser::ParseLabelledStatement() {
  return Nothing<Statement*>();
}
Maybe<Statement*> Parser::ParseLabelledItem() { return Nothing<Statement*>(); }
Maybe<Statement*> Parser::ParseThrowStatement() {
  return Nothing<Statement*>();
}
Maybe<Statement*> Parser::ParseTryStatement() { return Nothing<Statement*>(); }
Maybe<Statement*> Parser::ParseCatch() { return Nothing<Statement*>(); }
Maybe<Statement*> Parser::ParseFinally() { return Nothing<Statement*>(); }
Maybe<Statement*> Parser::ParseCatchParameter() {
  return Nothing<Statement*>();
}
Maybe<Statement*> Parser::ParseDebuggerStatement() {
  return Nothing<Statement*>();
}

Maybe<Statement*> Parser::ParseFunctionDeclaration(bool async,
                                                   bool in_default) {
  ENTER_PARSING
  auto start = position();
  if (async) {
    advance();
  }
  EXPECT(Statement*, this, cur(), Token::FUNCTION, "function");
  auto identifier = Nothing<Expression*>();

  if (cur() == Token::IDENTIFIER) {
    if (parser_state_->IsInState({IN_ASYNC_FUNCTION, IN_GENERATOR_FUNCTION,
                                  IN_ASYNC_GENERATOR_FUNCTION})) {
      identifier = ParseIdentifier();
    } else {
      identifier = ParseIdentifier();
    }
    advance();
  } else if (!in_default) {
    REPORT_SYNTAX_ERROR(Statement*, this, "Identifier expected");
  }
  auto formal_parameter_position = position();
  EXPECT(Statement*, this, cur(), Token::LEFT_PAREN, '(');
  return ParseFormalParameters() >>= [&, this](auto params) {
    params->set_start_positions(formal_parameter_position);
    params->set_end_positions(position());
    EXPECT(Statement*, this, cur(), Token::RIGHT_PAREN, ')');

    auto body_start_position = position();
    EXPECT(Statement*, this, cur(), Token::LEFT_BRACE, '{');

    return ParseFunctionBody() >>= [&, this](auto body) {
      EXPECT(Statement*, this, cur(), Token::RIGHT_BRACE, '}');
      body->set_start_positions(body_start_position);
      body->set_end_positions(position());
      auto function_type =
          async ? Function::ASYNC
                : parser_state_->IsInState({IN_GENERATOR_FUNCTION})
                      ? Function::GENERATOR
                      : parser_state_->IsInState({IN_ASYNC_GENERATOR_FUNCTION})
                            ? Function::ASYNC_GENERATOR
                            : Function::NORMAL;
      auto fn =
          NewNode<FunctionExpression>(function_type, identifier, params, body);
      fn->set_start_positions(start);
      fn->set_end_positions(previous_position());
      auto expr = NewNode<ExpressionStatement>(fn);
      expr->set_start_positions(start);
      expr->set_end_positions(previous_position());
      return Just(expr->ToStatement());
    };
  };
  EXIT_PARSING
}

bool IsFunctionBodyContinue(Token::Type token) {
  return token != Token::RIGHT_BRACE;
}

Maybe<Statement*> Parser::ParseFunctionBody() {
  ENTER_PARSING
  return ParseStatementList(&IsFunctionBodyContinue);
  EXIT_PARSING
}

Maybe<Expression*> Parser::ParseFunctionExpression() {
  return Nothing<Expression*>();
}
Maybe<Expression*> Parser::ParseArrowFormalParameters() {
  return Nothing<Expression*>();
}
Maybe<Expression*> Parser::ParseAsyncArrowFunction() {
  return Nothing<Expression*>();
}
Maybe<Ast*> Parser::ParseAsyncConciseBody() { return Nothing<Ast*>(); }
Maybe<Ast*> Parser::ParseAsyncArrowHead() { return Nothing<Ast*>(); }
Maybe<Expression*> Parser::ParsePropertySetParameterList() {
  return Nothing<Expression*>();
}
Maybe<Expression*> Parser::ParseGeneratorMethod() {
  return Nothing<Expression*>();
}
Maybe<Ast*> Parser::ParseGeneratorDeclaration() { return Nothing<Ast*>(); }
Maybe<Expression*> Parser::ParseRegularExpression() {
  ENTER_PARSING;
  LUX_SCOPED([&] { parser_state_->PopState(State::REGEXP_EXPECTED); })
  parser_state_->PushState(State::REGEXP_EXPECTED);
  return Nothing<Expression*>();
  EXIT_PARSING;
}
Maybe<Expression*> Parser::ParseGeneratorExpression() {
  return Nothing<Expression*>();
}
Maybe<Ast*> Parser::ParseGeneratorBody() { return Nothing<Ast*>(); }
Maybe<Expression*> Parser::ParseYieldExpression() {
  return Nothing<Expression*>();
}
Maybe<Expression*> Parser::ParseAsyncMethod() { return Nothing<Expression*>(); }
Maybe<Ast*> Parser::ParseAsyncFunctionDeclaration() { return Nothing<Ast*>(); }
Maybe<Ast*> Parser::ParseAsyncFunctionBody() { return Nothing<Ast*>(); }
Maybe<Expression*> Parser::ParseAwaitExpression() {
  return Nothing<Expression*>();
}
Maybe<Ast*> Parser::ParseClassDeclaration() { return Nothing<Ast*>(); }
Maybe<Expression*> Parser::ParseClassExpression() {
  return Nothing<Expression*>();
}
Maybe<Ast*> Parser::ParseClassTail() { return Nothing<Ast*>(); }
Maybe<Ast*> Parser::ParseClassHeritage() { return Nothing<Ast*>(); }
Maybe<Ast*> Parser::ParseClassBody() { return Nothing<Ast*>(); }
Maybe<Ast*> Parser::ParseClassElementList() { return Nothing<Ast*>(); }
Maybe<Ast*> Parser::ParseClassElement() { return Nothing<Ast*>(); }
}  // namespace lux
