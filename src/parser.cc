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

#include "./parser.h"
#include "./reporter.h"

namespace i6 {
#define REPORT_ERROR(parser, e)                 \
  reporter_->Report(e)

using Token = Parser::Token;

Token Parser::Tokenizer::Next() {
  if (lookahead_ != Token::INVALID) {
    auto ret = lookahead_;
    lookahead_ = Token::INVALID;
    return ret;
  }
  return token_ = Tokenize();
}

Token Parser::Tokenizer::Peek() {
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
      position_.location++;
      position_.position = 0;
      break;
    default:
      position_.position++;
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
  buffer_.clear();
  auto value = *it_;
  buffer_.push_back(value);
  u8 start = value.code();
  bool escaped = false;
  while (1) {
    Advance();
    value = *it_;
    buffer_.push_back(value);
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
  buffer_.clear();
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
    buffer_.push_back(value);
    Advance();
    value = *it_;
  }

  return Token::IDENTIFIER;
}

Token Parser::Tokenizer::TokenizeTemplateCharacters() {
  buffer_.clear();
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
    buffer_.push_back(value);
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

Parser::Parser(Utf16String* sources, Reporter* reporter)
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

void Parser::ParseScript() {
  ParseStatementList();
}

void Parser::ParseStatementList() {
  while (tokenizer_->HasMore()) {
    ParseStatementListItem();
  }
}

void Parser::ParseStatementListItem() {
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
        } else if(v.IsAsciiEqual("let")) {
          return ParseDeclaration();
        }
      }
      ParseStatement();
  }
}

void Parser::ParseStatement() {
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

void Parser::ParseExpressionStatement() {
  ParseExpression();
  if (cur() == TERMINATE) {
    next();
  }
}

void Parser::ParseExpression() {
  while (1) {
    ParseAssignmentExpression();
    if (cur() == COMMA) {
      ParseAssignmentExpression();
    } else {
      break;
    }
  }
}

void Parser::ParseAssignmentExpression() {
  switch (cur()) {
    case YIELD:
      if (!MatchStates({IN_GENERATOR_FUNCTION, IN_ASYNC_GENERATOR_FUNCTION})) {
        REPORT_ERROR(this,
                     "yield only allowed in generator or async generator.");
      }
      return ParseYieldExpression();
    case NEW:
      return ParseLeftHandSideExpression();
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
        next();
        auto next = peek();
        Restore();
        if (next != TERMINATE) {
          return ParseAsyncArrowFunction();
        }
        return ParseIdentifier();
      } else if (cur() == IDENTIFIER && peek() == LEFT_PAREN) {
        return ParseLeftHandSideExpression();
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

void Parser::ParseScriptBody() {}
void Parser::ParseModule() {}
void Parser::ParseModuleBody() {}
void Parser::ParseModuleItemList() {}
void Parser::ParseModuleItem() {}
void Parser::ParseImportDeclaration() {}
void Parser::ParseImportClause() {}
void Parser::ParseImportedDefaultBinding() {}
void Parser::ParseNameSpaceImport() {}
void Parser::ParseNamedImports() {}
void Parser::ParseFromClause() {}
void Parser::ParseImportsList() {}
void Parser::ParseImportSpecifier() {}
void Parser::ParseModuleSpecifier() {}
void Parser::ParseImportedBinding() {}
void Parser::ParseExportDeclaration() {}
void Parser::ParseExportClause() {}
void Parser::ParseExportsList() {}
void Parser::ParseExportSpecifier() {}

void Parser::ParseIdentifierReference() {}
void Parser::ParseBindingIdentifier() {}
void Parser::ParseIdentifier() {}
void Parser::ParseAsyncArrowBindingIdentifier() {}
void Parser::ParseLabelIdentifier() {}
void Parser::ParsePrimaryExpression() {}
void Parser::ParseCoverParenthesizedExpressionAndArrowParameterList() {}
void Parser::ParseParenthesizedExpression() {}
void Parser::ParseLiteral() {}
void Parser::ParseArrayLiteral() {}
void Parser::ParseElementList() {}
void Parser::ParseSpreadElement() {}
void Parser::ParseObjectLiteral() {}
void Parser::ParsePropertyDefinitionList() {}
void Parser::ParsePropertyDefinition() {}
void Parser::ParsePropertyName() {}
void Parser::ParseLiteralPropertyName() {}
void Parser::ParseComputedPropertyName() {}
void Parser::ParseCoverInitializedName() {}
void Parser::ParseInitializer() {}
void Parser::ParseTemplateLiteral() {}
void Parser::ParseTemplateSpans() {}
void Parser::ParseTemplateMiddleList() {}
void Parser::ParseMemberExpression() {}
void Parser::ParseSuperProperty() {}
void Parser::ParseNewTarget() {}
void Parser::ParseNewExpression() {}
void Parser::ParseCallExpression() {}
void Parser::ParseCallMemberExpression() {}
void Parser::ParseSuperCall() {}
void Parser::ParseArguments() {}
void Parser::ParseArgumentList() {}
void Parser::ParseLeftHandSideExpression() {}
void Parser::ParseUpdateExpression() {}
void Parser::ParseUnaryExpression() {}
void Parser::ParseExponentiationExpression() {}
void Parser::ParseMultiplicativeExpression() {}
void Parser::ParseMultiplicativeOperator() {}
void Parser::ParseAdditiveExpression() {}
void Parser::ParseShiftExpression() {}
void Parser::ParseRelationalExpression() {}
void Parser::ParseEqualityExpression() {}
void Parser::ParseBitwiseANDExpression() {}
void Parser::ParseBitwiseXORExpression() {}
void Parser::ParseBitwiseORExpression() {}
void Parser::ParseLogicalANDExpression() {}
void Parser::ParseLogicalORExpression() {}
void Parser::ParseConditionalExpression() {}
void Parser::ParseAssignmentPattern() {}
void Parser::ParseObjectAssignmentPattern() {}
void Parser::ParseArrayAssignmentPattern() {}
void Parser::ParseAssignmentPropertyList() {}
void Parser::ParseAssignmentElementList() {}
void Parser::ParseAssignmentElisionElement() {}
void Parser::ParseAssignmentProperty() {}
void Parser::ParseAssignmentElement() {}
void Parser::ParseAssignmentRestElement() {}
void Parser::ParseDestructuringAssignmentTarget() {}
void Parser::ParseDeclaration() {}
void Parser::ParseHoistableDeclaration() {}
void Parser::ParseBreakableStatement() {}
void Parser::ParseBlockStatement() {}
void Parser::ParseBlock() {}
void Parser::ParseLexicalDeclaration() {}
void Parser::ParseBindingList() {}
void Parser::ParseLexicalBinding() {}
void Parser::ParseVariableStatement() {}
void Parser::ParseVariableDeclarationList() {}
void Parser::ParseVariableDeclaration() {}
void Parser::ParseBindingPattern() {}
void Parser::ParseObjectBindingPattern() {}
void Parser::ParseArrayBindingPattern() {}
void Parser::ParseBindingPropertyList() {}
void Parser::ParseBindingElementList() {}
void Parser::ParseBindingElisionElement() {}
void Parser::ParseBindingProperty() {}
void Parser::ParseBindingElement() {}
void Parser::ParseSingleNameBinding() {}
void Parser::ParseBindingRestElement() {}
void Parser::ParseIfStatement() {}
void Parser::ParseIterationStatement() {}
void Parser::ParseForDeclaration() {}
void Parser::ParseForBinding() {}
void Parser::ParseContinueStatement() {}
void Parser::ParseBreakStatement() {}
void Parser::ParseReturnStatement() {}
void Parser::ParseWithStatement() {}
void Parser::ParseSwitchStatement() {}
void Parser::ParseCaseBlock() {}
void Parser::ParseCaseClauses() {}
void Parser::ParseCaseClause() {}
void Parser::ParseDefaultClause() {}
void Parser::ParseLabelledStatement() {}
void Parser::ParseLabelledItem() {}
void Parser::ParseThrowStatement() {}
void Parser::ParseTryStatement() {}
void Parser::ParseCatch() {}
void Parser::ParseFinally() {}
void Parser::ParseCatchParameter() {}
void Parser::ParseDebuggerStatement() {}
void Parser::ParseFunctionDeclaration() {}
void Parser::ParseFunctionExpression() {}
void Parser::ParseUniqueFormalParameters() {}
void Parser::ParseFormalParameters() {}
void Parser::ParseFormalParameterList() {}
void Parser::ParseFunctionRestParameter() {}
void Parser::ParseFormalParameter() {}
void Parser::ParseFunctionBody() {}
void Parser::ParseFunctionStatementList() {}
void Parser::ParseArrowFunction() {}
void Parser::ParseArrowParameters() {}
void Parser::ParseConciseBody() {}
void Parser::ParseArrowFormalParameters() {}
void Parser::ParseAsyncArrowFunction() {}
void Parser::ParseAsyncConciseBody() {}
void Parser::ParseAsyncArrowHead() {}
void Parser::ParseMethodDefinition() {}
void Parser::ParsePropertySetParameterList() {}
void Parser::ParseGeneratorMethod() {}
void Parser::ParseGeneratorDeclaration() {}
void Parser::ParseGeneratorExpression() {}
void Parser::ParseGeneratorBody() {}
void Parser::ParseYieldExpression() {}
void Parser::ParseAsyncMethod() {}
void Parser::ParseAsyncFunctionDeclaration() {}
void Parser::ParseAsyncFunctionExpression() {}
void Parser::ParseAsyncFunctionBody() {}
void Parser::ParseAwaitExpression() {}
void Parser::ParseClassDeclaration() {}
void Parser::ParseClassExpression() {}
void Parser::ParseClassTail() {}
void Parser::ParseClassHeritage() {}
void Parser::ParseClassBody() {}
void Parser::ParseClassElementList() {}
void Parser::ParseClassElement() {}

const uint8_t Ast::kStatementFlag;
const uint8_t Ast::kExpressionFlag;
}  // namespace i6
