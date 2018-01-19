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

#ifndef _I6_SRC_PARSER_H_
#define _I6_SRC_PARSER_H_

#include <stack>
#include <vector>

#include "./node.h"
#include "./unicode.h"

#define KEYWORD_TOKEN_LIST(V)                     \
  V(AWAIT)                                        \
  V(BREAK)                                        \
  V(CASE)                                         \
  V(CATCH)                                        \
  V(CLASS)                                        \
  V(CONST)                                        \
  V(CONTINUE)                                     \
  V(DEBUGGER)                                     \
  V(DEFAULT)                                      \
  V(DELETE)                                       \
  V(DO)                                           \
  V(ELSE)                                         \
  V(EXPORT)                                       \
  V(EXTENDS)                                      \
  V(FINALLY)                                      \
  V(FOR)                                          \
  V(FUNCTION)                                     \
  V(IF)                                           \
  V(IMPORT)                                       \
  V(IN)                                           \
  V(INSTANCEOF)                                   \
  V(NEW)                                          \
  V(NULL_VALUE)                                   \
  V(RETURN)                                       \
  V(SUPER)                                        \
  V(SWITCH)                                       \
  V(THIS)                                         \
  V(THROW)                                        \
  V(TRY)                                          \
  V(TYPEOF)                                       \
  V(VAR)                                          \
  V(VOID)                                         \
  V(WHILE)                                        \
  V(WITH)                                         \
  V(YIELD)

#define FUTURE_RESERVED_KEYWORD_LIST(V)         \
  V(ENUM)

#define VIRTUAL_TOKEN_LIST(V)                   \
  V(BACK_QUOTE)                                 \
  V(COLON)                                      \
  V(COMMA)                                      \
  V(DOT)                                        \
  V(PUNCTURE)                                   \
  V(FALSE)                                      \
  V(ARROW_FUNCTION_GLYPH)                       \
  V(IDENTIFIER)                                 \
  V(LEFT_BRACE)                                 \
  V(LEFT_BRACKET)                               \
  V(LEFT_PAREN)                                 \
  V(OP_AND)                                     \
  V(OP_AND_ASSIGN)                              \
  V(OP_ASSIGN)                                  \
  V(OP_DECREMENT)                               \
  V(OP_DIV)                                     \
  V(OP_DIV_ASSIGN)                              \
  V(OP_EQ)                                      \
  V(OP_GREATER_THAN)                            \
  V(OP_GREATER_THAN_EQ)                         \
  V(OP_INCREMENT)                               \
  V(OP_LESS_THAN)                               \
  V(OP_LESS_THAN_OR_EQ)                         \
  V(OP_LOGICAL_AND)                             \
  V(OP_LOGICAL_OR)                              \
  V(OP_MINUS)                                   \
  V(OP_MINUS_ASSIGN)                            \
  V(OP_MOD)                                     \
  V(OP_MOD_ASSIGN)                              \
  V(OP_MUL)                                     \
  V(OP_MUL_ASSIGN)                              \
  V(OP_NOT)                                     \
  V(OP_NOT_EQ)                                  \
  V(OP_OR)                                      \
  V(OP_OR_ASSIGN)                               \
  V(OP_PLUS)                                    \
  V(OP_PLUS_ASSIGN)                             \
  V(OP_POW)                                     \
  V(OP_POW_ASSIGN)                              \
  V(OP_SHIFT_LEFT)                              \
  V(OP_SHIFT_LEFT_ASSIGN)                       \
  V(OP_SHIFT_RIGHT)                             \
  V(OP_SHIFT_RIGHT_ASSIGN)                      \
  V(OP_STRICT_EQ)                               \
  V(OP_STRICT_NOT_EQ)                           \
  V(OP_TILDE)                                   \
  V(OP_U_SHIFT_RIGHT)                           \
  V(OP_U_SHIFT_RIGHT_ASSIGN)                    \
  V(OP_XOR)                                     \
  V(OP_XOR_ASSIGN)                              \
  V(NUMERIC_LITERAL)                            \
  V(QUESTION)                                   \
  V(REGEXP_LITERAL)                             \
  V(RIGHT_BRACE)                                \
  V(RIGHT_BRACKET)                              \
  V(RIGHT_PAREN)                                \
  V(STRING_LITERAL)                             \
  V(TEMPLATE_CHARACTERS)                        \
  V(TEMPLATE_SUBSTITUTION)                      \
  V(TERMINATE)                                  \
  V(TRUE)

#define TOKEN_LIST(V)                           \
  V(INVALID)                                    \
  KEYWORD_TOKEN_LIST(V)                         \
  FUTURE_RESERVED_KEYWORD_LIST(V)               \
  VIRTUAL_TOKEN_LIST(V)

namespace i6 {
class Reporter;
class Utf16CodePoint;

class Expression;
class BinaryExpression;
class UnaryExpression;
class Literal;

class Parser {
 public:
  enum ParseType {
    SCRIPT,
    MODULE
  };

  enum Token {
#define DEFINE(t) t,
      TOKEN_LIST(DEFINE)
#undef DEFINE
  };

  enum State {
    IN_FUNCTION,
    IN_ASYNC_FUNCTION,
    IN_GENERATOR_FUNCTION,
    IN_ASYNC_GENERATOR_FUNCTION,
    EXPECTED_BINARY_OPERATOR,
    IN_TEMPLATE_LITERAL
  };

  Parser(Utf16String* sources, Reporter* reporter);

  void Parse(ParseType parse_type);

 private:
  class ParserState {
   public:
    void PushState(State state) {
      state_stack_.push(state);
    }

    void PopState(State state) {
      INVALIDATE(state_stack_.top() == state);
      state_stack_.pop();
    }

    State CurrentState(State state) const {
      return state_stack_.top();
    }

    bool Is(State state) const {
      return state == state_stack_.top();
    }

   private:
    std::stack<State> state_stack_;
  };

  struct Position {
    int32_t position;
    int32_t location;
  };

  struct TokenizerRecord {
    Utf16String::iterator cursor;
    Token lookahead;
    Position position;
  };

 public:
  class Tokenizer {
   public:
    explicit Tokenizer(Utf16String* sources, const ParserState& parser_state)
        : token_(Token::INVALID),
          it_(sources->begin()),
          end_(sources->end()),
          sources_(sources),
          parser_state_(parser_state) {
      USE(sources_);
    }

    Token Next();

    Token Peek();

    Token Current();

    Position position() const {
      return position_;
    }

    inline bool HasMore() {
      return it_ != end_;
    }

    inline Utf16String Value() {
      return Utf16String(buffer_.data(), buffer_.size());
    }

    TokenizerRecord Record();

    void Restore(const TokenizerRecord& record);

   private:
    Token Tokenize();

    Token TokenizeStringLiteral();

    Token TokenizeIdentifier();

    Token TokenizeTemplateCharacters();

    Token TokenizeRegExp();

    Utf16CodePoint Advance();

    Position position_;
    Token token_;
    Token lookahead_;
    std::vector<Utf16CodePoint> buffer_;
    Utf16String::iterator it_;
    Utf16String::iterator end_;
    Utf16String* sources_;
    const ParserState& parser_state_;
  };

 private:
  inline Token next() {
    return tokenizer_->Next();
  }

  inline Token peek() {
    return tokenizer_->Peek();
  }

  inline Token cur() {
    return tokenizer_->Current();
  }

  inline Utf16String value() {
    return tokenizer_->Value();
  }

  inline void PushState(State s) {
    parser_state_->PushState(s);
  }

  inline void PopState(State s) {
    parser_state_->PopState(s);
  }

  inline bool MatchState(State s) {
    return parser_state_->Is(s);
  }

  void Record() {
    record_ = tokenizer_->Record();
  }

  void Restore() {
    tokenizer_->Restore(record_);
  }

  bool MatchStates(std::initializer_list<State> s);

  bool OneOf(Token base, std::initializer_list<Token> candidate);

  void ParseIdentifierReference();
  void ParseBindingIdentifier();
  void ParseIdentifier();
  void ParseAsyncArrowBindingIdentifier();
  void ParseLabelIdentifier();
  void ParsePrimaryExpression();
  void ParseCoverParenthesizedExpressionAndArrowParameterList();
  void ParseParenthesizedExpression();
  void ParseLiteral();
  void ParseArrayLiteral();
  void ParseElementList();
  void ParseSpreadElement();
  void ParseObjectLiteral();
  void ParsePropertyDefinitionList();
  void ParsePropertyDefinition();
  void ParsePropertyName();
  void ParseLiteralPropertyName();
  void ParseComputedPropertyName();
  void ParseCoverInitializedName();
  void ParseInitializer();
  void ParseTemplateLiteral();
  void ParseTemplateSpans();
  void ParseTemplateMiddleList();
  void ParseMemberExpression();
  void ParseSuperProperty();
  void ParseNewTarget();
  void ParseNewExpression();
  void ParseCallExpression();
  void ParseCallMemberExpression();
  void ParseSuperCall();
  void ParseArguments();
  void ParseArgumentList();
  void ParseLeftHandSideExpression();
  void ParseUpdateExpression();
  void ParseUnaryExpression();
  void ParseExponentiationExpression();
  void ParseMultiplicativeExpression();
  void ParseMultiplicativeOperator();
  void ParseAdditiveExpression();
  void ParseShiftExpression();
  void ParseRelationalExpression();
  void ParseEqualityExpression();
  void ParseBitwiseANDExpression();
  void ParseBitwiseXORExpression();
  void ParseBitwiseORExpression();
  void ParseLogicalANDExpression();
  void ParseLogicalORExpression();
  void ParseConditionalExpression();
  void ParseAssignmentExpression();
  void ParseAssignmentPattern();
  void ParseObjectAssignmentPattern();
  void ParseArrayAssignmentPattern();
  void ParseAssignmentPropertyList();
  void ParseAssignmentElementList();
  void ParseAssignmentElisionElement();
  void ParseAssignmentProperty();
  void ParseAssignmentElement();
  void ParseAssignmentRestElement();
  void ParseDestructuringAssignmentTarget();
  void ParseExpression();
  void ParseStatement();
  void ParseDeclaration();
  void ParseHoistableDeclaration();
  void ParseBreakableStatement();
  void ParseBlockStatement();
  void ParseBlock();
  void ParseStatementList();
  void ParseStatementListItem();
  void ParseLexicalDeclaration();
  void ParseBindingList();
  void ParseLexicalBinding();
  void ParseVariableStatement();
  void ParseVariableDeclarationList();
  void ParseVariableDeclaration();
  void ParseBindingPattern();
  void ParseObjectBindingPattern();
  void ParseArrayBindingPattern();
  void ParseBindingPropertyList();
  void ParseBindingElementList();
  void ParseBindingElisionElement();
  void ParseBindingProperty();
  void ParseBindingElement();
  void ParseSingleNameBinding();
  void ParseBindingRestElement();
  void ParseExpressionStatement();
  void ParseIfStatement();
  void ParseIterationStatement();
  void ParseForDeclaration();
  void ParseForBinding();
  void ParseContinueStatement();
  void ParseBreakStatement();
  void ParseReturnStatement();
  void ParseWithStatement();
  void ParseSwitchStatement();
  void ParseCaseBlock();
  void ParseCaseClauses();
  void ParseCaseClause();
  void ParseDefaultClause();
  void ParseLabelledStatement();
  void ParseLabelledItem();
  void ParseThrowStatement();
  void ParseTryStatement();
  void ParseCatch();
  void ParseFinally();
  void ParseCatchParameter();
  void ParseDebuggerStatement();
  void ParseFunctionDeclaration();
  void ParseFunctionExpression();
  void ParseUniqueFormalParameters();
  void ParseFormalParameters();
  void ParseFormalParameterList();
  void ParseFunctionRestParameter();
  void ParseFormalParameter();
  void ParseFunctionBody();
  void ParseFunctionStatementList();
  void ParseArrowFunction();
  void ParseArrowParameters();
  void ParseConciseBody();
  void ParseArrowFormalParameters();
  void ParseAsyncArrowFunction();
  void ParseAsyncConciseBody();
  void ParseAsyncArrowHead();
  void ParseMethodDefinition();
  void ParsePropertySetParameterList();
  void ParseGeneratorMethod();
  void ParseGeneratorDeclaration();
  void ParseGeneratorExpression();
  void ParseGeneratorBody();
  void ParseYieldExpression();
  void ParseAsyncMethod();
  void ParseAsyncFunctionDeclaration();
  void ParseAsyncFunctionExpression();
  void ParseAsyncFunctionBody();
  void ParseAwaitExpression();
  void ParseClassDeclaration();
  void ParseClassExpression();
  void ParseClassTail();
  void ParseClassHeritage();
  void ParseClassBody();
  void ParseClassElementList();
  void ParseClassElement();
  void ParseScript();
  void ParseScriptBody();
  void ParseModule();
  void ParseModuleBody();
  void ParseModuleItemList();
  void ParseModuleItem();
  void ParseImportDeclaration();
  void ParseImportClause();
  void ParseImportedDefaultBinding();
  void ParseNameSpaceImport();
  void ParseNamedImports();
  void ParseFromClause();
  void ParseImportsList();
  void ParseImportSpecifier();
  void ParseModuleSpecifier();
  void ParseImportedBinding();
  void ParseExportDeclaration();
  void ParseExportClause();
  void ParseExportsList();
  void ParseExportSpecifier();

  TokenizerRecord record_;
  LazyInitializer<ParserState> parser_state_;
  LazyInitializer<Tokenizer> tokenizer_;
  Reporter* reporter_;
  Utf16String* sources_;
};

#define BASE_AST_TYPES(A)                       \
  A(STATEMENT, Statement, 0)                    \
  A(EXPRESSION, Expression, 0)


#define CONCRETE_AST_TYPES(A)                         \
  A(CONDITIONAL_EXPRESSION, ConditionalExpression, 1) \
  A(BINARY_EXPRESSION, BinaryExpression, 3)           \
  A(UNARY_EXPRESSION, UnaryExpression, 5)             \
  A(LITERAL, Literal, 7)

#define AST_TYPES(A)                            \
  BASE_AST_TYPES(A)                             \
  CONCRETE_AST_TYPES(A)

#define FORWARD(T, Type, _) class Type;
AST_TYPES(FORWARD)
#undef FORWARD

// Represented by uint8_t
enum AstType: uint8_t {
#define D(V, _ , v) V = v,
  AST_TYPES(D)
#undef D
};

class Ast: public Node<AstType> {
 public:
  static const uint8_t kStatementFlag = 0x0;
  static const uint8_t kExpressionFlag = 0x1;
#define IS_TYPE(TYPE, name, _)                            \
  inline bool Is##name() const {                          \
    return type() == AstType::TYPE;                       \
  }
  CONCRETE_AST_TYPES(IS_TYPE)
#undef IS_TYPE

  inline bool IsStatement() {
    return (type() & kStatementFlag) == kStatementFlag;
  }

  inline bool IsExpression() {
    return (type() & kExpressionFlag) == kExpressionFlag;
  }

#define CAST(TYPE, Type, _)                     \
  static Type* CastTo##Type(Node<AstType>* n) { \
    INVALIDATE(n->Is##Type());                  \
    return reinterpret_cast<Type*>(n);           \
  }
  CONCRETE_AST_TYPES(CAST)
#undef CAST
};

// Expression represented by below layout.
// [ 8bit flag ]
class Expression: public Ast {
 public:
  bool is_parentheses() const;
  void set_parentheses(bool v);
 protected:
  static Expression* New(size_t size, ZoneAllocator* zone_allocator);
};

// BinaryExpression represented by below layout.
// [ 24/56bit padding ]
// [ 32/64bit pointer ]
// [ 32/64bit pointer ]
class BinaryExpression: public Expression {
 public:
  static BinaryExpression* New(ZoneAllocator* zone_allocator);
  void set_lhs(Expression* n);
  void set_rhs(Expression* n);
};

// UnaryExpression represented by below layout.
// [ 8bit operand ]
// [ 24/56bit padding ]
// [ 32/64bit pointer ]
class UnaryExpression: public Expression {
 public:
  static UnaryExpression* New(ZoneAllocator* zone_allocator);
  Parser::Token operand() const;
  void set_expression(Expression* n);
  Expression* expression();
};

// UnaryExpression represented by below layout.
// [ 24/56bit padding ]
// [ 32/64bit pointer ]
// [ 24/56bit padding ]
// [ 32/64bit pointer ]
// [ 24/56bit padding ]
// [ 32/64bit pointer ]
class ConditionalExpression: public Expression {
 public:
  static ConditionalExpression* New(ZoneAllocator* zone_allocator);

  void set_condition(Expression* e);
  Expression* condition() const;

  void set_then_expression(Expression* n);
  Expression* then_expression();

  void set_else_expression(Expression* n);
  Expression* else_expression();
};

// UnaryExpression represented by below layout.
// [ 8bit operand ]
// [ 24/56bit padding ]
// [ 32/64bit pointer ]
class Literal: public Expression {
 public:
  static Literal* New(ZoneAllocator* zone_allocator);

  Parser::Token type() const;
  void set_type(Parser::Token t);

  Utf16String* value() const;
  void set_value(Utf16String* n);
};
}  // namespace i6

#endif  // _I6_SRC_PARSER_H_
