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

#ifndef SRC_PARSER_H_
#define SRC_PARSER_H_

#include <stack>
#include <string>
#include <vector>

#include "./source_position.h"
#include "./unicode.h"
#include "./zone.h"

#define KEYWORD_TOKEN_LIST(V)                   \
  V(AWAIT, "await")                             \
  V(BREAK, "break")                             \
  V(CASE, "case")                               \
  V(CATCH, "catch")                             \
  V(CLASS, "class")                             \
  V(CONST, "const")                             \
  V(CONTINUE, "continue")                       \
  V(DEBUGGER, "debugger")                       \
  V(DEFAULT, "default")                         \
  V(DELETE, "delete")                           \
  V(DO, "do")                                   \
  V(ELSE, "else")                               \
  V(EXPORT, "export")                           \
  V(EXTENDS, "extends")                         \
  V(FINALLY, "finally")                         \
  V(FOR, "for")                                 \
  V(FUNCTION, "function")                       \
  V(IF, "if")                                   \
  V(IMPORT, "import")                           \
  V(IN, "in")                                   \
  V(INSTANCEOF, "instanceof")                   \
  V(NEW, "new")                                 \
  V(NULL_VALUE, "null")                         \
  V(RETURN, "return")                           \
  V(SUPER, "super")                             \
  V(SWITCH, "switch")                           \
  V(THIS, "this")                               \
  V(THROW, "throw")                             \
  V(TRY, "try")                                 \
  V(TYPEOF, "typeof")                           \
  V(VAR, "var")                                 \
  V(VOID, "void")                               \
  V(WHILE, "while")                             \
  V(WITH, "with")                               \
  V(YIELD, "yield")

#define FUTURE_RESERVED_KEYWORD_LIST(V)         \
  V(ENUM, "enum")

#define SYMBOL_TOKEN_LIST(V)                         \
  V(BACK_QUOTE, "`")                                  \
  V(COLON, ":")                                       \
  V(COMMA, ",")                                       \
  V(DOT, ".")                                         \
  V(FALSE, "false")                                   \
  V(ARROW_FUNCTION_GLYPH, "=>")                       \
  V(LEFT_BRACE, "{")                                  \
  V(LEFT_BRACKET, "[")                                \
  V(LEFT_PAREN, "(")                                  \
  V(OP_AND, "&")                                      \
  V(OP_AND_ASSIGN, "&=")                              \
  V(OP_ASSIGN, "=")                                   \
  V(OP_DECREMENT, "--")                               \
  V(OP_DIV, "/")                                      \
  V(OP_DIV_ASSIGN, "=")                               \
  V(OP_EQ, "==")                                      \
  V(OP_GREATER_THAN, ">")                             \
  V(OP_GREATER_THAN_EQ, ">=")                         \
  V(OP_INCREMENT, "++")                               \
  V(OP_LESS_THAN, "<")                                \
  V(OP_LESS_THAN_OR_EQ, "<=")                         \
  V(OP_LOGICAL_AND, "&&")                             \
  V(OP_LOGICAL_OR, "||")                              \
  V(OP_MINUS, "-")                                    \
  V(OP_MINUS_ASSIGN, "-=")                            \
  V(OP_MOD, "%")                                      \
  V(OP_MOD_ASSIGN, "%=")                              \
  V(OP_MUL, "*")                                      \
  V(OP_MUL_ASSIGN, "*=")                              \
  V(OP_NOT, "!")                                      \
  V(OP_NOT_EQ, "!=")                                  \
  V(OP_OR, "|")                                       \
  V(OP_OR_ASSIGN, "|=")                               \
  V(OP_PLUS, "+")                                     \
  V(OP_PLUS_ASSIGN, "+=")                             \
  V(OP_POW, "**")                                     \
  V(OP_POW_ASSIGN, "**=")                             \
  V(OP_SHIFT_LEFT, "<<")                              \
  V(OP_SHIFT_LEFT_ASSIGN, "<<=")                      \
  V(OP_SHIFT_RIGHT, ">>")                             \
  V(OP_SHIFT_RIGHT_ASSIGN, ">>=")                     \
  V(OP_STRICT_EQ, "===")                              \
  V(OP_STRICT_NOT_EQ, "!==")                          \
  V(OP_TILDE, "~")                                    \
  V(OP_U_SHIFT_RIGHT, ">>>")                          \
  V(OP_U_SHIFT_RIGHT_ASSIGN, ">>>=")                  \
  V(OP_XOR, "^")                                      \
  V(OP_XOR_ASSIGN, "^=")                              \
  V(QUESTION, "?")                                    \
  V(RIGHT_BRACE, "}")                                 \
  V(RIGHT_BRACKET, "]")                               \
  V(RIGHT_PAREN, ")")                                 \
  V(TERMINATE, ";")                                   \
  V(TRUE, "true")

#define LITERAL_TOKEN_LIST(V)                         \
  V(IDENTIFIER, "Identifier")                         \
  V(NUMERIC_LITERAL, "Numeric Literal")               \
  V(REGEXP_LITERAL, "RegExp Literal")                 \
  V(STRING_LITERAL, "String Literal")                 \
  V(TEMPLATE_CHARACTERS, "Template Characters")       \
  V(TEMPLATE_SUBSTITUTION, "Template Substitutions")  \

#define TOKEN_LIST(V)                           \
  V(INVALID, "Invalid")                         \
  KEYWORD_TOKEN_LIST(V)                         \
  FUTURE_RESERVED_KEYWORD_LIST(V)               \
  SYMBOL_TOKEN_LIST(V)                          \
  LITERAL_TOKEN_LIST(V)

namespace lux {
class ErrorReporter;
class Utf16CodePoint;
class Ast;

#define BASE_AST_TYPES(A)                       \
  A(STATEMENT, Statement, 0)                    \
  A(EXPRESSION, Expression, 0)

#define CONCRETE_AST_TYPES(A)                         \
  A(EXPRESSIONS, Expressions, 1)                      \
  A(BINARY_EXPRESSION, BinaryExpression, 3)           \
  A(CALL_EXPRESSION, CallExpression, 5)               \
  A(CONDITIONAL_EXPRESSION, ConditionalExpression, 7) \
  A(FUNCTION_EXPRESSION, FunctionExpression, 9)       \
  A(LITERAL, Literal, 11)                             \
  A(PROPERTY_ACCESS_EXPRESSION,                       \
    PropertyAccessExpression, 13)                     \
      A(UNARY_EXPRESSION, UnaryExpression, 15)        \
  A(STATEMENTS, Statements, 0)

#define AST_TYPES(A)                            \
  BASE_AST_TYPES(A)                             \
  CONCRETE_AST_TYPES(A)

#define FORWARD(T, Type, _) class Type;
AST_TYPES(FORWARD)
#undef FORWARD

class Parser {
 public:
  enum ParseType {
    SCRIPT,
    MODULE
  };

  enum Token {
#define DEFINE(TOKEN, _) TOKEN,
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

  Parser(Utf16String* sources, ErrorReporter* reporter);

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

  struct TokenizerRecord {
    Utf16String::iterator cursor;
    Token lookahead;
    SourcePosition position;
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

    SourcePosition position() const {
      return position_;
    }

    inline bool HasMore() const {
      return it_ != end_;
    }

    inline Utf16String Value() const {
      return Utf16String(buffer_.data(), buffer_.size());
    }

    inline Utf16String PeekValue() const {
      return Utf16String(lookahead_buffer_.data(), lookahead_buffer_.size());
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

    SourcePosition position_;
    SourcePosition lookahead_position_;
    SourcePosition* current_position_;
    Token token_;
    Token lookahead_;
    std::vector<Utf16CodePoint> buffer_;
    std::vector<Utf16CodePoint> lookahead_buffer_;
    std::vector<Utf16CodePoint>* current_buffer_;
    Utf16String::iterator it_;
    Utf16String::iterator end_;
    Utf16String* sources_;
    const ParserState& parser_state_;
  };

 private:
  LUX_INLINE Token advance() {
    return tokenizer_->Next();
  }

  LUX_INLINE Token peek() {
    return tokenizer_->Peek();
  }

  LUX_INLINE Token cur() {
    return tokenizer_->Current();
  }

  LUX_INLINE Utf16String value() const {
    return tokenizer_->Value();
  }

  LUX_INLINE Utf16String peek_value() const {
    return tokenizer_->PeekValue();
  }

  LUX_INLINE SourcePosition position() const {
    return tokenizer_->position();
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

  Expression* ParseIdentifierReference();
  Expression* ParseBindingIdentifier();
  Expression* ParseIdentifier();
  Expression* ParseAsyncArrowBindingIdentifier();
  Expression* ParseLabelIdentifier();
  Expression* ParsePrimaryExpression();
  Expression* ParseCoverParenthesizedExpressionAndArrowParameterList();
  Expression* ParseParenthesizedExpression();
  Expression* ParseLiteral();
  Expression* ParseArrayLiteral();
  Expression* ParseElementList();
  Expression* ParseSpreadElement();
  Expression* ParseObjectLiteral();
  Expression* ParsePropertyDefinitionList();
  Expression* ParsePropertyDefinition();
  Expression* ParsePropertyName();
  Expression* ParseLiteralPropertyName();
  Expression* ParseComputedPropertyName();
  Expression* ParseCoverInitializedName();
  Expression* ParseInitializer();
  Expression* ParseTemplateLiteral();
  Expression* ParseTemplateSpans();
  Expression* ParseTemplateMiddleList();
  Expression* ParseMemberExpression();
  Expression* ParseSuperProperty();
  Expression* ParseNewTarget();
  Expression* ParseNewExpression();
  Expression* ParseCallExpression();
  Expression* ParsePostCallExpression(Expression* callexp);
  Expression* ParseCoverCallExpressionAndAsyncArrowHead();
  Expression* ParseCallMemberExpression();
  Expression* ParseSuperCall();
  Expression* ParseArguments();
  Expression* ParseArgumentList();
  Expression* ParseLeftHandSideExpression();
  Expression* ParseUpdateExpression();
  Expression* ParseUnaryExpression();
  Expression* ParseExponentiationExpression();
  Expression* ParseMultiplicativeExpression();
  Expression* ParseMultiplicativeOperator();
  Expression* ParseAdditiveExpression();
  Expression* ParseShiftExpression();
  Expression* ParseRelationalExpression();
  Expression* ParseEqualityExpression();
  Expression* ParseBitwiseANDExpression();
  Expression* ParseBitwiseXORExpression();
  Expression* ParseBitwiseORExpression();
  Expression* ParseLogicalANDExpression();
  Expression* ParseLogicalORExpression();
  Expression* ParseConditionalExpression();
  Expression* ParseAssignmentExpression();
  Expression* ParseAssignmentExpressionLhs();
  Expression* ParseAssignmentPattern();
  Expression* ParseObjectAssignmentPattern();
  Expression* ParseArrayAssignmentPattern();
  Expression* ParseAssignmentPropertyList();
  Expression* ParseAssignmentElementList();
  Expression* ParseAssignmentElisionElement();
  Expression* ParseAssignmentProperty();
  Expression* ParseAssignmentElement();
  Expression* ParseAssignmentRestElement();
  Expression* ParseDestructuringAssignmentTarget();
  Expression* ParseExpression();
  Ast* ParseStatement();
  Ast* ParseDeclaration();
  Ast* ParseHoistableDeclaration();
  Ast* ParseBreakableStatement();
  Ast* ParseBlockStatement();
  Ast* ParseBlock();
  Ast* ParseStatementList();
  Ast* ParseStatementListItem();
  Ast* ParseLexicalDeclaration();
  Ast* ParseBindingList();
  Ast* ParseLexicalBinding();
  Ast* ParseVariableStatement();
  Ast* ParseVariableDeclarationList();
  Ast* ParseVariableDeclaration();
  Ast* ParseBindingPattern();
  Ast* ParseObjectBindingPattern();
  Ast* ParseArrayBindingPattern();
  Ast* ParseBindingPropertyList();
  Ast* ParseBindingElementList();
  Ast* ParseBindingElisionElement();
  Ast* ParseBindingProperty();
  Ast* ParseBindingElement();
  Ast* ParseSingleNameBinding();
  Ast* ParseBindingRestElement();
  Ast* ParseExpressionStatement();
  Ast* ParseIfStatement();
  Ast* ParseIterationStatement();
  Ast* ParseForDeclaration();
  Ast* ParseForBinding();
  Ast* ParseContinueStatement();
  Ast* ParseBreakStatement();
  Ast* ParseReturnStatement();
  Ast* ParseWithStatement();
  Ast* ParseSwitchStatement();
  Ast* ParseCaseBlock();
  Ast* ParseCaseClauses();
  Ast* ParseCaseClause();
  Ast* ParseDefaultClause();
  Ast* ParseLabelledStatement();
  Ast* ParseLabelledItem();
  Ast* ParseThrowStatement();
  Ast* ParseTryStatement();
  Ast* ParseCatch();
  Ast* ParseFinally();
  Ast* ParseCatchParameter();
  Ast* ParseDebuggerStatement();
  Ast* ParseFunctionDeclaration();
  Ast* ParseFunctionExpression();
  Ast* ParseUniqueFormalParameters();
  Ast* ParseFormalParameters();
  Ast* ParseFormalParameterList();
  Ast* ParseFunctionRestParameter();
  Ast* ParseFormalParameter();
  Ast* ParseFunctionBody();
  Ast* ParseFunctionStatementList();
  Expression* ParseArrowFunction();
  Expression* ParseArrowParameters();
  Ast* ParseConciseBody();
  Expression* ParseArrowFormalParameters();
  Expression* ParseAsyncArrowFunction();
  Ast* ParseAsyncConciseBody();
  Ast* ParseAsyncArrowHead();
  Ast* ParseMethodDefinition();
  Ast* ParsePropertySetParameterList();
  Ast* ParseGeneratorMethod();
  Ast* ParseGeneratorDeclaration();
  Ast* ParseGeneratorExpression();
  Ast* ParseGeneratorBody();
  Expression* ParseYieldExpression();
  Ast* ParseAsyncMethod();
  Ast* ParseAsyncFunctionDeclaration();
  Expression* ParseAsyncFunctionExpression();
  Ast* ParseAsyncFunctionBody();
  Expression* ParseAwaitExpression();
  Ast* ParseClassDeclaration();
  Ast* ParseClassExpression();
  Ast* ParseClassTail();
  Ast* ParseClassHeritage();
  Ast* ParseClassBody();
  Ast* ParseClassElementList();
  Ast* ParseClassElement();
  Ast* ParseScript();
  Ast* ParseScriptBody();
  Ast* ParseModule();
  Ast* ParseModuleBody();
  Ast* ParseModuleItemList();
  Ast* ParseModuleItem();
  Ast* ParseImportDeclaration();
  Ast* ParseImportClause();
  Ast* ParseImportedDefaultBinding();
  Ast* ParseNameSpaceImport();
  Ast* ParseNamedImports();
  Ast* ParseFromClause();
  Ast* ParseImportsList();
  Ast* ParseImportSpecifier();
  Ast* ParseModuleSpecifier();
  Ast* ParseImportedBinding();
  Ast* ParseExportDeclaration();
  Ast* ParseExportClause();
  Ast* ParseExportsList();
  Ast* ParseExportSpecifier();

  LUX_INLINE bool IsAssignmentOperator(Token token) {
    return OneOf(token, {
        Token::OP_MUL_ASSIGN,
          Token::OP_DIV_ASSIGN,
          Token::OP_PLUS_ASSIGN,
          Token::OP_MINUS_ASSIGN,
          Token::OP_SHIFT_LEFT_ASSIGN,
          Token::OP_SHIFT_RIGHT_ASSIGN,
          Token::OP_U_SHIFT_RIGHT_ASSIGN,
          Token::OP_AND_ASSIGN,
          Token::OP_OR_ASSIGN,
          Token::OP_XOR_ASSIGN,
          Token::OP_POW_ASSIGN
          });
  }

  ZoneAllocator* zone() {
    return &zone_allocator_;
  }

#if defined(DEBUG)
  std::string ToStringCurrentToken() {
    switch (cur()) {
#define CASE(T, v) case Token::T : return std::string(v);
      SYMBOL_TOKEN_LIST(CASE);
      KEYWORD_TOKEN_LIST(CASE);
#undef CASE
#define LITERAL_CASE(T, v) case Token::T :
      LITERAL_TOKEN_LIST(LITERAL_CASE)
        return value().ToUtf8String();
#undef LITERAL_CASE
      default:
        return std::string("Invalid");
    }
  }

  template <bool Print>
  class DebugStream {
   public:
    template <typename T>
    DebugStream& operator << (T value) {
      buffer_ << value;
      if (Print) {
        std::cout << value;
      }
      return *this;
    }


    void PrintStackTrace() {
      printf("%s\n", buffer_.str().c_str());
    }
   private:
    std::stringstream buffer_;
  };

  DebugStream<false> phase_buffer_;
  std::string indent_;
#endif

  TokenizerRecord record_;
  LazyInitializer<ParserState> parser_state_;
  LazyInitializer<Tokenizer> tokenizer_;
  ErrorReporter* reporter_;
  Utf16String* sources_;
  ZoneAllocator zone_allocator_;
};

// Represented by uint8_t
enum AstType: uint8_t {
#define D(V, _ , v) V = v,
  AST_TYPES(D)
#undef D
};

template <typename T>
class AstListTraits {
 public:
  using iterator = typename std::vector<T*>::iterator;
  void Push(T* el) {
    statements_.push_back(el);
  }

  T* at(size_t n) const {
    return statements_[n];
  }

  iterator begin() {
    return statements_.begin();
  }

  iterator end() {
    return statements_.end();
  }

 private:
  std::vector<T*> statements_;
};

class Ast: public Zone {
 public:
  static const uint8_t kStatementFlag = 0x0;
  static const uint8_t kExpressionFlag = 0x1;
#define IS_TYPE(TYPE, name, _)                            \
  inline bool Is##name() const {                          \
    return type_ == AstType::TYPE;                       \
  }
  CONCRETE_AST_TYPES(IS_TYPE)
#undef IS_TYPE

  inline bool IsStatement() const {
    return (type_ & kStatementFlag) == kStatementFlag;
  }

  inline bool IsExpression() const {
    return (type_ & kExpressionFlag) == kExpressionFlag;
  }

#define CAST(TYPE, Type, _)                     \
  Type* To##Type() {                        \
    INVALIDATE(Is##Type());                     \
    return reinterpret_cast<Type*>(this);       \
  }                                             \
  const Type* To##Type() const {            \
    INVALIDATE(Is##Type());                     \
    return reinterpret_cast<const Type*>(this); \
  }
  AST_TYPES(CAST)
#undef CAST

 protected:
  explicit Ast(AstType type)
    : type_(type) {}

 private:
  AstType type_;
};

class Expression: public Ast {
 protected:
  explicit Expression(AstType type)
      : Ast(type) {}
};

class Expressions: public Expression, public AstListTraits<Expression> {
 public:
  Expressions()
      : Expression(AstType::EXPRESSIONS),
        AstListTraits<Expression>() {}
  Expressions(std::initializer_list<Expression*> list)
      : Expression(AstType::EXPRESSIONS),
        AstListTraits<Expression>() {
    for (auto &it : list) {
      Push(it);
    }
  }
};

class BinaryExpression: public Expression {
 public:
  BinaryExpression()
      : Expression(AstType::BINARY_EXPRESSION) {}

  BinaryExpression(Expression* lhs, Expression* rhs)
      : Expression(AstType::BINARY_EXPRESSION),
        lhs_(lhs), rhs_(rhs) {}

  LUX_INLINE Expression* lhs() const {
    return lhs_;
  }

  LUX_INLINE void set_lhs(Expression* n) {
    lhs_ = n;
  }

  LUX_INLINE Expression* rhs() const {
    return rhs_;
  }

  LUX_INLINE void set_rhs(Expression* n) {
    rhs_ = n;
  }

 private:
  Expression* lhs_;
  Expression* rhs_;
};

class UnaryExpression: public Expression {
 public:
  enum OperandPosition {
    PRE,
    POST
  };

  UnaryExpression()
      : Expression(AstType::UNARY_EXPRESSION) {}

  UnaryExpression(OperandPosition position, Parser::Token op, Expression* exp)
      : Expression(AstType::UNARY_EXPRESSION),
        operand_position_(position), operand_(op), expression_(exp) {}

  LUX_CONST_PROPERTY(Parser::Token, operand, operand_)
  LUX_CONST_PROPERTY(Expression*, expression, expression_)
  LUX_CONST_PROPERTY(OperandPosition, operand_position, operand_position_)

 private:
  OperandPosition operand_position_;
  Parser::Token operand_;
  Expression* expression_;
};

class ConditionalExpression: public Expression {
 public:
  ConditionalExpression()
      : Expression(CONDITIONAL_EXPRESSION) {}

  ConditionalExpression(
      Expression* condition,
      Expression* then_expression,
      Expression* else_expression)
      : Expression(CONDITIONAL_EXPRESSION),
        condition_(condition),
        then_expression_(then_expression),
        else_expression_(else_expression) {}

  LUX_INLINE void set_condition(Expression* e) {
    condition_ = e;
  }
  LUX_INLINE Expression* condition() const {
    return condition_;
  }

  LUX_INLINE void set_then_expression(Expression* n) {
    then_expression_ = n;
  }

  LUX_INLINE Expression* then_expression() const {
    return then_expression_;
  }

  LUX_INLINE void set_else_expression(Expression* n) {
    else_expression_ = n;
  }

  LUX_INLINE Expression* else_expression() const {
    return else_expression_;
  }

 private:
  Expression* condition_;
  Expression* then_expression_;
  Expression* else_expression_;
};

class CallExpression: public Expression {
 public:
  enum CallType {
    NORMAL,
    NEW,
    SUPER,
    TEMPLATE
  };
  CallExpression()
      : Expression(AstType::CALL_EXPRESSION) {}
  CallExpression(CallType call_type,
                 Expression* callee,
                 Expression* arguments = nullptr)
      : Expression(AstType::CALL_EXPRESSION),
        call_type_(call_type),
        callee_(callee),
        arguments_(arguments) {}

  LUX_CONST_PROPERTY(CallType, call_type, call_type_)
  LUX_CONST_PROPERTY(Expression*, callee, callee_)
  LUX_CONST_PROPERTY(Expression*, arguments, arguments_)

 private:
  CallType call_type_;
  Expression* callee_;
  Expression* arguments_;
};

class PropertyAccessExpression: public Expression {
 public:
  enum AccessType {
    DOT,
    ELEMENT
  };
  PropertyAccessExpression():
      Expression(AstType::PROPERTY_ACCESS_EXPRESSION) {}
  PropertyAccessExpression(AccessType access_type,
                           Expression* receiver,
                           Expression* property):
      Expression(AstType::PROPERTY_ACCESS_EXPRESSION),
      access_type_(access_type),
      receiver_(receiver),
      property_(property) {}

  LUX_CONST_PROPERTY(AccessType, access_type, access_type_);
  LUX_CONST_PROPERTY(Expression*, receiver, receiver_);
  LUX_CONST_PROPERTY(Expression*, property, property_);

 private:
  AccessType access_type_;
  Expression* receiver_;
  Expression* property_;
};

class FunctionExpression: public Expression {
 public:
  enum ScopeType: uint8_t {
    OPAQUE,
    TRANSPARENT
  };
  enum FunctionType: uint8_t {
    NORMAL = 0,
    ASYNC = 0x1,
    GENERATOR = 0x2
  };
  FunctionExpression()
      : Expression(AstType::FUNCTION_EXPRESSION) {}

  FunctionExpression(FunctionType fn_type,
                     ScopeType scope_type,
                     Expression* formal_parameters,
                     Expression* body)
      : Expression(AstType::FUNCTION_EXPRESSION),
        scope_type_(scope_type),
        formal_parameters_(formal_parameters),
        body_(body) {
    function_type_.assign(fn_type);
  }

  void set_function_type(FunctionType type) {
    function_type_.assign(type);
  }

  bool is_normal_function() {
    return function_type_.get(FunctionType::NORMAL);
  }

  bool is_async_function() {
    return function_type_.get(FunctionType::ASYNC);
  }

  bool is_generator_function() {
    return function_type_.get(FunctionType::GENERATOR);
  }

  bool is_async_generator_function() {
    return is_async_function() && is_generator_function();
  }

  LUX_CONST_PROPERTY(ScopeType, scope_type, scope_type_)
  LUX_CONST_PROPERTY(Expression*, formal_parameters, formal_parameters_)
  LUX_CONST_PROPERTY(Expression*, body, body_)

 private:
  Bitset<uint8_t> function_type_;
  ScopeType scope_type_;
  Expression* formal_parameters_;
  Expression* body_;
};

class Literal: public Expression {
 public:
  Literal()
      : Expression(AstType::LITERAL) {}

  Parser::Token literal_type() const {
    return literal_type_;
  }

  void set_literal_type(Parser::Token t) {
    literal_type_ = t;
  }

  Utf16String* value() const {
    return value_;
  }

  void set_value(Utf16String* n) {
    value_ = n;
  }

 private:
  Parser::Token literal_type_;
  Utf16String* value_;
};

class Statement: public Ast {
 protected:
  explicit Statement(AstType type)
      : Ast(type) {}
};

class Statements: public Statement, public AstListTraits<Statement> {
 public:
  Statements()
      : Statement(AstType::STATEMENTS),
        AstListTraits<Statement>() {}
};
}  // namespace lux

#endif  // SRC_PARSER_H_
