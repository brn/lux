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

#ifndef SRC_PARSER_H_
#define SRC_PARSER_H_

#include <stack>
#include <string>
#include <vector>

#include "./source_position.h"
#include "./unicode.h"
#include "./zone.h"

#define KEYWORD_TOKEN_LIST(V, G)                \
  G('a')                                        \
  V(AWAIT, "await")                             \
  G('b')                                        \
  V(BREAK, "break")                             \
  G('c')                                        \
  V(CASE, "case")                               \
  V(CATCH, "catch")                             \
  V(CLASS, "class")                             \
  V(CONST, "const")                             \
  V(CONTINUE, "continue")                       \
  G('d')                                        \
  V(DEBUGGER, "debugger")                       \
  V(DEFAULT, "default")                         \
  V(DELETE, "delete")                           \
  V(DO, "do")                                   \
  G('e')                                        \
  V(ELSE, "else")                               \
  V(EXPORT, "export")                           \
  V(EXTENDS, "extends")                         \
  G('f')                                        \
  V(FINALLY, "finally")                         \
  V(FOR, "for")                                 \
  V(FUNCTION, "function")                       \
  G('i')                                        \
  V(IF, "if")                                   \
  V(IMPORT, "import")                           \
  V(IN, "in")                                   \
  V(INSTANCEOF, "instanceof")                   \
  G('n')                                        \
  V(NEW, "new")                                 \
  V(NULL_VALUE, "null")                         \
  G('r')                                        \
  V(RETURN, "return")                           \
  G('s')                                        \
  V(SUPER, "super")                             \
  V(SWITCH, "switch")                           \
  G('t')                                        \
  V(THIS, "this")                               \
  V(THROW, "throw")                             \
  V(TRY, "try")                                 \
  V(TYPEOF, "typeof")                           \
  G('v')                                        \
  V(VAR, "var")                                 \
  V(VOID, "void")                               \
  G('w')                                        \
  V(WHILE, "while")                             \
  V(WITH, "with")                               \
  G('y')                                        \
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
  V(SPREAD, "...")                                    \
  V(TERMINATE, ";")                                   \
  V(TRUE, "true")

#define LITERAL_TOKEN_LIST(V)                         \
  V(IDENTIFIER, "Identifier")                         \
  V(NUMERIC_LITERAL, "Numeric Literal")               \
  V(REGEXP_LITERAL, "RegExp Literal")                 \
  V(STRING_LITERAL, "String Literal")                 \
  V(TEMPLATE_CHARACTERS, "Template Characters")       \
  V(TEMPLATE_SUBSTITUTION, "Template Substitutions")  \

#define GROUP_DUMMY_(v)
#define TOKEN_LIST(V)                           \
  V(INVALID, "Invalid")                         \
  KEYWORD_TOKEN_LIST(V, GROUP_DUMMY_)           \
  FUTURE_RESERVED_KEYWORD_LIST(V)               \
  SYMBOL_TOKEN_LIST(V)                          \
  LITERAL_TOKEN_LIST(V)

namespace lux {
class ErrorReporter;
class Utf16CodePoint;

#define BASE_AST_TYPES(A)                       \
  A(STATEMENT, Statement, 0)                    \
  A(EXPRESSION, Expression, 0)

#define CONCRETE_AST_TYPES(A)                           \
  A(ARROW_FUNCTION_EXPRESSION,                          \
    ArrowFunctionExpression, 1)                         \
  A(BINARY_EXPRESSION, BinaryExpression, 3)             \
  A(ELISION, Elision, 5)                                \
  A(EXPRESSIONS, Expressions, 7)                        \
  A(CALL_EXPRESSION, CallExpression, 9)                 \
  A(CONDITIONAL_EXPRESSION, ConditionalExpression, 11)  \
  A(FUNCTION_EXPRESSION, FunctionExpression, 13)        \
  A(LITERAL, Literal, 15)                               \
  A(OBJECT_PROPERTY_EXPRESSION,                         \
    ObjectPropertyExpression, 17)                       \
  A(PROPERTY_ACCESS_EXPRESSION,                         \
    PropertyAccessExpression, 19)                       \
  A(STRUCTUAL_LITERAL, StructuralLiteral, 21)           \
  A(UNARY_EXPRESSION, UnaryExpression, 23)              \
  A(STATEMENTS, Statements, 0)

#define AST_TYPES(A)                            \
  BASE_AST_TYPES(A)                             \
  CONCRETE_AST_TYPES(A)

#define FORWARD(T, Type, _) class Type;
AST_TYPES(FORWARD)
#undef FORWARD

struct Token {
  enum Type {
#define DEFINE(TOKEN, _) TOKEN,
    TOKEN_LIST(DEFINE)
#undef DEFINE
  };

  inline static bool OneOf(
      Type base, std::initializer_list<Token::Type> candidate) {
    for (auto &t : candidate) {
      if (t == base) {
        return true;
      }
    }
    return false;
  }
};

struct Receiver {
  enum Type {
    EXPRESSION = 0,
    NEW = 0x4,
    SUPER = 0x8,
    TEMPLATE = 0xa,
  };
};

struct Scope {
  enum Type {
    OPAQUE,
    TRANSPARENT
  };
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
  enum Type {
#define D(V, _ , v) V = v,
    AST_TYPES(D)
#undef D
  };
  static const uint8_t kStatementFlag = 0x0;
  static const uint8_t kExpressionFlag = 0x1;
#define IS_TYPE(TYPE, name, _)                            \
  inline bool Is##name() const {                          \
    return type_ == Ast::TYPE;                            \
  }
  CONCRETE_AST_TYPES(IS_TYPE)
#undef IS_TYPE

  inline bool IsStatement() const {
    return (type() & kStatementFlag) == kStatementFlag;
  }

  inline bool IsExpression() const {
    return (type() & kExpressionFlag) == kExpressionFlag;
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
  explicit Ast(Ast::Type type)
    : type_(type) {}

 private:
  LUX_INLINE uint8_t type() const {
    return static_cast<uint8_t>(type_);
  }

  Ast::Type type_;
};

class Expression: public Ast {
 protected:
  explicit Expression(Ast::Type type)
      : Ast(type) {}
};

class Expressions: public Expression, public AstListTraits<Expression> {
 public:
  Expressions()
      : Expression(Ast::EXPRESSIONS),
        AstListTraits<Expression>() {}
  Expressions(std::initializer_list<Expression*> list)
      : Expression(Ast::EXPRESSIONS),
        AstListTraits<Expression>() {
    for (auto &it : list) {
      Push(it);
    }
  }
};

class BinaryExpression: public Expression {
 public:
  BinaryExpression()
      : Expression(Ast::BINARY_EXPRESSION) {}

  BinaryExpression(Token::Type op,
                   Expression* lhs, Expression* rhs)
      : Expression(Ast::BINARY_EXPRESSION),
        operand_(op), lhs_(lhs), rhs_(rhs) {}

  LUX_CONST_PROPERTY(Expression*, lhs, lhs_);
  LUX_CONST_PROPERTY(Expression*, rhs, rhs_);
  LUX_CONST_PROPERTY(Token::Type, operand, operand_);

 private:
  Token::Type operand_;
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
      : Expression(Ast::UNARY_EXPRESSION) {}

  UnaryExpression(OperandPosition position, Token::Type op, Expression* exp)
      : Expression(Ast::UNARY_EXPRESSION),
        operand_position_(position), operand_(op), expression_(exp) {}

  LUX_CONST_PROPERTY(Token::Type, operand, operand_)
  LUX_CONST_PROPERTY(Expression*, expression, expression_)
  LUX_CONST_PROPERTY(OperandPosition, operand_position, operand_position_)

 private:
  OperandPosition operand_position_;
  Token::Type operand_;
  Expression* expression_;
};

class ConditionalExpression: public Expression {
 public:
  ConditionalExpression()
      : Expression(Ast::CONDITIONAL_EXPRESSION) {}

  ConditionalExpression(
      Expression* condition,
      Expression* then_expression,
      Expression* else_expression)
      : Expression(Ast::CONDITIONAL_EXPRESSION),
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
  CallExpression()
      : Expression(Ast::CALL_EXPRESSION) {}
  CallExpression(Receiver::Type receiver_type,
                 Expression* callee,
                 Expression* arguments = nullptr)
      : Expression(Ast::CALL_EXPRESSION),
        receiver_type_(receiver_type),
        callee_(callee),
        arguments_(arguments) {}

  LUX_CONST_PROPERTY(Receiver::Type, receiver_type, receiver_type_)
  LUX_CONST_PROPERTY(Expression*, callee, callee_)
  LUX_CONST_PROPERTY(Expression*, arguments, arguments_)

 private:
  Receiver::Type receiver_type_;
  Expression* callee_;
  Expression* arguments_;
};

class PropertyAccessExpression: public Expression {
 public:
  enum AccessType: uint8_t {
    DOT = 0x1,
    ELEMENT = 0x2
  };

  static const uint8_t kAccessTypeMask
  = AccessType::DOT | AccessType::ELEMENT;

  static const uint8_t kReceiverTypeMask
  = Receiver::Type::NEW | Receiver::Type::SUPER;

  PropertyAccessExpression():
      Expression(Ast::PROPERTY_ACCESS_EXPRESSION) {}
  PropertyAccessExpression(AccessType access_type,
                           Receiver::Type receiver_type,
                           Expression* receiver,
                           Expression* property):
      Expression(Ast::PROPERTY_ACCESS_EXPRESSION),
      receiver_(receiver),
      property_(property) {
    type_flag_.assign(access_type | receiver_type);
  }

  AccessType access_type() const {
    return type_flag_.mask<AccessType>(kAccessTypeMask);
  }

  Receiver::Type receiver_type() const {
    return type_flag_.mask<Receiver::Type>(kReceiverTypeMask);
  }

  bool is_dot_access() const {
    return type_flag_.get(AccessType::DOT);
  }

  bool is_element_access() const {
    return type_flag_.get(AccessType::ELEMENT);
  }

  bool is_meta_property() const {
    return type_flag_.get(Receiver::Type::NEW);
  }

  bool is_super_property() const {
    return type_flag_.get(Receiver::Type::SUPER);
  }

  LUX_CONST_PROPERTY(Expression*, receiver, receiver_);
  LUX_CONST_PROPERTY(Expression*, property, property_);

 private:
  Bitset<uint8_t> type_flag_;
  Expression* receiver_;
  Expression* property_;
};

struct Function {
#define FUNCTION_TYPE_LIST(A)                   \
  A(NORMAL, normal, 0x4)                        \
  A(ASYNC, async, 0x8)                          \
  A(GENERATOR, generator, 0x10)                 \
  A(GETTER, getter, 0x20)                       \
  A(SETTER, setter, 0x40)

  enum Type {
#define FUNCTION_TYPE_DEF(A, n, v) A = v,
    FUNCTION_TYPE_LIST(FUNCTION_TYPE_DEF)
#undef FUNCTION_TYPE_DEF
  };
};

class BaseFunction {
 public:
  BaseFunction() {}

  BaseFunction(Function::Type fn_type,
                 Scope::Type scope_type,
                 Expression* formal_parameters)
      : formal_parameters_(formal_parameters) {
    flags_.assign(fn_type | scope_type);
  }

#define FUNCTION_TYPE_PROPERTIES(NAME, name, _)     \
  bool is_##name() {                                \
    return flags_.get(Function::NAME);        \
  }                                                 \
  void set_##name() {                               \
    flags_.set(Function::NAME);         \
  }
  FUNCTION_TYPE_LIST(FUNCTION_TYPE_PROPERTIES)
#undef FUNCTION_TYPE_PROPERTIES

  bool is_transparent_scope() {
    return flags_.get(Scope::TRANSPARENT);
  }

  bool is_opaque_scope() {
    return flags_.get(Scope::OPAQUE);
  }

  LUX_CONST_PROPERTY(Expression*, formal_parameters, formal_parameters_)

 protected:
  Bitset<uint8_t> flags_;
  Expression* formal_parameters_;
};

template <bool allow_statements>
class FunctionTraits: public BaseFunction {};

template <>
class FunctionTraits<true>: public BaseFunction {
 public:
  FunctionTraits(Function::Type fn_type,
                 Scope::Type scope_type,
                 Expression* formal_parameters,
                 Statement* body)
      : BaseFunction(fn_type,
                     scope_type,
                     formal_parameters),
        body_(body) {}

  LUX_CONST_PROPERTY(Statement*, body, body_)
 protected:
  Statement* body_;
};

template <>
class FunctionTraits<false>: public BaseFunction {
 public:
  FunctionTraits(Function::Type fn_type,
                 Scope::Type scope_type,
                 Expression* formal_parameters,
                 Ast* body)
      : BaseFunction(fn_type,
                     scope_type,
                     formal_parameters),
        body_(body) {}
  LUX_CONST_PROPERTY(Ast*, body, body_)
 protected:
  Ast* body_;
};

class FunctionExpression: public Expression,
                          public FunctionTraits<true> {
 public:
  FunctionExpression(Function::Type fn_type,
                     Expression* formal_parameters,
                     Statement* body)
      : Expression(Ast::FUNCTION_EXPRESSION),
        FunctionTraits(
            fn_type, Scope::OPAQUE, formal_parameters, body) {}
};

class ArrowFunctionExpression: public Expression,
                               public FunctionTraits<false> {
 public:
  ArrowFunctionExpression(Function::Type fn_type,
                          Expression* formal_parameters,
                          Ast* body)
      : Expression(Ast::ARROW_FUNCTION_EXPRESSION),
        FunctionTraits(
            fn_type, Scope::TRANSPARENT, formal_parameters, body) {}
};

class ObjectPropertyExpression: public Expression {
 public:
  ObjectPropertyExpression()
      : Expression(Ast::OBJECT_PROPERTY_EXPRESSION) {}

  ObjectPropertyExpression(Expression* key, Expression* value,
                           Expression* initializer = nullptr)
      : Expression(Ast::OBJECT_PROPERTY_EXPRESSION),
        key_(key), value_(value), initializer_(initializer) {}

  LUX_CONST_PROPERTY(Expression*, key, key_);
  LUX_CONST_PROPERTY(Expression*, value, value_);
  LUX_CONST_PROPERTY(Expression*, initializer, initializer_);
 private:
  Expression* key_;
  Expression* value_;
  Expression* initializer_;
};

class Elision: public Expression {
 public:
  Elision()
      : Expression(Ast::ELISION) {}
};

class StructuralLiteral: public Expression, public AstListTraits<Expression> {
 public:
  enum Type {
    ARRAY = 0x1,
    OBJECT = 0x2
  };

  enum Flag {
    HAS_ACCESSOR = 0x4,
    HAS_GENERATOR = 0x8,
    HAS_SPREAD = 0x10
  };

  static const uint8_t kTypeMask = Type::ARRAY | Type::OBJECT;
  static const uint8_t kFlagMask =
    Flag::HAS_ACCESSOR | Flag::HAS_GENERATOR | Flag::HAS_SPREAD;

  explicit StructuralLiteral(uint8_t flag)
      : Expression(Ast::STRUCTUAL_LITERAL) {
    flag_.assign(flag);
  }

  LUX_INLINE StructuralLiteral::Type type() const {
    return flag_.mask<Type>(kTypeMask);
  }

  LUX_INLINE StructuralLiteral::Flag flag() const {
    return flag_.mask<Flag>(kFlagMask);
  }

  LUX_INLINE bool is_array_literal() const {
    return flag_.get(ARRAY);
  }

  LUX_INLINE bool is_object_literal() const {
    return flag_.get(OBJECT);
  }

  LUX_INLINE void set_accessor() {
    flag_.set(HAS_ACCESSOR);
  }

  LUX_INLINE bool has_accessor() const {
    return flag_.get(HAS_ACCESSOR);
  }

  LUX_INLINE void set_generator() {
    flag_.set(HAS_GENERATOR);
  }

  LUX_INLINE bool has_generator() const {
    return flag_.get(HAS_GENERATOR);
  }

  LUX_INLINE void set_spread() {
    flag_.set(HAS_SPREAD);
  }

  LUX_INLINE bool has_spread() const {
    return flag_.get(HAS_SPREAD);
  }

 private:
  Bitset<uint8_t> flag_;
};

class Literal: public Expression {
 public:
  Literal()
      : Expression(Ast::LITERAL) {}

  explicit Literal(Token::Type type)
      : Expression(Ast::LITERAL),
        literal_type_(type) {}

  Literal(Token::Type type, Utf16String value)
      : Expression(Ast::LITERAL),
        literal_type_(type),
        value_(value) {}

  bool Is(Token::Type t) {
    return literal_type_ == t;
  }

  Token::Type literal_type() const {
    return literal_type_;
  }

  void set_literal_type(Token::Type t) {
    literal_type_ = t;
  }

  Utf16String value() const {
    return value_;
  }

  void set_value(Utf16String n) {
    value_ = n;
  }

 private:
  Token::Type literal_type_;
  Utf16String value_;
};

class Statement: public Ast {
 protected:
  explicit Statement(Ast::Type type)
      : Ast(type) {}
};

class Statements: public Statement, public AstListTraits<Statement> {
 public:
  Statements()
      : Statement(Ast::STATEMENTS),
        AstListTraits<Statement>() {}
};

class Parser {
 public:
  enum ParseType {
    SCRIPT,
    MODULE
  };

  enum State {
    IN_FUNCTION,
    IN_ASYNC_FUNCTION,
    IN_GENERATOR_FUNCTION,
    IN_ASYNC_GENERATOR_FUNCTION,
    EXPECTED_BINARY_OPERATOR,
    IN_TEMPLATE_LITERAL
  };

  class Allowance {
   public:
#define ALLOWANCE_TYPE_LIST(A)                  \
    A(TEMPLATE, template, 0x1)                  \
    A(CALL, call, 0x2)                          \
    A(BINDING_PATTERN, binding_pattern, 0x4)    \
    A(INITIALIZER, initializer, 0x8)
    enum Type {
#define DEF_ALLOWANCE(V, _, v) V = v,
      ALLOWANCE_TYPE_LIST(DEF_ALLOWANCE)
#undef DEF_ALLOWANCE
    };

    explicit Allowance(uint16_t type) {
      flag_.assign(type);
    }

    Allowance() {}

#define ALLOWANCE_PROPERTIES(V, name, _)          \
    LUX_INLINE void set_##name() {                \
      flag_.set(V);                               \
    }                                             \
    LUX_INLINE bool is_##name##_allowed() const { \
      return flag_.get(V);                        \
    }
    ALLOWANCE_TYPE_LIST(ALLOWANCE_PROPERTIES)
#undef ALLOWANCE_PROPERTIES

    LUX_INLINE void assign(uint16_t flag) {
      flag_.assign(flag);
    }

   private:
    Bitset<uint16_t> flag_;
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
    Token::Type lookahead;
    SourcePosition position;
  };

 public:
  class Tokenizer {
   public:
    enum Flag: uint8_t {
      HAS_LINE_BREAK_BEFORE = 0x1,
      HAS_LINE_BREAK_AFTER = 0x2,
    };
    explicit Tokenizer(Utf16String* sources, const ParserState& parser_state)
        : token_(Token::INVALID),
          it_(sources->begin()),
          end_(sources->end()),
          sources_(sources),
          parser_state_(parser_state) {
      USE(sources_);
    }

    Token::Type Next();

    Token::Type Peek();

    Token::Type Current();

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

    bool has_linebreak_after() const;
    void set_linebreak_after();
    void unset_linebreak_after();

    bool has_linebreak_before() const;
    void set_linebreak_before();
    void unset_linebreak_before();

   private:
    Token::Type Tokenize(Utf16CodePoint);

    Token::Type TokenizeStringLiteral();

    Token::Type TokenizeIdentifier();

    Token::Type TokenizeTemplateCharacters();

    Token::Type TokenizeRegExp();

    Token::Type GetIdentifierType();

    void SkipSingleLineComment();
    void SkipMultiLineComment();

    Utf16CodePoint Advance(bool beginning = false);

    SourcePosition position_;
    SourcePosition lookahead_position_;
    SourcePosition* current_position_;
    Token::Type token_;
    Token::Type lookahead_;
    std::vector<Utf16CodePoint> buffer_;
    std::vector<Utf16CodePoint> lookahead_buffer_;
    std::vector<Utf16CodePoint>* current_buffer_;
    Utf16String::iterator it_;
    Utf16String::iterator end_;
    Utf16String* sources_;
    Bitset<uint8_t> flag_;
    const ParserState& parser_state_;
  };

 private:
  LUX_INLINE Token::Type advance() {
    return tokenizer_->Next();
  }

  LUX_INLINE Token::Type peek() {
    return tokenizer_->Peek();
  }

  LUX_INLINE Token::Type cur() {
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

  Expression* ParseIdentifierReference();
  Expression* ParseIdentifier();
  Expression* ParseAsyncArrowBindingIdentifier();
  Expression* ParseLabelIdentifier();
  Expression* ParsePrimaryExpression();
  Expression* ParseLiteral();
  Expression* ParseCoverParenthesizedExpressionAndArrowParameterList();
  Expression* ParseParenthesizedExpression();
  Expression* ParseRegularExpression();
  Expression* ParseArrayLiteral(
      Parser::Allowance a = Parser::Allowance());
  Expression* ParseElementList();
  Expression* ParseSpreadElement();
  Expression* ParseObjectLiteralProperty(Parser::Allowance);
  Expression* ParseObjectLiteral(
      Parser::Allowance a = Parser::Allowance());
  Expression* ParsePropertyDefinitionList();
  Expression* ParsePropertyDefinition();
  Expression* ParsePropertyName();
  Expression* ParseCoverInitializedName();
  Expression* ParseInitializer();
  Expression* ParseTemplateLiteral();
  Expression* ParseTemplateSpans();
  Expression* ParseTemplateMiddleList();
  Expression* ParseMemberExpression();
  Expression* ParsePostMemberExpression(Expression* pre);
  Expression* ParsePropertyAccessPostExpression(
      Expression*, Receiver::Type, Allowance,
      bool error_if_default = false);
  Expression* ParseSuperProperty();
  Expression* ParseNewTarget();
  Expression* ParseNewExpression();
  Expression* ParseCallExpression();
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
  Ast* ParseLexicalBinding();
  Ast* ParseVariableStatement();
  Ast* ParseVariableDeclarationList();
  Ast* ParseVariableDeclaration();
  Expression* ParseBindingPattern();
  Expression* ParseBindingElement();
  Expression* ParseSingleNameBinding(
      Parser::Allowance a = Parser::Allowance());
  Expression* ParseBindingRestElement();
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
  Expression* ParseFunctionExpression();
  Expression* ParseFormalParameters();
  Expression* ParseFormalParameterList();
  Expression* ParseFunctionRestParameter();
  Statement* ParseFunctionBody();
  Ast* ParseFunctionStatementList();
  Expression* ParseArrowFunction();
  Expression* ParseArrowParameters();
  Ast* ParseConciseBody();
  Expression* ParseArrowFormalParameters();
  Expression* ParseAsyncArrowFunction();
  Ast* ParseAsyncConciseBody();
  Ast* ParseAsyncArrowHead();
  Expression* ParseMethodDefinition();
  Expression* ParsePropertySetParameterList();
  Expression* ParseGeneratorMethod();
  Ast* ParseGeneratorDeclaration();
  Expression* ParseGeneratorExpression();
  Ast* ParseGeneratorBody();
  Expression* ParseYieldExpression();
  Expression* ParseAsyncMethod();
  Ast* ParseAsyncFunctionDeclaration();
  Expression* ParseAsyncFunctionExpression();
  Ast* ParseAsyncFunctionBody();
  Expression* ParseAwaitExpression();
  Ast* ParseClassDeclaration();
  Expression* ParseClassExpression();
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

  LUX_INLINE bool IsAssignmentOperator(Token::Type token) {
    return Token::OneOf(token, {
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

  bool has_linebreak_before() const {
    return tokenizer_->has_linebreak_before();
  }

  bool has_linebreak_after() const {
    return tokenizer_->has_linebreak_after();
  }

  ZoneAllocator* zone() {
    return &zone_allocator_;
  }

#if defined(DEBUG)
  std::string ToStringCurrentToken() {
    switch (cur()) {
#define CASE(T, v) case Token::T : return std::string(v);
      SYMBOL_TOKEN_LIST(CASE)
      KEYWORD_TOKEN_LIST(CASE, GROUP_DUMMY_)
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
}  // namespace lux

#undef GROUP_DUMMY_
#endif  // SRC_PARSER_H_
