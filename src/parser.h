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

#include <sstream>
#include <string>
#include <vector>

#include "./maybe.h"
#include "./reporter.h"
#include "./source_position.h"
#include "./unicode.h"
#include "./zone.h"

#define KEYWORD_TOKEN_LIST(V, G) \
  G('a')                         \
  V(AWAIT, "await")              \
  G('b')                         \
  V(BREAK, "break")              \
  G('c')                         \
  V(CASE, "case")                \
  V(CATCH, "catch")              \
  V(CLASS, "class")              \
  V(CONST, "const")              \
  V(CONTINUE, "continue")        \
  G('d')                         \
  V(DEBUGGER, "debugger")        \
  V(DEFAULT, "default")          \
  V(DELETE, "delete")            \
  V(DO, "do")                    \
  G('e')                         \
  V(ELSE, "else")                \
  V(EXPORT, "export")            \
  V(EXTENDS, "extends")          \
  G('f')                         \
  V(FINALLY, "finally")          \
  V(FOR, "for")                  \
  V(FUNCTION, "function")        \
  G('i')                         \
  V(IF, "if")                    \
  V(IMPORT, "import")            \
  V(IN, "in")                    \
  V(INSTANCEOF, "instanceof")    \
  G('n')                         \
  V(NEW, "new")                  \
  V(NULL_VALUE, "null")          \
  G('r')                         \
  V(RETURN, "return")            \
  G('s')                         \
  V(SUPER, "super")              \
  V(SWITCH, "switch")            \
  G('t')                         \
  V(THIS, "this")                \
  V(THROW, "throw")              \
  V(TRY, "try")                  \
  V(TYPEOF, "typeof")            \
  G('v')                         \
  V(VAR, "var")                  \
  V(VOID, "void")                \
  G('w')                         \
  V(WHILE, "while")              \
  V(WITH, "with")                \
  G('y')                         \
  V(YIELD, "yield")

#define FUTURE_RESERVED_KEYWORD_LIST(V) V(ENUM, "enum")

#define SYMBOL_TOKEN_LIST(V)         \
  V(BACK_QUOTE, "`")                 \
  V(COLON, ":")                      \
  V(COMMA, ",")                      \
  V(DOT, ".")                        \
  V(FALSE, "false")                  \
  V(ARROW_FUNCTION_GLYPH, "=>")      \
  V(LEFT_BRACE, "{")                 \
  V(LEFT_BRACKET, "[")               \
  V(LEFT_PAREN, "(")                 \
  V(OP_AND, "&")                     \
  V(OP_AND_ASSIGN, "&=")             \
  V(OP_ASSIGN, "=")                  \
  V(OP_DECREMENT, "--")              \
  V(OP_DIV, "/")                     \
  V(OP_DIV_ASSIGN, "=")              \
  V(OP_EQ, "==")                     \
  V(OP_GREATER_THAN, ">")            \
  V(OP_GREATER_THAN_EQ, ">=")        \
  V(OP_INCREMENT, "++")              \
  V(OP_LESS_THAN, "<")               \
  V(OP_LESS_THAN_OR_EQ, "<=")        \
  V(OP_LOGICAL_AND, "&&")            \
  V(OP_LOGICAL_OR, "||")             \
  V(OP_MINUS, "-")                   \
  V(OP_MINUS_ASSIGN, "-=")           \
  V(OP_MOD, "%")                     \
  V(OP_MOD_ASSIGN, "%=")             \
  V(OP_MUL, "*")                     \
  V(OP_MUL_ASSIGN, "*=")             \
  V(OP_NOT, "!")                     \
  V(OP_NOT_EQ, "!=")                 \
  V(OP_OR, "|")                      \
  V(OP_OR_ASSIGN, "|=")              \
  V(OP_PLUS, "+")                    \
  V(OP_PLUS_ASSIGN, "+=")            \
  V(OP_POW, "**")                    \
  V(OP_POW_ASSIGN, "**=")            \
  V(OP_SHIFT_LEFT, "<<")             \
  V(OP_SHIFT_LEFT_ASSIGN, "<<=")     \
  V(OP_SHIFT_RIGHT, ">>")            \
  V(OP_SHIFT_RIGHT_ASSIGN, ">>=")    \
  V(OP_STRICT_EQ, "===")             \
  V(OP_STRICT_NOT_EQ, "!==")         \
  V(OP_TILDE, "~")                   \
  V(OP_U_SHIFT_RIGHT, ">>>")         \
  V(OP_U_SHIFT_RIGHT_ASSIGN, ">>>=") \
  V(OP_XOR, "^")                     \
  V(OP_XOR_ASSIGN, "^=")             \
  V(QUESTION, "?")                   \
  V(RIGHT_BRACE, "}")                \
  V(RIGHT_BRACKET, "]")              \
  V(RIGHT_PAREN, ")")                \
  V(SPREAD, "...")                   \
  V(TERMINATE, ";")                  \
  V(TRUE, "true")

#define LITERAL_TOKEN_LIST(V)                   \
  V(IDENTIFIER, "Identifier")                   \
  V(NUMERIC_LITERAL, "Numeric Literal")         \
  V(REGEXP_LITERAL, "RegExp Literal")           \
  V(STRING_LITERAL, "String Literal")           \
  V(TEMPLATE_CHARACTERS, "Template Characters") \
  V(TEMPLATE_SUBSTITUTION, "Template Substitutions")

#define GROUP_DUMMY_(v)
#define TOKEN_LIST(V)                 \
  V(INVALID, "Invalid")               \
  V(END, "End")                       \
  KEYWORD_TOKEN_LIST(V, GROUP_DUMMY_) \
  FUTURE_RESERVED_KEYWORD_LIST(V)     \
  SYMBOL_TOKEN_LIST(V)                \
  LITERAL_TOKEN_LIST(V)

namespace lux {
class ErrorReporter;
class Utf16CodePoint;

#define BASE_AST_TYPES(A)    \
  A(STATEMENT, Statement, 0) \
  A(EXPRESSION, Expression, 0)

#define CONCRETE_AST_TYPES(A)                                 \
  A(ARROW_FUNCTION_EXPRESSION, ArrowFunctionExpression, 1)    \
  A(BINARY_EXPRESSION, BinaryExpression, 3)                   \
  A(ELISION, Elision, 5)                                      \
  A(EXPRESSIONS, Expressions, 7)                              \
  A(CALL_EXPRESSION, CallExpression, 9)                       \
  A(CONDITIONAL_EXPRESSION, ConditionalExpression, 11)        \
  A(FUNCTION_EXPRESSION, FunctionExpression, 13)              \
  A(LITERAL, Literal, 15)                                     \
  A(OBJECT_PROPERTY_EXPRESSION, ObjectPropertyExpression, 17) \
  A(PROPERTY_ACCESS_EXPRESSION, PropertyAccessExpression, 19) \
  A(STRUCTUAL_LITERAL, StructuralLiteral, 21)                 \
  A(UNARY_EXPRESSION, UnaryExpression, 23)                    \
  A(IMPORT_SPECIFIER, ImportSpecifier, 25)                    \
  A(IMPORT_BINDING, ImportBinding, 27)                        \
  A(NAMED_IMPORT_LIST, NamedImportList, 29)                   \
  A(STATEMENTS, Statements, 0)                                \
  A(IMPORT_DECLARATION, ImportDeclaration, 2)                 \
  A(EXPORT_DECLARATION, ExportDeclaration, 4)                 \
  A(EXPRESSION_STATEMENT, ExpressionStatement, 6)

#define AST_TYPES(A) \
  BASE_AST_TYPES(A)  \
  CONCRETE_AST_TYPES(A)

#define FORWARD(T, Type, _) class Type;
AST_TYPES(FORWARD)
#undef FORWARD

struct Token {
  enum Type {
#define DEFINE(TOKEN, _) TOKEN,
    TOKEN_LIST(DEFINE)
#undef DEFINE
        LAST__
  };

  inline static bool OneOf(Type base,
                           std::initializer_list<Token::Type> candidate) {
    for (auto& t : candidate) {
      if (t == base) {
        return true;
      }
    }
    return false;
  }

  static const char* ToString(Type token) {
    static const std::array<const char*, Token::LAST__> kTokenNameArray = {{
#define DEF_TOKEN(T, _) #T,
        TOKEN_LIST(DEF_TOKEN)
#undef DEF_TOKEN
    }};
    return kTokenNameArray[token];
  }
};

#define RECEIVER_TYPE_LIST(A) \
  A(EXPRESSION, 0)            \
  A(NEW, 0x4)                 \
  A(SUPER, 0x8)               \
  A(TEMPLATE, 0xa)

struct Receiver {
  enum Type {
#define DEFINE_RECIEVER(R, v) R = v,
    RECEIVER_TYPE_LIST(DEFINE_RECIEVER)
#undef DEFINE_RECIEVER
  };

  static const char* ToString(Type type) {
    switch (type) {
#define DEFINE_RECIEVER(R, v) \
  case R:                     \
    return #R;
      RECEIVER_TYPE_LIST(DEFINE_RECIEVER)
#undef DEFINE_RECIEVER
      default:
        UNREACHABLE();
    }
  }
};

struct Scope {
  enum Type { OPAQUE, TRANSPARENT };
};

template <typename T>
class AstListTraits {
 public:
  using iterator = typename std::vector<T*>::iterator;
  using const_iterator = typename std::vector<T*>::const_iterator;
  void Push(T* el) { node_list_.push_back(el); }

  T* at(size_t n) const { return node_list_[n]; }

  iterator begin() { return node_list_.begin(); }

  iterator end() { return node_list_.end(); }

  const_iterator begin() const { return node_list_.cbegin(); }

  const_iterator end() const { return node_list_.cend(); }

 protected:
  void ToStringList(std::string* indent, std::stringstream* ss) const {
    auto ni = "  " + (*indent);
    for (auto& n : *this) {
      n->ToStringTree(&ni, ss);
    }
  }

 private:
  std::vector<T*> node_list_;
};

class Ast : public Zone {
 public:
  enum Type {
#define D(V, _, v) V = v,
    AST_TYPES(D)
#undef D
        LAST__
  };
  static const uint8_t kStatementFlag = 0x0;
  static const uint8_t kExpressionFlag = 0x1;
#define IS_TYPE(TYPE, name, _) \
  inline bool Is##name() const { return type_ == Ast::TYPE; }
  CONCRETE_AST_TYPES(IS_TYPE)
#undef IS_TYPE

  inline const char* GetName() const {
    switch (type_) {
#define DEF_AST_VALUE(Type, name, _) \
  case Type:                         \
    return #name;
      CONCRETE_AST_TYPES(DEF_AST_VALUE)
#undef DEF_AST_VALUE
    }
  }

  inline bool IsStatement() const {
    return (type() & kStatementFlag) == kStatementFlag;
  }

  inline bool IsExpression() const {
    return (type() & kExpressionFlag) == kExpressionFlag;
  }

  void set_start_positions(uint32_t col, uint32_t line) {
    source_position_.set_start_col(col);
    source_position_.set_start_line_number(line);
  }

  void set_end_positions(uint32_t col, uint32_t line) {
    source_position_.set_end_col(col);
    source_position_.set_end_line_number(line);
  }

  void set_start_positions(const SourcePosition& pos) {
    source_position_.set_start_col(pos.start_col());
    source_position_.set_start_line_number(pos.start_line_number());
  }

  void set_end_positions(const SourcePosition& pos) {
    source_position_.set_end_col(pos.end_col());
    source_position_.set_end_line_number(pos.end_line_number());
  }

  void set_start_pos(uint32_t start) { source_position_.set_start_col(start); }

  void set_end_pos(uint32_t end) { source_position_.set_end_col(end); }

  void set_start_line_number(uint32_t start_line) {
    source_position_.set_start_line_number(start_line);
  }

  void set_end_line_number(uint32_t end_line) {
    source_position_.set_end_line_number(end_line);
  }

  LUX_CONST_PROPERTY(SourcePosition, source_position, source_position_)

  std::string ToString() const {
    std::string indent = "";
    std::stringstream ss;
    ToStringSelf(this, &indent, &ss);
    return ss.str();
  }
  std::string ToStringTree() const {
    std::string indent = "";
    std::stringstream ss;
    ToStringTree(&indent, &ss);
    return ss.str();
  }

  virtual void ToStringSelf(const Ast* ast, std::string* indent,
                            std::stringstream* ss) const = 0;
  virtual void ToStringTree(std::string* indent,
                            std::stringstream* ss) const = 0;

#define CAST(TYPE, Type, _)                     \
  Type* To##Type() {                            \
    INVALIDATE(Is##Type());                     \
    return reinterpret_cast<Type*>(this);       \
  }                                             \
  const Type* To##Type() const {                \
    INVALIDATE(Is##Type());                     \
    return reinterpret_cast<const Type*>(this); \
  }
  AST_TYPES(CAST)
#undef CAST

 protected:
  explicit Ast(Ast::Type type) : type_(type) {}

 private:
  LUX_INLINE uint8_t type() const { return static_cast<uint8_t>(type_); }

  SourcePosition source_position_;
  Ast::Type type_;
};

class Expression : public Ast {
 protected:
  explicit Expression(Ast::Type type) : Ast(type) {}
};

class Expressions : public Expression, public AstListTraits<Expression> {
 public:
  Expressions() : Expression(Ast::EXPRESSIONS), AstListTraits<Expression>() {}
  Expressions(std::initializer_list<Expression*> list)
      : Expression(Ast::EXPRESSIONS), AstListTraits<Expression>() {
    for (auto& it : list) {
      Push(it);
    }
  }

  void ToStringSelf(const Ast* ast, std::string* indent,
                    std::stringstream* ss) const {
    (*ss) << *indent << '[' << ast->GetName() << ' '
          << source_position().ToString() << ']' << '\n';
  }
  void ToStringTree(std::string* indent, std::stringstream* ss) const {
    ToStringSelf(this, indent, ss);
    ToStringList(indent, ss);
  }
};

class BinaryExpression : public Expression {
 public:
  BinaryExpression() : Expression(Ast::BINARY_EXPRESSION) {}

  BinaryExpression(Token::Type op, Expression* lhs, Expression* rhs)
      : Expression(Ast::BINARY_EXPRESSION),
        operand_(op),
        lhs_(lhs),
        rhs_(rhs) {}

  LUX_CONST_PROPERTY(Expression*, lhs, lhs_);
  LUX_CONST_PROPERTY(Expression*, rhs, rhs_);
  LUX_CONST_PROPERTY(Token::Type, operand, operand_);

  void ToStringSelf(const Ast* ast, std::string* indent,
                    std::stringstream* ss) const {
    (*ss) << *indent << '[' << ast->GetName()
          << " operand = " << Token::ToString(operand_) << ' '
          << source_position().ToString() << ']' << '\n';
  }
  void ToStringTree(std::string* indent, std::stringstream* ss) const {
    ToStringSelf(this, indent, ss);
    auto ni = "  " + (*indent);
    lhs_->ToStringTree(&ni, ss);
    rhs_->ToStringTree(&ni, ss);
  }

 private:
  Token::Type operand_;
  Expression* lhs_;
  Expression* rhs_;
};

class UnaryExpression : public Expression {
 public:
  enum OperandPosition { PRE, POST };

  UnaryExpression() : Expression(Ast::UNARY_EXPRESSION) {}

  UnaryExpression(OperandPosition position, Token::Type op, Expression* exp)
      : Expression(Ast::UNARY_EXPRESSION),
        operand_position_(position),
        operand_(op),
        expression_(exp) {}

  LUX_CONST_PROPERTY(Token::Type, operand, operand_)
  LUX_CONST_PROPERTY(Expression*, expression, expression_)
  LUX_CONST_PROPERTY(OperandPosition, operand_position, operand_position_)

  void ToStringSelf(const Ast* ast, std::string* indent,
                    std::stringstream* ss) const {
    (*ss) << *indent << '[' << ast->GetName()
          << " operand = " << Token::ToString(operand_)
          << " position = " << (operand_position_ == PRE ? "PRE" : "POST")
          << ' ' << source_position().ToString() << ']' << '\n';
  }
  void ToStringTree(std::string* indent, std::stringstream* ss) const {
    ToStringSelf(this, indent, ss);
    auto ni = "  " + (*indent);
    expression_->ToStringTree(&ni, ss);
  }

 private:
  OperandPosition operand_position_;
  Token::Type operand_;
  Expression* expression_;
};

class ConditionalExpression : public Expression {
 public:
  ConditionalExpression() : Expression(Ast::CONDITIONAL_EXPRESSION) {}

  ConditionalExpression(Expression* condition, Expression* then_expression,
                        Expression* else_expression)
      : Expression(Ast::CONDITIONAL_EXPRESSION),
        condition_(condition),
        then_expression_(then_expression),
        else_expression_(else_expression) {}

  LUX_INLINE void set_condition(Expression* e) { condition_ = e; }
  LUX_INLINE Expression* condition() const { return condition_; }

  LUX_INLINE void set_then_expression(Expression* n) { then_expression_ = n; }

  LUX_INLINE Expression* then_expression() const { return then_expression_; }

  LUX_INLINE void set_else_expression(Expression* n) { else_expression_ = n; }

  LUX_INLINE Expression* else_expression() const { return else_expression_; }

  void ToStringSelf(const Ast* ast, std::string* indent,
                    std::stringstream* ss) const {
    (*ss) << *indent << '[' << ast->GetName() << ' '
          << source_position().ToString() << ']' << '\n';
  }
  void ToStringTree(std::string* indent, std::stringstream* ss) const {
    ToStringSelf(this, indent, ss);
    auto ni = "  " + (*indent);
    condition_->ToStringTree(&ni, ss);
    then_expression_->ToStringTree(&ni, ss);
    else_expression_->ToStringTree(&ni, ss);
  }

 private:
  Expression* condition_;
  Expression* then_expression_;
  Expression* else_expression_;
};

class CallExpression : public Expression {
 public:
  CallExpression() : Expression(Ast::CALL_EXPRESSION) {}
  CallExpression(Receiver::Type receiver_type, Expression* callee,
                 Expression* arguments = nullptr)
      : Expression(Ast::CALL_EXPRESSION),
        receiver_type_(receiver_type),
        callee_(callee),
        arguments_(arguments) {}

  LUX_CONST_PROPERTY(Receiver::Type, receiver_type, receiver_type_)
  LUX_CONST_PROPERTY(Expression*, callee, callee_)
  LUX_CONST_PROPERTY(Expression*, arguments, arguments_)

  void ToStringSelf(const Ast* ast, std::string* indent,
                    std::stringstream* ss) const {
    (*ss) << *indent << '[' << ast->GetName()
          << " receiver = " << Receiver::ToString(receiver_type_) << ' '
          << source_position().ToString() << ']' << '\n';
  }
  void ToStringTree(std::string* indent, std::stringstream* ss) const {
    ToStringSelf(this, indent, ss);
    auto ni = "  " + (*indent);
    callee_->ToStringTree(&ni, ss);
    arguments_->ToStringTree(&ni, ss);
  }

 private:
  Receiver::Type receiver_type_;
  Expression* callee_;
  Expression* arguments_;
};

class PropertyAccessExpression : public Expression {
 public:
  enum AccessType : uint8_t { DOT = 0x1, ELEMENT = 0x2 };

  static const uint8_t kAccessTypeMask = AccessType::DOT | AccessType::ELEMENT;

  static const uint8_t kReceiverTypeMask =
      Receiver::Type::NEW | Receiver::Type::SUPER;

  PropertyAccessExpression() : Expression(Ast::PROPERTY_ACCESS_EXPRESSION) {}
  PropertyAccessExpression(AccessType access_type, Receiver::Type receiver_type,
                           Expression* receiver, Expression* property)
      : Expression(Ast::PROPERTY_ACCESS_EXPRESSION),
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

  bool is_dot_access() const { return type_flag_.get(AccessType::DOT); }

  bool is_element_access() const { return type_flag_.get(AccessType::ELEMENT); }

  bool is_meta_property() const { return type_flag_.get(Receiver::Type::NEW); }

  bool is_super_property() const {
    return type_flag_.get(Receiver::Type::SUPER);
  }

  LUX_CONST_PROPERTY(Expression*, receiver, receiver_);
  LUX_CONST_PROPERTY(Expression*, property, property_);

  void ToStringSelf(const Ast* ast, std::string* indent,
                    std::stringstream* ss) const {
    (*ss) << *indent << '[' << ast->GetName() << " property_access = ";
    if (is_dot_access()) {
      (*ss) << " dot";
    } else if (is_element_access()) {
      (*ss) << " element";
    } else if (is_meta_property()) {
      (*ss) << " meta";
    } else {
      INVALIDATE(is_super_property());
      (*ss) << " super";
    }
    (*ss) << ' ' << source_position().ToString() << "]\n";
  }
  void ToStringTree(std::string* indent, std::stringstream* ss) const {
    ToStringSelf(this, indent, ss);
    auto ni = "  " + (*indent);
    receiver_->ToStringTree(&ni, ss);
    property_->ToStringTree(&ni, ss);
  }

 private:
  Bitset<uint8_t> type_flag_;
  Expression* receiver_;
  Expression* property_;
};

struct Function {
#define FUNCTION_TYPE_LIST(A)               \
  A(NORMAL, normal, 0x4)                    \
  A(ASYNC, async, 0x8)                      \
  A(GENERATOR, generator, 0x10)             \
  A(ASYNC_GENERATOR, async_generator, 0x20) \
  A(GETTER, getter, 0x40)                   \
  A(SETTER, setter, 0x80)

  enum Type {
#define FUNCTION_TYPE_DEF(A, n, v) A = v,
    FUNCTION_TYPE_LIST(FUNCTION_TYPE_DEF)
#undef FUNCTION_TYPE_DEF
  };
};

class BaseFunction {
 public:
  BaseFunction() {}

  BaseFunction(Function::Type fn_type, Scope::Type scope_type,
               Maybe<Expression*> name, Expression* formal_parameters,
               Ast* body)
      : name_(name), formal_parameters_(formal_parameters), body_(body) {
    flags_.assign(fn_type | scope_type);
  }

#define FUNCTION_TYPE_PROPERTIES(NAME, name, _)           \
  bool is_##name() { return flags_.get(Function::NAME); } \
  void set_##name() { flags_.set(Function::NAME); }
  FUNCTION_TYPE_LIST(FUNCTION_TYPE_PROPERTIES)
#undef FUNCTION_TYPE_PROPERTIES

  bool is_transparent_scope() { return flags_.get(Scope::TRANSPARENT); }

  bool is_opaque_scope() { return flags_.get(Scope::OPAQUE); }

  LUX_CONST_PROPERTY(Maybe<Expression*>, name, name_)

  LUX_CONST_PROPERTY(Expression*, formal_parameters, formal_parameters_)

 protected:
  Bitset<uint8_t> flags_;
  Maybe<Expression*> name_;
  Expression* formal_parameters_;
  Ast* body_;
};

template <bool allow_statements>
class FunctionTraits : public BaseFunction {};

template <>
class FunctionTraits<true> : public BaseFunction {
 public:
  FunctionTraits(Function::Type fn_type, Scope::Type scope_type,
                 Maybe<Expression*> name, Expression* formal_parameters,
                 Ast* body)
      : BaseFunction(fn_type, scope_type, name, formal_parameters, body) {}

  LUX_CONST_GETTER(Statement*, body, reinterpret_cast<Statement*>(body_))
  LUX_SETTER(Ast*, body, body_)
};

template <>
class FunctionTraits<false> : public BaseFunction {
 public:
  FunctionTraits(Function::Type fn_type, Scope::Type scope_type,
                 Maybe<Expression*> name, Expression* formal_parameters,
                 Ast* body)
      : BaseFunction(fn_type, scope_type, name, formal_parameters, body) {}
  LUX_CONST_PROPERTY(Ast*, body, body_)
};

template <bool allow_statements>
class FunctionExpressionBase : public Expression,
                               public FunctionTraits<allow_statements> {
 public:
  FunctionExpressionBase(Ast::Type type, Function::Type fn_type,
                         Scope::Type scope_type, Maybe<Expression*> name,
                         Expression* formal_parameters, Ast* body)
      : Expression(type),
        FunctionTraits<allow_statements>(fn_type, scope_type, name,
                                         formal_parameters, body) {}

  void ToStringSelf(const Ast* ast, std::string* indent,
                    std::stringstream* ss) const {
    (*ss) << *indent << '[' << ast->GetName() << ' '
          << source_position().ToString() << ']' << '\n';
  }

  void ToStringTree(std::string* indent, std::stringstream* ss) const {
    ToStringSelf(this, indent, ss);
    auto ni = "  " + (*indent);
    BaseFunction::name_ >>= [&](auto n) { n->ToStringTree(&ni, ss); };
    BaseFunction::formal_parameters_->ToStringTree(&ni, ss);
    BaseFunction::body_->ToStringTree(&ni, ss);
  }
};

class FunctionExpression : public FunctionExpressionBase<true> {
 public:
  FunctionExpression(Function::Type fn_type, Maybe<Expression*> name,
                     Expression* formal_parameters, Statement* body)
      : FunctionExpressionBase<true>(Ast::FUNCTION_EXPRESSION, fn_type,
                                     Scope::OPAQUE, name, formal_parameters,
                                     reinterpret_cast<Ast*>(body)) {}
};

class ArrowFunctionExpression : public FunctionExpressionBase<false> {
 public:
  ArrowFunctionExpression(Function::Type fn_type, Maybe<Expression*> name,
                          Expression* formal_parameters, Ast* body)
      : FunctionExpressionBase<false>(Ast::ARROW_FUNCTION_EXPRESSION, fn_type,
                                      Scope::TRANSPARENT, name,
                                      formal_parameters, body) {}
};

class ObjectPropertyExpression : public Expression {
 public:
  ObjectPropertyExpression() : Expression(Ast::OBJECT_PROPERTY_EXPRESSION) {}

  ObjectPropertyExpression(Expression* key, Expression* value,
                           Expression* initializer = nullptr)
      : Expression(Ast::OBJECT_PROPERTY_EXPRESSION),
        key_(key),
        value_(value),
        initializer_(initializer) {}

  LUX_CONST_PROPERTY(Expression*, key, key_);
  LUX_CONST_PROPERTY(Expression*, value, value_);
  LUX_CONST_PROPERTY(Expression*, initializer, initializer_);

  void ToStringSelf(const Ast* ast, std::string* indent,
                    std::stringstream* ss) const {
    (*ss) << *indent << '[' << ast->GetName() << ' '
          << source_position().ToString() << ']' << '\n';
  }
  void ToStringTree(std::string* indent, std::stringstream* ss) const {
    ToStringSelf(this, indent, ss);
    auto ni = "  " + (*indent);
    key_->ToStringTree(&ni, ss);
    if (value_) {
      value_->ToStringTree(&ni, ss);
    }
    if (initializer_) {
      initializer_->ToStringTree(&ni, ss);
    }
  }

 private:
  Expression* key_;
  Expression* value_;
  Expression* initializer_;
};

class Elision : public Expression {
 public:
  Elision() : Expression(Ast::ELISION) {}

  void ToStringSelf(const Ast* ast, std::string* indent,
                    std::stringstream* ss) const {
    (*ss) << *indent << '[' << ast->GetName() << ' '
          << source_position().ToString() << ']' << '\n';
  }

  void ToStringTree(std::string* indent, std::stringstream* ss) const {
    ToStringSelf(this, indent, ss);
  }
};

class StructuralLiteral : public Expression, public AstListTraits<Expression> {
 public:
  enum Type { ARRAY = 0x1, OBJECT = 0x2 };

  enum Flag { HAS_ACCESSOR = 0x4, HAS_GENERATOR = 0x8, HAS_SPREAD = 0x10 };

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

  LUX_INLINE bool is_array_literal() const { return flag_.get(ARRAY); }

  LUX_INLINE bool is_object_literal() const { return flag_.get(OBJECT); }

  LUX_INLINE void set_accessor() { flag_.set(HAS_ACCESSOR); }

  LUX_INLINE bool has_accessor() const { return flag_.get(HAS_ACCESSOR); }

  LUX_INLINE void set_generator() { flag_.set(HAS_GENERATOR); }

  LUX_INLINE bool has_generator() const { return flag_.get(HAS_GENERATOR); }

  LUX_INLINE void set_spread() { flag_.set(HAS_SPREAD); }

  LUX_INLINE bool has_spread() const { return flag_.get(HAS_SPREAD); }

  void ToStringSelf(const Ast* ast, std::string* indent,
                    std::stringstream* ss) const {
    (*ss) << *indent << '[' << ast->GetName() << " type = ";
    if (is_array_literal()) {
      (*ss) << "ArrayLiteral";
    } else {
      INVALIDATE(is_object_literal());
      (*ss) << "ObjectLiteral";
    }
    if (has_accessor()) {
      (*ss) << " accessor = true";
    }
    if (has_generator()) {
      (*ss) << " generator = true";
    }
    if (has_spread()) {
      (*ss) << " spread = true";
    }
    (*ss) << ' ' << source_position().ToString() << "]\n";
  }
  void ToStringTree(std::string* indent, std::stringstream* ss) const {
    ToStringSelf(this, indent, ss);
    ToStringList(indent, ss);
  }

 private:
  Bitset<uint8_t> flag_;
};

class Literal : public Expression {
 public:
  Literal() : Expression(Ast::LITERAL) {}

  explicit Literal(Token::Type type)
      : Expression(Ast::LITERAL), literal_type_(type) {}

  Literal(Token::Type type, Utf16String value)
      : Expression(Ast::LITERAL), literal_type_(type), value_(value) {}

  bool Is(Token::Type t) { return literal_type_ == t; }

  Token::Type literal_type() const { return literal_type_; }

  void set_literal_type(Token::Type t) { literal_type_ = t; }

  Utf16String value() const { return value_; }

  void set_value(Utf16String n) { value_ = n; }

  void ToStringSelf(const Ast* ast, std::string* indent,
                    std::stringstream* ss) const {
    (*ss) << *indent << '[' << ast->GetName()
          << " type = " << Token::ToString(literal_type_)
          << " value = " << value().ToUtf8String() << ' '
          << source_position().ToString() << "]\n";
  }
  void ToStringTree(std::string* indent, std::stringstream* ss) const {
    ToStringSelf(this, indent, ss);
  }

 private:
  Token::Type literal_type_;
  Utf16String value_;
};

class Statement : public Ast {
 protected:
  explicit Statement(Ast::Type type) : Ast(type) {}
};

class Statements : public Statement, public AstListTraits<Statement> {
 public:
  Statements() : Statement(Ast::STATEMENTS), AstListTraits<Statement>() {}

  void ToStringSelf(const Ast* ast, std::string* indent,
                    std::stringstream* ss) const {
    (*ss) << *indent << '[' << ast->GetName() << ' '
          << source_position().ToString() << ']' << '\n';
  }
  void ToStringTree(std::string* indent, std::stringstream* ss) const {
    ToStringSelf(this, indent, ss);
    ToStringList(indent, ss);
  }
};

class ImportSpecifier : public Expression {
 public:
  ImportSpecifier(Maybe<Expression*> name, Maybe<Expression*> as,
                  bool is_namespace)
      : Expression(Ast::IMPORT_SPECIFIER),
        is_namespace_(is_namespace),
        name_(name),
        as_(as) {}

  LUX_CONST_GETTER(Maybe<Expression*>, name, name_)
  LUX_CONST_GETTER(Maybe<Expression*>, as, as_)
  LUX_CONST_GETTER(bool, is_namespace, is_namespace_)

  void ToStringSelf(const Ast* ast, std::string* indent,
                    std::stringstream* ss) const {
    (*ss) << *indent << '[' << ast->GetName() << ' '
          << source_position().ToString() << ']' << '\n';
  }
  void ToStringTree(std::string* indent, std::stringstream* ss) const {
    ToStringSelf(this, indent, ss);
    auto n = *indent + "  ";
    name_ >>= [&](auto name) { name->ToStringTree(&n, ss); };
    as_ >>= [&](auto as) { as->ToStringTree(&n, ss); };
  }

 private:
  bool is_namespace_;
  Maybe<Expression*> name_;
  Maybe<Expression*> as_;
};

class NamedImportList : public Expression,
                        public AstListTraits<ImportSpecifier> {
 public:
  NamedImportList()
      : Expression(Ast::NAMED_IMPORT_LIST), AstListTraits<ImportSpecifier>() {}

  void ToStringSelf(const Ast* ast, std::string* indent,
                    std::stringstream* ss) const {
    (*ss) << *indent << '[' << ast->GetName() << ' '
          << source_position().ToString() << ']' << '\n';
  }

  void ToStringTree(std::string* indent, std::stringstream* ss) const {
    ToStringSelf(this, indent, ss);
    ToStringList(indent, ss);
  }
};

class ImportBinding : public Expression {
 public:
  explicit ImportBinding(Maybe<Expression*> default_binding)
      : Expression(Ast::IMPORT_BINDING), default_binding_(default_binding) {}

  LUX_CONST_GETTER(Maybe<Expression*>, default_binding, default_binding_)

  LUX_CONST_GETTER(Maybe<Expression*>, namespace_import, namespace_import_)
  LUX_CONST_GETTER(Maybe<Expression*>, named_import_list, named_import_list_)
  void set_namespace_import(Maybe<Expression*> exp) { namespace_import_ = exp; }

  void set_named_import_list(Maybe<Expression*> exp) {
    named_import_list_ = exp;
  }

  void ToStringSelf(const Ast* ast, std::string* indent,
                    std::stringstream* ss) const {
    (*ss) << *indent << '[' << ast->GetName() << ' '
          << source_position().ToString() << ']' << '\n';
  }

  void ToStringTree(std::string* indent, std::stringstream* ss) const {
    ToStringSelf(this, indent, ss);
    default_binding_ >>= [&](auto db) {
      auto ni = *indent + "  ";
      db->ToStringTree(&ni, ss);
    };
    namespace_import_ >>= [&](auto i) {
      auto ni = *indent + "  ";
      i->ToStringTree(&ni, ss);
    };
    named_import_list_ >>= [&](auto list) {
      auto ni = *indent + "  ";
      list->ToStringTree(&ni, ss);
    };
  }

 private:
  Maybe<Expression*> default_binding_;
  Maybe<Expression*> namespace_import_;
  Maybe<Expression*> named_import_list_;
};

class ImportDeclaration : public Statement {
 public:
  ImportDeclaration(Maybe<Expression*> import_binding,
                    Expression* module_specifier)
      : Statement(Ast::IMPORT_DECLARATION),
        import_binding_(import_binding),
        module_specifier_(module_specifier) {}

  LUX_CONST_GETTER(Maybe<Expression*>, import_binding, import_binding_)
  LUX_CONST_GETTER(Expression*, module_specifier, module_specifier_)

  void ToStringSelf(const Ast* ast, std::string* indent,
                    std::stringstream* ss) const {
    (*ss) << *indent << '[' << ast->GetName() << ' '
          << source_position().ToString() << ']' << '\n';
  }
  void ToStringTree(std::string* indent, std::stringstream* ss) const {
    ToStringSelf(this, indent, ss);
    auto n = *indent + "  ";
    import_binding_ >>= [&](auto ib) { ib->ToStringTree(&n, ss); };
    module_specifier_->ToStringTree(&n, ss);
  }

 private:
  Maybe<Expression*> import_binding_;
  Expression* module_specifier_;
};

class ExportDeclaration : public Statement {
 public:
  enum { NAMESPACE_EXPORT, DEFAULT_EXPORT };

  ExportDeclaration() : Statement(Ast::EXPORT_DECLARATION) {}

  LUX_CONST_GETTER(bool, namespace_export, flags_.get(NAMESPACE_EXPORT))
  LUX_CONST_GETTER(bool, default_export, flags_.get(DEFAULT_EXPORT))
  void set_namespace_export() { flags_.set(NAMESPACE_EXPORT); }
  void set_default_export() { flags_.set(DEFAULT_EXPORT); }

  LUX_CONST_PROPERTY(Maybe<Ast*>, export_clause, export_clause_)
  LUX_CONST_PROPERTY(Maybe<Ast*>, from_clause, from_clause_)

  void ToStringSelf(const Ast* ast, std::string* indent,
                    std::stringstream* ss) const {
    (*ss) << *indent << '[' << ast->GetName()
          << " type = " << (namespace_export() ? "namespace" : "default") << ' '
          << source_position().ToString() << ']' << '\n';
  }
  void ToStringTree(std::string* indent, std::stringstream* ss) const {
    ToStringSelf(this, indent, ss);
    auto n = *indent + "  ";
    export_clause_ >>= [&](auto ast) { ast->ToStringTree(&n, ss); };
    from_clause_ >>= [&](auto ast) { ast->ToStringTree(&n, ss); };
  }

 private:
  Bitset<uint8_t> flags_;
  Maybe<Ast*> export_clause_;
  Maybe<Ast*> from_clause_;
};

class ExpressionStatement : public Statement {
 public:
  explicit ExpressionStatement(Expression* expr)
      : Statement(Ast::EXPRESSION_STATEMENT), expr_(expr) {}

  ExpressionStatement()
      : Statement(Ast::EXPRESSION_STATEMENT), expr_(nullptr) {}

  LUX_CONST_PROPERTY(Expression*, expr, expr_)

  void ToStringSelf(const Ast* ast, std::string* indent,
                    std::stringstream* ss) const {
    (*ss) << *indent << '[' << ast->GetName() << ' '
          << source_position().ToString() << ']' << '\n';
  }
  void ToStringTree(std::string* indent, std::stringstream* ss) const {
    ToStringSelf(this, indent, ss);
    auto n = *indent + "  ";
    expr_->ToStringTree(&n, ss);
  }

 private:
  Expression* expr_;
};

class Parser {
 public:
  enum ParseType { SCRIPT, MODULE };

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
#define ALLOWANCE_TYPE_LIST(A)             \
  A(TEMPLATE, template, 0x1)               \
  A(CALL, call, 0x2)                       \
  A(BINDING_PATTERN, binding_pattern, 0x4) \
  A(INITIALIZER, initializer, 0x8)
    enum Type {
#define DEF_ALLOWANCE(V, _, v) V = v,
      ALLOWANCE_TYPE_LIST(DEF_ALLOWANCE)
#undef DEF_ALLOWANCE
    };

    explicit Allowance(uint16_t type) { flag_.assign(type); }

    Allowance() {}

#define ALLOWANCE_PROPERTIES(V, name, _)         \
  LUX_INLINE void set_##name() { flag_.set(V); } \
  LUX_INLINE bool is_##name##_allowed() const { return flag_.get(V); }
    ALLOWANCE_TYPE_LIST(ALLOWANCE_PROPERTIES)
#undef ALLOWANCE_PROPERTIES

    LUX_INLINE void assign(uint16_t flag) { flag_.assign(flag); }

   private:
    Bitset<uint16_t> flag_;
  };

  Parser(Utf16String* sources, ErrorReporter* reporter);

 private:
  class ParserState {
   public:
    void PushState(State state) { state_stack_.push_back(state); }

    void PopState(State state) {
      INVALIDATE(state_stack_.back() == state);
      state_stack_.pop_back();
    }

    State CurrentState(State state) const { return state_stack_.back(); }

    bool Is(State state) const {
      return state_stack_.size() > 0 ? state == state_stack_.back() : false;
    }

    bool IsInState(std::initializer_list<State> states) const {
      for (auto& s : state_stack_) {
        for (auto& ms : states) {
          if (s == ms) {
            return true;
          }
        }
      }
      return false;
    }

    bool is_strict_mode() const { return flags_.get(0); }

    void set_strict_mode() { flags_.set(0); }

   private:
    std::vector<State> state_stack_;
    Bitset<uint8_t> flags_;
  };

  struct TokenizerRecord {
    Utf16String::iterator cursor;
    Token::Type lookahead;
    SourcePosition position;
  };

 public:
  class TokenizerState {
   public:
    enum Type { IMPLICIT_OCTAL };

    bool Is(Type type) const { return flag_.get(type); }

    void Set(Type type) { flag_.set(type); }

    void Unset(Type type) { flag_.unset(type); }

   private:
    Bitset<uint8_t> flag_;
  };
  class Tokenizer {
   public:
    enum Flag { HAS_LINE_BREAK_BEFORE, HAS_LINE_BREAK_AFTER };
    explicit Tokenizer(Utf16String* sources, ParserState* parser_state,
                       ErrorReporter* reporter)
        : current_position_(nullptr),
          skipped_(0),
          token_(Token::INVALID),
          lookahead_(Token::INVALID),
          it_(sources->begin()),
          end_(sources->end()),
          sources_(sources),
          parser_state_(parser_state),
          reporter_(reporter) {
      USE(sources_);
      Next();
    }

    Token::Type Next();

    Token::Type Peek();

    Token::Type Current() const { return token_; }

    const SourcePosition& position() const { return position_; }

    inline bool HasMore() const {
      return it_ != end_ && !reporter_->HasPendingError();
    }

    inline Utf16String Value() const {
      return Utf16String::FromVectorNonCopy(&buffer_);
    }

    inline Utf16String PeekValue() const {
      return Utf16String::FromVectorNonCopy(&lookahead_buffer_);
    }

    TokenizerRecord Record();

    void Restore(const TokenizerRecord& record);

    bool has_linebreak_after() const;
    void set_linebreak_after();
    void unset_linebreak_after();

    bool has_linebreak_before() const;
    void set_linebreak_before();
    void unset_linebreak_before();

    const TokenizerState& state() { return state_; }

   private:
    Token::Type Tokenize();

    Token::Type TokenizeStringLiteral();

    Token::Type TokenizeIdentifier();

    Token::Type TokenizeNumericLiteral(bool period_seen);

    Token::Type TokenizeTemplateCharacters();

    Token::Type TokenizeRegExp();

    Token::Type GetIdentifierType();

    void CollectLineBreak();
    bool SkipLineBreak();
    bool SkipWhiteSpace();

    void SkipSingleLineComment();
    void SkipMultiLineComment();

    void Prologue();
    void Epilogue();

    Utf16CodePoint Advance();

    void AdvanceAndPushBuffer();
    Utf16CodePoint DecodeEscapeSequence(bool* ok);
    Utf16CodePoint DecodeHexEscape(bool* ok, int len = 4);
    Utf16CodePoint DecodeAsciiEscapeSequence(bool* ok);

    SourcePosition position_;
    SourcePosition lookahead_position_;
    SourcePosition* current_position_;
    uint32_t skipped_;
    Token::Type token_;
    Token::Type lookahead_;
    std::vector<Utf16CodePoint> buffer_;
    std::vector<Utf16CodePoint> lookahead_buffer_;
    std::vector<Utf16CodePoint>* current_buffer_;
    Utf16String::iterator it_;
    Utf16String::iterator end_;
    Utf16String* sources_;
    TokenizerState state_;
    TokenizerState lookahead_state_;
    TokenizerState* current_state_;
    ParserState* parser_state_;
    ErrorReporter* reporter_;
    Bitset<uint8_t> flag_;
  };

  Maybe<Ast*> Parse(ParseType parse_type);

 private:
  void ParseDirectivePrologue();

  LUX_INLINE Token::Type advance() { return tokenizer_->Next(); }

  LUX_INLINE Token::Type peek() { return tokenizer_->Peek(); }

  LUX_INLINE Token::Type cur() const { return tokenizer_->Current(); }

  LUX_INLINE Utf16String value() const { return tokenizer_->Value(); }

  LUX_INLINE Utf16String peek_value() const { return tokenizer_->PeekValue(); }

  LUX_INLINE const SourcePosition& position() const {
    return tokenizer_->position();
  }

  inline void PushState(State s) { parser_state_->PushState(s); }

  inline void PopState(State s) { parser_state_->PopState(s); }

  inline bool MatchState(State s) { return parser_state_->Is(s); }

  inline bool IsInState(std::initializer_list<State> s) {
    return parser_state_->IsInState(s);
  }

  inline bool has_more() const {
    return cur() != Token::END && !reporter_->HasPendingError();
  }

  const TokenizerState& tokenizer_state() { return tokenizer_->state(); }

  void Record() { record_ = tokenizer_->Record(); }

  void Restore() { tokenizer_->Restore(record_); }

  bool MatchStates(std::initializer_list<State> s);

  VISIBLE_FOR_TESTING : template <typename T>
                        Maybe<T*>
                        ParseTerminator(T* node);
  Maybe<Expression*> ParseIdentifierReference();
  Maybe<Expression*> ParseIdentifier();
  Maybe<Expression*> ParseAsyncArrowBindingIdentifier();
  Maybe<Expression*> ParseLabelIdentifier();
  Maybe<Expression*> ParsePrimaryExpression();
  Maybe<Expression*> ParseLiteral();
  Maybe<Expression*> ParseCoverParenthesizedExpressionAndArrowParameterList();
  Maybe<Expression*> ParseParenthesizedExpression();
  Maybe<Expression*> ParseRegularExpression();
  Maybe<Expression*> ParseArrayLiteral(
      Parser::Allowance a = Parser::Allowance());
  Maybe<Expression*> ParseElementList();
  Maybe<Expression*> ParseSpreadElement();
  Maybe<Expression*> ParseObjectLiteralProperty(Parser::Allowance);
  Maybe<Expression*> ParseObjectLiteral(
      Parser::Allowance a = Parser::Allowance());
  Maybe<Expression*> ParsePropertyDefinitionList();
  Maybe<Expression*> ParsePropertyDefinition();
  Maybe<Expression*> ParsePropertyName();
  Maybe<Expression*> ParseCoverInitializedName();
  Maybe<Expression*> ParseInitializer();
  Maybe<Expression*> ParseTemplateLiteral();
  Maybe<Expression*> ParseTemplateSpans();
  Maybe<Expression*> ParseTemplateMiddleList();
  Maybe<Expression*> ParseMemberExpression();
  Maybe<Expression*> ParsePostMemberExpression(Expression* pre);
  Maybe<Expression*> ParsePropertyAccessPostExpression(
      Expression*, Receiver::Type, Allowance, bool error_if_default = false);
  Maybe<Expression*> ParseSuperProperty();
  Maybe<Expression*> ParseNewTarget();
  Maybe<Expression*> ParseNewExpression();
  Maybe<Expression*> ParseCallExpression();
  Maybe<Expression*> ParseCoverCallExpressionAndAsyncArrowHead();
  Maybe<Expression*> ParseCallMemberExpression();
  Maybe<Expression*> ParseSuperCall();
  Maybe<Expression*> ParseArguments();
  Maybe<Expression*> ParseArgumentList();
  Maybe<Expression*> ParseLeftHandSideExpression();
  Maybe<Expression*> ParseUpdateExpression();
  Maybe<Expression*> ParseUnaryExpression();
  Maybe<Expression*> ParseExponentiationExpression();
  Maybe<Expression*> ParseMultiplicativeExpression();
  Maybe<Expression*> ParseMultiplicativeOperator();
  Maybe<Expression*> ParseAdditiveExpression();
  Maybe<Expression*> ParseShiftExpression();
  Maybe<Expression*> ParseRelationalExpression();
  Maybe<Expression*> ParseEqualityExpression();
  Maybe<Expression*> ParseBitwiseANDExpression();
  Maybe<Expression*> ParseBitwiseXORExpression();
  Maybe<Expression*> ParseBitwiseORExpression();
  Maybe<Expression*> ParseLogicalANDExpression();
  Maybe<Expression*> ParseLogicalORExpression();
  Maybe<Expression*> ParseConditionalExpression();
  Maybe<Expression*> ParseAssignmentExpression();
  Maybe<Expression*> ParseAssignmentExpressionLhs();
  Maybe<Expression*> ParseAssignmentPattern();
  Maybe<Expression*> ParseObjectAssignmentPattern();
  Maybe<Expression*> ParseArrayAssignmentPattern();
  Maybe<Expression*> ParseAssignmentPropertyList();
  Maybe<Expression*> ParseAssignmentElementList();
  Maybe<Expression*> ParseAssignmentElisionElement();
  Maybe<Expression*> ParseAssignmentProperty();
  Maybe<Expression*> ParseAssignmentElement();
  Maybe<Expression*> ParseAssignmentRestElement();
  Maybe<Expression*> ParseDestructuringAssignmentTarget();
  Maybe<Expression*> ParseExpression();
  Maybe<Statement*> ParseStatement();
  Maybe<Statement*> ParseDeclaration();
  Maybe<Statement*> ParseHoistableDeclaration();
  Maybe<Statement*> ParseBreakableStatement();
  Maybe<Statement*> ParseBlockStatement();
  Maybe<Statement*> ParseBlock();
  Maybe<Statement*> ParseStatementList(bool (*condition)(Token::Type));
  Maybe<Statement*> ParseStatementListItem();
  Maybe<Statement*> ParseLexicalDeclaration();
  Maybe<Statement*> ParseLexicalBinding();
  Maybe<Statement*> ParseVariableStatement();
  Maybe<Statement*> ParseVariableDeclarationList();
  Maybe<Statement*> ParseVariableDeclaration();
  Maybe<Expression*> ParseBindingPattern();
  Maybe<Expression*> ParseBindingElement();
  Maybe<Expression*> ParseSingleNameBinding(
      Parser::Allowance a = Parser::Allowance());
  Maybe<Expression*> ParseBindingRestElement();
  Maybe<Statement*> ParseExpressionStatement();
  Maybe<Statement*> ParseIfStatement();
  Maybe<Statement*> ParseIterationStatement();
  Maybe<Statement*> ParseForDeclaration();
  Maybe<Statement*> ParseForBinding();
  Maybe<Statement*> ParseContinueStatement();
  Maybe<Statement*> ParseBreakStatement();
  Maybe<Statement*> ParseReturnStatement();
  Maybe<Statement*> ParseWithStatement();
  Maybe<Statement*> ParseSwitchStatement();
  Maybe<Statement*> ParseCaseBlock();
  Maybe<Statement*> ParseCaseClauses();
  Maybe<Statement*> ParseCaseClause();
  Maybe<Statement*> ParseDefaultClause();
  Maybe<Statement*> ParseLabelledStatement();
  Maybe<Statement*> ParseLabelledItem();
  Maybe<Statement*> ParseThrowStatement();
  Maybe<Statement*> ParseTryStatement();
  Maybe<Statement*> ParseCatch();
  Maybe<Statement*> ParseFinally();
  Maybe<Statement*> ParseCatchParameter();
  Maybe<Statement*> ParseDebuggerStatement();
  Maybe<Statement*> ParseFunctionDeclaration(bool async,
                                             bool in_default = false);
  Maybe<Expression*> ParseFunctionExpression();
  Maybe<Expression*> ParseFormalParameters();
  Maybe<Expression*> ParseFormalParameterList();
  Maybe<Expression*> ParseFunctionRestParameter();
  Maybe<Statement*> ParseFunctionBody();
  Maybe<Expression*> ParseArrowFunction();
  Maybe<Expression*> ParseArrowParameters();
  Maybe<Ast*> ParseConciseBody();
  Maybe<Expression*> ParseArrowFormalParameters();
  Maybe<Expression*> ParseAsyncArrowFunction();
  Maybe<Ast*> ParseAsyncConciseBody();
  Maybe<Ast*> ParseAsyncArrowHead();
  Maybe<Expression*> ParseMethodDefinition();
  Maybe<Expression*> ParsePropertySetParameterList();
  Maybe<Expression*> ParseGeneratorMethod();
  Maybe<Ast*> ParseGeneratorDeclaration();
  Maybe<Expression*> ParseGeneratorExpression();
  Maybe<Ast*> ParseGeneratorBody();
  Maybe<Expression*> ParseYieldExpression();
  Maybe<Expression*> ParseAsyncMethod();
  Maybe<Ast*> ParseAsyncFunctionDeclaration();
  Maybe<Expression*> ParseAsyncFunctionExpression();
  Maybe<Ast*> ParseAsyncFunctionBody();
  Maybe<Expression*> ParseAwaitExpression();
  Maybe<Ast*> ParseClassDeclaration();
  Maybe<Expression*> ParseClassExpression();
  Maybe<Ast*> ParseClassTail();
  Maybe<Ast*> ParseClassHeritage();
  Maybe<Ast*> ParseClassBody();
  Maybe<Ast*> ParseClassElementList();
  Maybe<Ast*> ParseClassElement();
  Maybe<Ast*> ParseScript();
  Maybe<Statement*> ParseModule();
  Maybe<Statement*> ParseModuleBody();
  Maybe<Statement*> ParseModuleItem();
  Maybe<Statement*> ParseImportDeclaration();
  Maybe<Expression*> ParseNameSpaceImport();
  Maybe<Expression*> ParseNamedImports();
  Maybe<Statement*> ParseExportDeclaration();
  Maybe<Expression*> ParseExportClause();
  Maybe<Expression*> ParseNamedList();

 private:
  template <typename T, typename... Args>
  T* NewNode(Args... args) {
    auto ast = new (zone()) T(args...);
    ast->set_source_position(position());
    return ast;
  }

  template <typename T, typename... Args>
  T* NewNodeWithPosition(SourcePosition start, Args... args) {
    auto n = NewNode<T>(args...);
    n->set_start_positions(start);
    return n;
  }

  template <typename T, typename E>
  T* NewNode(std::initializer_list<E> args) {
    auto ast = new (zone()) T(args);
    ast->set_source_position(position());
    return ast;
  }

  template <typename T, typename... Args>
  Expression* NewExpression(Args... args) {
    auto expr = new (zone()) T(args...);
    expr->set_source_position(position());
    return expr;
  }

  template <typename T, typename... Args>
  Expression* NewExpressionWithPosition(const SourcePosition& start,
                                        Args... args) {
    auto expr = NewExpression<T>(args...);
    expr->set_start_positions(start);
    return expr;
  }

  template <typename T, typename... Args>
  Statement* NewStatement(Args... args) {
    auto stmt = new (zone()) T(args...);
    stmt->set_source_position(position());
    return stmt;
  }

  template <typename T, typename... Args>
  Statement* NewStatementWithPosition(SourcePosition start, Args... args) {
    auto stmt = NewStatement<T>(args...);
    stmt->set_start_positions(start);
    return stmt;
  }

  LUX_INLINE bool IsAssignmentOperator(Token::Type token) {
    return Token::OneOf(
        token,
        {Token::OP_MUL_ASSIGN, Token::OP_DIV_ASSIGN, Token::OP_PLUS_ASSIGN,
         Token::OP_MINUS_ASSIGN, Token::OP_SHIFT_LEFT_ASSIGN,
         Token::OP_SHIFT_RIGHT_ASSIGN, Token::OP_U_SHIFT_RIGHT_ASSIGN,
         Token::OP_AND_ASSIGN, Token::OP_OR_ASSIGN, Token::OP_XOR_ASSIGN,
         Token::OP_POW_ASSIGN});
  }

  bool has_linebreak_before() const {
    return tokenizer_->has_linebreak_before();
  }

  bool has_linebreak_after() const { return tokenizer_->has_linebreak_after(); }

  ZoneAllocator* zone() { return &zone_allocator_; }

#if defined(DEBUG)
  std::string ToStringCurrentToken() {
    switch (cur()) {
#define CASE(T, v) \
  case Token::T:   \
    return #T;
      SYMBOL_TOKEN_LIST(CASE)
      KEYWORD_TOKEN_LIST(CASE, GROUP_DUMMY_)
#undef CASE
#define LITERAL_CASE(T, v)                                    \
  case Token::T: {                                            \
    std::stringstream st;                                     \
    st << #T << "[value = " << value().ToUtf8String() << "]"; \
    return st.str();                                          \
  }
      LITERAL_TOKEN_LIST(LITERAL_CASE)
#undef LITERAL_CASE
      case Token::END:
        return "End";
      default:
        return std::string("Invalid");
    }
  }

  template <bool Print>
  class DebugStream {
   public:
    template <typename T>
    DebugStream& operator<<(T value) {
      buffer_ << value;
      if (Print) {
        std::cout << value;
      }
      return *this;
    }

    void PrintStackTrace() { printf("%s\n", buffer_.str().c_str()); }

   private:
    std::stringstream buffer_;
  };

  DebugStream<false> phase_buffer_;
  std::string indent_;
#endif

  Maybe<Ast*> result_;
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
