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

#ifndef SRC_REGEXP_H_
#define SRC_REGEXP_H_

#include <string>
#include <sstream>
#include <unordered_set>
#include <utility>
#include <vector>
#include "./bytecode.h"
#include "./heap.h"
#include "./source_position.h"
#include "./unicode.h"
#include "./utils.h"
#include "./zone.h"
#include "./reporter.h"

namespace lux {
class Isolate;
class JSString;
namespace regexp {
#define REGEXP_AST_TYPES(A)                     \
  A(GROUP, Group)                               \
  A(BACK_REFERENCE, BackReference)              \
  A(CHAR_CLASS, CharClass)                      \
  A(ALTERNATE, Alternate)                       \
  A(REPEAT, Repeat)                             \
  A(REPEAT_RANGE, RepeatRange)                  \
  A(CHAR, Char)                                 \
  A(CONJUNCTION, Conjunction)                   \
  A(ROOT, Root)

#define RE_AST_FORWARD_DECL(_, Name) class Name;
REGEXP_AST_TYPES(RE_AST_FORWARD_DECL)
#undef RE_AST_FORWARD_DECL

#define REGEXP_VISIT_INTERNAL_IFACE                   \
  void VisitInternal(Visitor* v)
#define REGEXP_VISIT_INTERNAL_METHOD_DECL(Name)           \
  void VisitInternal(Visitor* v) {                        \
    v->Visit##Name(this);                                 \
  }

class Visitor {
 public:
  explicit Visitor(BytecodeBuilder* bytecode_builder,
                   ZoneAllocator* zone_allocator);

  LUX_GETTER(BytecodeBuilder*, builder, bytecode_builder_)
  LUX_GETTER(ZoneAllocator*, zone, zone_allocator_)

#define REGEXP_VISIT_DECL(G, Name)                    \
  void Visit##Name(Name* ast);
  REGEXP_AST_TYPES(REGEXP_VISIT_DECL)
#undef REGEXP_VISIT_DECL

 private:
  void EmitCompare(u16 code);
  void CollectMatchedChar(Var* char_register);
  void Return(Var* ret = nullptr);

  BytecodeLabel matched_;
  BytecodeLabel failed_;
  BytecodeBuilder* bytecode_builder_;
  ZoneAllocator* zone_allocator_;
};

class Ast: public Zone {
 public:
  enum Type {
#define REGEXP_AST_DEF(A, _) A,
    REGEXP_AST_TYPES(REGEXP_AST_DEF)
#undef REGEXP_AST_DEF
  };

  explicit Ast(Type type):
      type_(type) {}

  LUX_CONST_GETTER(Type, type, type_)

#define REGEXP_AST_CAST(NAME, Name)                   \
  bool Is##Name() const {                             \
    return type_ == NAME;                             \
  }
    REGEXP_AST_TYPES(REGEXP_AST_CAST)
#undef REGEXP_AST_CAST

#ifdef DEBUG
  static const char* kNodeTypeStringList[];
  virtual std::string ToString(std::string* indent = nullptr) const = 0;
#endif

  LUX_INLINE void Visit(Visitor* visitor) {
    VisitInternal(visitor);
  }

 private:
  virtual REGEXP_VISIT_INTERNAL_IFACE {}

  Type type_;
};

struct Config {
  enum Type {
    FROM_START,
    TO_END
  };
};

class AstListTrait {
 public:
  using AstList = std::vector<Ast*>;
  using iterator = AstList::iterator;
  Ast* at(int index) const {
    return list_[index];
  }

  void Push(Ast* a) {
    list_.push_back(a);
  }

  size_t size() const {
    return list_.size();
  }

  iterator begin() {
    return list_.begin();
  }

  iterator end() {
    return list_.end();
  }

 protected:
#ifdef DEBUG
  virtual std::string ToStringList(const char* name,
                                   std::string* indent = nullptr) const {
    std::stringstream st;
    st << (indent == nullptr? "": *indent) << "[" << name << "]\n";
    if (indent != nullptr) {
      (*indent) += "  ";
    }
    for (auto &l : list_) {
      st << l->ToString(indent);
    }
    if (indent != nullptr) {
      (*indent) = (*indent).substr(0, indent->size() - 2);
    }
    return st.str();
  }
#endif

  std::vector<Ast*> list_;
};

class Root: public Ast {
 public:
  Root()
      : Ast(Ast::ROOT), regexp_(nullptr) {}

  void set_config(Config::Type type) {
    flag_.set(type);
  }

  bool is_from_start() const {
    return flag_.get(Config::FROM_START);
  }

  bool is_to_end() const {
    return flag_.get(Config::TO_END);
  }

#ifdef DEBUG
  std::string ToString(std::string* indent = nullptr) const {
    std::stringstream st;
    std::string i = "  ";
    st << "[Root]\n";
    st << regexp_->ToString(&i);
    return st.str();
  }
#endif

  LUX_CONST_PROPERTY(Ast*, regexp, regexp_)

  REGEXP_VISIT_INTERNAL_METHOD_DECL(Root);

 private:
  Bitset<uint8_t> flag_;
  Ast* regexp_;
};

class Conjunction: public Ast, public AstListTrait {
 public:
  Conjunction()
      : Ast(Ast::CONJUNCTION),
        AstListTrait() {}

  std::string ToString(std::string* indent = nullptr) const {
    return ToStringList("Conjunction", indent);
  }

  REGEXP_VISIT_INTERNAL_METHOD_DECL(Conjunction);
};

class Group: public Ast {
 public:
#define REGEXP_GROUP_TYPE_LIST(A)               \
  A(CAPTURE)                                    \
  A(UNCAPTURE)                                  \
  A(POSITIVE_LOOKAHEAD)                         \
  A(NEGATIVE_LOOKAHEAD)

  enum Type {
#define REGEXP_GROUP_TYPE_DECL(N) N,
  REGEXP_GROUP_TYPE_LIST(REGEXP_GROUP_TYPE_DECL)
#undef REGEXP_GROUP_TYPE_DECL
  };
  Group()
    : Ast(Ast::GROUP) {}
  explicit Group(Type type, Ast* node)
      : Ast(Ast::GROUP),
        type_(type),
        node_(node) {}

  LUX_CONST_PROPERTY(Ast*, node, node_)
  LUX_CONST_PROPERTY(Type, type, type_)

  REGEXP_VISIT_INTERNAL_METHOD_DECL(Group);

#ifdef DEBUG
  std::string ToString(std::string* indent = nullptr) const {
    static std::array<const char*, 4> kGroupTypeNameMap = {{
#define REGEXP_GROUP_VALUE_MAP_DECL(A) #A,
        REGEXP_GROUP_TYPE_LIST(REGEXP_GROUP_VALUE_MAP_DECL)
#undef REGEXP_GROUP_VALUE_MAP_DECL
          }};

    std::stringstream st;
    st << (indent == nullptr? "": *indent)
       << "[Group type = " << kGroupTypeNameMap[type_] << "]\n";
    if (indent != nullptr) {
      (*indent) += "  ";
    }
    if (node_ != nullptr) {
      st << node_->ToString(indent);
    }
    if (indent != nullptr) {
      (*indent) = (*indent).substr(0, indent->size() - 2);
    }
    return st.str();
  }
#endif

 private:
  Type type_;
  Ast* node_;
};

class BackReference: public Ast {
 public:
  BackReference()
    : Ast(Ast::BACK_REFERENCE) {}
};

class CharClass: public Ast {
 public:
  explicit CharClass(bool exclude, JSString* value)
      : Ast(Ast::CHAR_CLASS),
        exclude_(exclude),
        value_(value) {}

  REGEXP_VISIT_INTERNAL_METHOD_DECL(CharClass);

  LUX_CONST_GETTER(JSString*, value, value_)

#ifdef DEBUG
  std::string ToString(std::string* indent = nullptr) const {
    std::stringstream st;
    JSString::Utf8String u8str(value_);
    st << (indent == nullptr? "": *indent)
       << "[CharClass exclude = " << (exclude_? "true": "false");
    st << ' ' << '<' << u8str.value() << ">]\n";
    if (indent != nullptr) {
      (*indent) = (*indent).substr(0, indent->size() - 2);
    }
    return st.str();
  }
#endif
 private:
  bool exclude_;
  JSString* value_;
};

class Alternate final: public Ast {
 public:
  Alternate(Ast* left, Ast* right)
      : Ast(Ast::ALTERNATE),
        left_(left), right_(right) {}

  LUX_CONST_PROPERTY(Ast*, left, left_)
  LUX_CONST_PROPERTY(Ast*, right, right_)
  REGEXP_VISIT_INTERNAL_METHOD_DECL(Alternate);

#ifdef DEBUG
  std::string ToString(std::string* indent = nullptr) const {
    std::stringstream st;
    st << (indent == nullptr? "": *indent) << "[Alternate]\n";
    if (indent != nullptr) {
      (*indent) += "  ";
    }
    if (left_ != nullptr) {
      st << left_->ToString(indent);
    }
    if (right_ != nullptr) {
      st << right_->ToString(indent);
    }
    if (indent != nullptr) {
      (*indent) = (*indent).substr(0, indent->size() - 2);
    }
    return st.str();
  }
#endif

 private:
  Ast* left_;
  Ast* right_;
};

class Repeat: public Ast {
 public:
  Repeat()
      : Ast(Ast::REPEAT) {}
  explicit Repeat(int32_t more_than, Ast* node)
      : Ast(Ast::REPEAT),
        more_than_(more_than), target_(node) {}

  LUX_CONST_PROPERTY(Ast*, target, target_)
  LUX_CONST_PROPERTY(int32_t, more_than, more_than_)

  REGEXP_VISIT_INTERNAL_METHOD_DECL(Repeat);

#ifdef DEBUG
  std::string ToString(std::string* indent = nullptr) const {
    std::stringstream st;
    st << (indent == nullptr? "": *indent)
       << "[Repeat more than " << more_than_ << "]\n";
    if (target_ != nullptr) {
      if (indent != nullptr) {
        (*indent) += "  ";
      }
      st << target_->ToString(indent);
      if (indent != nullptr) {
        (*indent) = (*indent).substr(0, indent->size() - 2);
      }
    }
    return st.str();
  }
#endif

 private:
  int32_t more_than_;
  Ast* target_;
};

class RepeatRange: public Ast {
 public:
  RepeatRange()
      : Ast(Ast::REPEAT) {}
  explicit RepeatRange(int32_t more_than, int32_t less_than, Ast* node)
      : Ast(Ast::REPEAT),
        more_than_(more_than), less_than_(less_than), target_(node) {}

  LUX_CONST_PROPERTY(int32_t, more_than, more_than_)
  LUX_CONST_PROPERTY(int32_t, less_than, less_than_)
  LUX_CONST_PROPERTY(Ast*, target, target_)

  REGEXP_VISIT_INTERNAL_METHOD_DECL(RepeatRange);

#ifdef DEBUG
  std::string ToString(std::string* indent = nullptr) const {
    std::stringstream st;
    st << (indent == nullptr? "": *indent)
       << "[RepeatRange more than "
       << more_than_ << ", less than " << less_than_ << "]\n";
    if (target_ != nullptr) {
      if (indent != nullptr) {
        (*indent) += "  ";
      }
      st << target_->ToString(indent);
      if (indent != nullptr) {
        (*indent) = (*indent).substr(0, indent->size() - 2);
      }
    }
    return st.str();
  }
#endif

 private:
  int32_t more_than_;
  int32_t less_than_;
  Ast* target_;
};

class Char: public Ast {
 public:
  Char()
      : Ast(Ast::CHAR) {}

  explicit Char(Utf16CodePoint value)
      : Ast(Ast::CHAR),
        value_(value) {}

  LUX_CONST_PROPERTY(Utf16CodePoint, value, value_)

#ifdef DEBUG
  std::string ToString(std::string* indent = nullptr) const {
    std::stringstream st;
    st << (indent == nullptr? "": *indent)
       << "[Char '" << value_.ToUtf8String() << "']\n";
    return st.str();
  }
#endif

 private:
  REGEXP_VISIT_INTERNAL_METHOD_DECL(Char);

  Utf16CodePoint value_;
};

class Parser {
 public:
  Parser(Isolate* isolate,
         ErrorReporter* error_reporter,
         SourcePosition* source_position,
         Handle<JSString> source)
      : isolate_(isolate),
        reporter_(error_reporter),
        source_position_(source_position),
        it_(source->begin()), end_(source->end()),
        source_(source) {}

  void Parse();

  Root* node() {
    return &root_;
  }

 private:
  LUX_GETTER(SourcePosition&, position, *source_position_)

  Ast* ParseRegExp();
  Ast* ParseRoot();

  Ast* ParseChar(bool allow_selection = true);

  Ast* ParseGroup();

  Ast* ParseCharClass();

  Ast* ParseSelection(Ast* a);

  Ast* ParseRangeRepeat(Ast* a);

  Utf16String::ParseIntResult ToInt();

  void update_start_pos() {
    source_position_->set_start_col(source_position_->end_col());
  }

  bool IsSpecialChar(Utf16CodePoint cp) const;

  Utf16CodePoint advance() {
    source_position_->add_end_col();
    return *(it_++);
  }
  Utf16CodePoint peek() {
    return *(it_ + 1);
  }
  Utf16CodePoint cur() {
    return *it_;
  }
  bool has_more() {
    return it_ != end_;
  }

  ZoneAllocator* zone() {
    return &zone_allocator_;
  }

  Root root_;
  Isolate* isolate_;
  ErrorReporter* reporter_;
  SourcePosition* source_position_;
  JSString::iterator it_;
  JSString::iterator end_;
  Handle<JSString> source_;
  ZoneAllocator zone_allocator_;
};

class Compiler {
 public:
  Compiler(Isolate* isolate,
           ErrorReporter* error_reporter,
           SourcePosition* sp)
      : isolate_(isolate),
        error_reporter_(error_reporter),
        sp_(sp) {}

  Handle<JSRegExp> Compile(const char* source);

 private:
  Isolate* isolate_;
  ErrorReporter* error_reporter_;
  SourcePosition* sp_;
};
}  // namespace regexp
}  // namespace lux

#undef REGEXP_VISIT_INTERNAL_IFACE
#undef REGEXP_VISIT_INTERNAL_METHOD_DECL
#endif  // SRC_REGEXP_H_
