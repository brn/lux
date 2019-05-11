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

#include <sstream>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>
#include "./bytecode.h"
#include "./heap.h"
#include "./maybe.h"
#include "./reporter.h"
#include "./source_position.h"
#include "./unicode.h"
#include "./utils.h"
#include "./zone.h"

namespace lux {
class Isolate;
class JSString;
namespace regexp {
#define REGEXP_FLAG_LIST(A) \
  A(Multiline, 0x1)         \
  A(Global, 0x2)            \
  A(IgnoreCase, 0x4)        \
  A(Sticky, 0x8)

struct Flag {
  enum Type : uint8_t {
    kNone,
#define REGEXP_DEF_FLAG(Flag, v) k##Flag = v,
    REGEXP_FLAG_LIST(REGEXP_DEF_FLAG)
#undef REGEXP_DEF_FLAG
  };

#define FLAG_CHECK(Name, _)                   \
  inline static bool Is##Name(uint8_t flag) { \
    return (flag & k##Name) == k##Name;       \
  }
  REGEXP_FLAG_LIST(FLAG_CHECK)
#undef FLAG_CHECK
};

#define REGEXP_AST_TYPES(A)          \
  A(GROUP, Group)                    \
  A(BACK_REFERENCE, BackReference)   \
  A(CHAR_CLASS, CharClass)           \
  A(ALTERNATE, Alternate)            \
  A(REPEAT, Repeat)                  \
  A(REPEAT_RANGE, RepeatRange)       \
  A(CHAR_SEQUENCE, CharSequence)     \
  A(CHAR, Char)                      \
  A(ESCAPE_SEQUENCE, EscapeSequence) \
  A(ANY, Any)                        \
  A(CONJUNCTION, Conjunction)        \
  A(CHAR_RANGE, CharRange)           \
  A(MATCH_ROOT, MatchRoot)           \
  A(ROOT, Root)

#define RE_AST_FORWARD_DECL(_, Name) class Name;
REGEXP_AST_TYPES(RE_AST_FORWARD_DECL)
#undef RE_AST_FORWARD_DECL

struct AstContext {
  BytecodeLabel* success_label;
  BytecodeLabel* failure_label;
};

#define REGEXP_VISIT_INTERNAL_IFACE \
  void VisitInternal(Visitor* v, AstContext* failure_label)
#define REGEXP_VISIT_INTERNAL_METHOD_DECL(Name)               \
  void VisitInternal(Visitor* v, AstContext* failure_label) { \
    v->Visit##Name(this, failure_label);                      \
  }

class Visitor {
 public:
  explicit Visitor(Isolate* isolate, uint16_t captured_count,
                   BytecodeBuilder* bytecode_builder,
                   ZoneAllocator* zone_allocator, uint8_t flag);

  LUX_GETTER(BytecodeBuilder*, builder, bytecode_builder_)
  LUX_GETTER(ZoneAllocator*, zone, zone_allocator_)

#define REGEXP_VISIT_DECL(G, Name) \
  void Visit##Name(Name* ast, AstContext* context);
  REGEXP_AST_TYPES(REGEXP_VISIT_DECL)
#undef REGEXP_VISIT_DECL

 private:
  enum { kDisableRetry = 0 };
  void disable_retry() { flags_.set(kDisableRetry); }
  bool is_retryable() { return !flags_.get(kDisableRetry); }
  void EmitCompare(u16 code);
  void CollectMatchedChar(Var* char_register);
  void Return(Var* ret = nullptr);
  void EmitEveryOrRune(const std::vector<Utf16CodePoint>& buf,
                       AstContext* context);
  void EmitSomeOrRune(const std::vector<Utf16CodePoint>& buf);
  void EmitResultBranch(AstContext*);
  LUX_INLINE bool IsGlobal() {
    return (flag_ & Flag::kGlobal) == Flag::kGlobal;
  }
  LUX_CONST_GETTER(uint16_t, captured_count, captured_count_)

  uint8_t flag_;
  uint16_t captured_count_;
  Isolate* isolate_;
  Bitset<uint8_t> flags_;
  BytecodeLabel matched_;
  BytecodeLabel failed_;
  BytecodeBuilder* bytecode_builder_;
  ZoneAllocator* zone_allocator_;
};

class Ast : public Zone {
 public:
  enum Type {
#define REGEXP_AST_DEF(A, _) A,
    REGEXP_AST_TYPES(REGEXP_AST_DEF)
#undef REGEXP_AST_DEF
  };

  explicit Ast(Type type) : type_(type) {}

  LUX_CONST_GETTER(Type, type, type_)

#define REGEXP_AST_CAST(NAME, Name)                                       \
  bool Is##Name() const { return type_ == NAME; }                         \
  Name* UncheckedCastTo##Name() { return reinterpret_cast<Name*>(this); } \
  const Name* UncheckedCastTo##Name() const {                             \
    return reinterpret_cast<const Name*>(this);                           \
  }
  REGEXP_AST_TYPES(REGEXP_AST_CAST)
#undef REGEXP_AST_CAST

#ifdef DEBUG
  static const char* kNodeTypeStringList[];
  virtual std::string ToString(std::string* indent = nullptr) const = 0;
#endif

  LUX_INLINE void Visit(Visitor* visitor, AstContext* context) {
    VisitInternal(visitor, context);
  }

 private:
  virtual REGEXP_VISIT_INTERNAL_IFACE {}

  Type type_;
};

struct Config {
  enum Type { FROM_START, TO_END };
};

class AstListTrait {
 public:
  using AstList = std::vector<Ast*>;
  using iterator = AstList::iterator;
  Ast* at(int index) const { return list_[index]; }

  void Set(int index, Ast* ast) { list_[index] = ast; }

  void Push(Ast* a) { list_.push_back(a); }

  Ast* Last() const { return list_.back(); }

  size_t size() const { return list_.size(); }

  iterator begin() { return list_.begin(); }

  iterator end() { return list_.end(); }

 protected:
#ifdef DEBUG
  virtual std::string ToStringList(const char* name,
                                   std::string* indent = nullptr) const {
    std::stringstream st;
    st << (indent == nullptr ? "" : *indent) << "[" << name << "]\n";
    if (indent != nullptr) {
      (*indent) += "  ";
    }
    for (auto& l : list_) {
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

class Root : public Ast {
 public:
  Root() : Ast(Ast::ROOT), regexp_(nullptr) {}

  void set_config(Config::Type type) { flag_.set(type); }

  bool is_from_start() const { return flag_.get(Config::FROM_START); }

  bool is_to_end() const { return flag_.get(Config::TO_END); }

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

class Conjunction : public Ast, public AstListTrait {
 public:
  Conjunction() : Ast(Ast::CONJUNCTION), AstListTrait() {}

  std::string ToString(std::string* indent = nullptr) const {
    return ToStringList("Conjunction", indent);
  }

  REGEXP_VISIT_INTERNAL_METHOD_DECL(Conjunction);
};

class Group : public Ast, public AstListTrait {
 public:
#define REGEXP_GROUP_TYPE_LIST(A) \
  A(CAPTURE)                      \
  A(UNCAPTURE)                    \
  A(POSITIVE_LOOKAHEAD)           \
  A(NEGATIVE_LOOKAHEAD)

  enum Type {
#define REGEXP_GROUP_TYPE_DECL(N) N,
    REGEXP_GROUP_TYPE_LIST(REGEXP_GROUP_TYPE_DECL)
#undef REGEXP_GROUP_TYPE_DECL
  };
  Group() : Ast(Ast::GROUP), group_specifier_name_(nullptr) {}
  Group(Type type, uint16_t captured_index, JSString* group_specifier_name)
      : Ast(Ast::GROUP),
        type_(type),
        captured_index_(captured_index),
        group_specifier_name_(group_specifier_name) {}

  LUX_CONST_PROPERTY(Type, type, type_)
  LUX_CONST_GETTER(uint16_t, captured_index, captured_index_)
  LUX_CONST_GETTER(JSString*, group_specifier_name, group_specifier_name_)

  inline bool IsCapturable() const { return type_ != Group::UNCAPTURE; }

  inline bool HasGroupSpecifierName() const {
    return group_specifier_name_ != nullptr;
  }

  inline bool Is(Type type) const { return type_ == type; }

  REGEXP_VISIT_INTERNAL_METHOD_DECL(Group);

#ifdef DEBUG
  std::string ToString(std::string* indent = nullptr) const {
    static std::array<const char*, 4> kGroupTypeNameMap = {{
#define REGEXP_GROUP_VALUE_MAP_DECL(A) #A,
        REGEXP_GROUP_TYPE_LIST(REGEXP_GROUP_VALUE_MAP_DECL)
#undef REGEXP_GROUP_VALUE_MAP_DECL
    }};

    std::stringstream st;
    st << "Group type = " << kGroupTypeNameMap[type_] << "";
    if (HasGroupSpecifierName()) {
      JSString::Utf8String str(group_specifier_name_);
      st << " name = " << str.value();
    }
    auto x = st.str();
    return ToStringList(x.c_str(), indent);
  }
#endif

 private:
  Type type_;
  uint16_t captured_index_;
  JSString* group_specifier_name_;
};

class BackReference : public Ast {
 public:
  explicit BackReference(uint32_t index)
      : Ast(Ast::BACK_REFERENCE), index_(index) {}

  LUX_CONST_PROPERTY(uint32_t, index, index_);

  REGEXP_VISIT_INTERNAL_METHOD_DECL(BackReference);

 private:
  std::string ToString(std::string* indent = nullptr) const {
    std::stringstream st;
    st << (indent == nullptr ? "" : *indent) << "[BackReference index = '"
       << index_ << "']\n";
    return st.str();
  }

  uint32_t index_;
};

class CharClass : public Ast, public AstListTrait {
 public:
  explicit CharClass(bool exclude) : Ast(Ast::CHAR_CLASS), exclude_(exclude) {}

  REGEXP_VISIT_INTERNAL_METHOD_DECL(CharClass);

  LUX_INLINE bool IsExclude() const { return exclude_; }

#ifdef DEBUG
  std::string ToString(std::string* indent = nullptr) const {
    std::stringstream st;
    st << "[CharClass exclude = " << (exclude_ ? "true" : "false") << "]\n";
    return ToStringList(st.str().c_str(), indent);
  }
#endif
 private:
  bool exclude_;
};

class Alternate final : public Ast {
 public:
  Alternate(Ast* left, Ast* right)
      : Ast(Ast::ALTERNATE), left_(left), right_(right) {}

  LUX_CONST_PROPERTY(Ast*, left, left_)
  LUX_CONST_PROPERTY(Ast*, right, right_)
  REGEXP_VISIT_INTERNAL_METHOD_DECL(Alternate);

#ifdef DEBUG
  std::string ToString(std::string* indent = nullptr) const {
    std::stringstream st;
    st << (indent == nullptr ? "" : *indent) << "[Alternate]\n";
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

class Repeat : public Ast {
 public:
  enum Type { GREEDY, SHORTEST };

  Repeat(Type type, int32_t more_than, Ast* node)
      : Ast(Ast::REPEAT), type_(type), more_than_(more_than), target_(node) {}

  LUX_CONST_PROPERTY(Ast*, target, target_)
  LUX_CONST_PROPERTY(int32_t, more_than, more_than_)
  LUX_CONST_PROPERTY(Type, type, type_)

  REGEXP_VISIT_INTERNAL_METHOD_DECL(Repeat);

#ifdef DEBUG
  std::string ToString(std::string* indent = nullptr) const {
    std::stringstream st;
    st << (indent == nullptr ? "" : *indent) << "[Repeat more than "
       << more_than_ << "]\n";
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
  Type type_;
  int32_t more_than_;
  Ast* target_;
};

class RepeatRange : public Ast {
 public:
  RepeatRange() : Ast(Ast::REPEAT) {}
  explicit RepeatRange(int32_t more_than, int32_t less_than, Ast* node)
      : Ast(Ast::REPEAT),
        more_than_(more_than),
        less_than_(less_than),
        target_(node) {}

  LUX_CONST_PROPERTY(int32_t, more_than, more_than_)
  LUX_CONST_PROPERTY(int32_t, less_than, less_than_)
  LUX_CONST_PROPERTY(Ast*, target, target_)

  REGEXP_VISIT_INTERNAL_METHOD_DECL(RepeatRange);

#ifdef DEBUG
  std::string ToString(std::string* indent = nullptr) const {
    std::stringstream st;
    st << (indent == nullptr ? "" : *indent) << "[RepeatRange more than "
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

class CharSequence : public Ast, public AstListTrait {
 public:
  CharSequence() : Ast(Ast::CHAR_SEQUENCE), AstListTrait() {}

  std::string ToString(std::string* indent = nullptr) const {
    return ToStringList("Conjunction", indent);
  }

  REGEXP_VISIT_INTERNAL_METHOD_DECL(CharSequence);
};

class Char : public Ast {
 public:
  Char() : Ast(Ast::CHAR) {}

  explicit Char(Utf16CodePoint value) : Ast(Ast::CHAR), value_(value) {}

  LUX_CONST_PROPERTY(Utf16CodePoint, value, value_)

#ifdef DEBUG
  std::string ToString(std::string* indent = nullptr) const {
    std::stringstream st;
    st << (indent == nullptr ? "" : *indent) << "[Char '"
       << value_.ToUtf8String() << "']\n";
    return st.str();
  }
#endif

 private:
  REGEXP_VISIT_INTERNAL_METHOD_DECL(Char);

  Utf16CodePoint value_;
};

#define REGEXP_ESCAPE_SEQUENCES(A) \
  A(WORDS, 0x001, 'w')             \
  A(NOT_WORDS, 0x002, 'W')         \
  A(NOT_WORDCHAR, 0x004, 'b')      \
  A(DIGIT, 0x008, 'd')             \
  A(NOT_DIGIT, 0x010, 'D')         \
  A(WHITE_SPACE, 0x020, 's')       \
  A(NOT_WHITE_SPACE, 0x040, 'S')

enum class RegexSpecialCharType : uint8_t {
#define DEF_REGEXP_ESCAPE_SEQUENCE_ENUM(NAME, value, _) NAME = value,
  REGEXP_ESCAPE_SEQUENCES(DEF_REGEXP_ESCAPE_SEQUENCE_ENUM)
#undef DEF_REGEXP_ESCAPE_SEQUENCE_ENUM
      __SENTINEL = 0
};

class EscapeSequence : public Ast {
 public:
  explicit EscapeSequence(RegexSpecialCharType type)
      : Ast(Ast::ESCAPE_SEQUENCE), type_(type) {}

  LUX_CONST_PROPERTY(RegexSpecialCharType, type, type_)

#ifdef DEBUG
  std::string ToString(std::string* indent = nullptr) const {
    std::stringstream st;
    st << (indent == nullptr ? "" : *indent) << "[EscapeSequence type = '"
       << GetTypeString() << "']\n";
    return st.str();
  }
  const char* GetTypeString() const {
    switch (type_) {
#define DEF_REGEXP_ESCAPE_SEQUENCE_CASES(NAME, value, _) \
  case RegexSpecialCharType::NAME:                       \
    return #NAME;
      REGEXP_ESCAPE_SEQUENCES(DEF_REGEXP_ESCAPE_SEQUENCE_CASES)
#undef DEF_REGEXP_ESCAPE_SEQUENCE_CASES
      default:
        return "";
    }
  }
#endif

 private:
  REGEXP_VISIT_INTERNAL_METHOD_DECL(EscapeSequence);

  RegexSpecialCharType type_;
};

class CharRange : public Ast {
 public:
  CharRange(u16 start, u16 end)
      : Ast(Ast::CHAR_RANGE), start_(start), end_(end) {}

  LUX_CONST_GETTER(u16, start, start_);
  LUX_CONST_GETTER(u16, end, end_);

#ifdef DEBUG
  std::string ToString(std::string* indent = nullptr) const {
    std::stringstream st;
    st << (indent == nullptr ? "" : *indent) << "[CharRange from = '" << start_
       << " to = " << end_ << "']\n";
    return st.str();
  }
#endif

 private:
  REGEXP_VISIT_INTERNAL_METHOD_DECL(CharRange);

  u16 start_;
  u16 end_;
};

class MatchRoot : public Ast, public AstListTrait {
 public:
  MatchRoot() : Ast(Ast::MATCH_ROOT), AstListTrait() {}

#ifdef DEBUG
  std::string ToString(std::string* indent = nullptr) const {
    return ToStringList("MatchRoot", indent);
  }
#endif

 private:
  REGEXP_VISIT_INTERNAL_METHOD_DECL(MatchRoot);
};

class Any : public Ast {
 public:
  enum Type {
    EAT_ANY,
    EAT_GREEDY,
    EAT_MINIMUM,
    EAT_GREEDY_L1,
    EAT_MINIMUM_L1,
  };

  Any(Type type = EAT_ANY) : Ast(Ast::ANY), type_(type) {}

  LUX_CONST_PROPERTY(Any::Type, type, type_);

  bool Is(Type type) const { return type_ == type; }

#ifdef DEBUG
  std::string ToString(std::string* indent = nullptr) const {
    std::stringstream st;
    st << (indent == nullptr ? "" : *indent) << "[Any]\n";
    return st.str();
  }
#endif

 private:
  REGEXP_VISIT_INTERNAL_METHOD_DECL(Any);

  Type type_;
};

class Parser {
 public:
  Parser(Isolate* isolate, ErrorReporter* error_reporter,
         SourcePosition* source_position, Handle<JSString> source)
      : capture_count_(0),
        isolate_(isolate),
        reporter_(error_reporter),
        source_position_(source_position),
        it_(source->begin()),
        end_(source->end()),
        source_(source) {}

  void Parse();

  Root* node() { return &root_; }

  LUX_CONST_GETTER(uint32_t, capture_count, capture_count_)

 private:
  LUX_GETTER(SourcePosition&, position, *source_position_)

  Ast* ParseRegExp();
  Maybe<Ast*> ParseDisjunction();

  Maybe<Ast*> ParseAtom();

  Maybe<Ast*> ParseChar();

  Maybe<Ast*> ParseGroup();

  Maybe<Ast*> ParseCharClass();

  Maybe<Ast*> ParseRepeat(Ast*);

  Maybe<Ast*> ParseRangeRepeat(Ast* a);

  Maybe<JSString*> ParseGroupSpecifierName();

  Utf16CodePoint DecodeHexEscape(bool* ok, int len = 4);

  Utf16CodePoint DecodeAsciiEscapeSequence(bool* ok);

  Utf16String::ParseIntResult ToInt();

  Maybe<Ast*> ParseSingleWord(bool is_inner_char_class);

  inline bool has_pending_error() { return reporter_->HasPendingError(); }

  inline void SkipWhiteSpace();

  inline void Capture() { capture_count_++; }

  inline bool IsRepeat(Ast* node) {
    return node->IsRepeat() || node->IsRepeatRange();
  }

  template <typename T>
  Maybe<Ast*> SplitCharSequenceIf(Ast*, T);

  void update_start_pos() {
    source_position_->set_start_col(source_position_->end_col());
  }

  bool IsSpecialChar(Utf16CodePoint cp) const;

  Utf16CodePoint advance() {
    source_position_->add_end_col();
    return *(it_++);
  }
  Utf16CodePoint peek() { return *(it_ + 1); }
  Utf16CodePoint cur() { return *it_; }
  bool has_more() { return it_ != end_; }

  ZoneAllocator* zone() { return &zone_allocator_; }

  uint32_t capture_count_;
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
  Compiler(Isolate* isolate, ErrorReporter* error_reporter, SourcePosition* sp)
      : isolate_(isolate), error_reporter_(error_reporter), sp_(sp) {}

  Handle<JSRegExp> Compile(const char* source, uint8_t flag);

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
