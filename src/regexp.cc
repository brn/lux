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

#include "./regexp.h"
#include "./chars.h"
#include "./maybe.h"
#include "./objects/jsobject.h"
#include "./unicode.h"

namespace lux {
namespace regexp {
#define ENTER()  // printf("%s\n", __FUNCTION__);

#define BASE_REPORT_SYNTAX_ERROR_(parser)                              \
  auto e = std::make_shared<lux::ErrorDescriptor>(parser->position()); \
  (reporter_->ReportSyntaxError(e))

#ifdef DEBUG
#define __BASE_DEBUG_REPORT_SYNTAX_ERROR(parser, message)                   \
  JSString::Utf8String str(*source_);                                       \
  BASE_REPORT_SYNTAX_ERROR_(parser)                                         \
      << "Invalid regular expression: /" << str.value() << "/: " << message \
      << " ("                                                               \
      << std::to_string(parser->position().start_line_number() + 1).c_str() \
      << ":" << std::to_string(parser->position().start_col() + 1).c_str()  \
      << "~" << std::to_string(parser->position().end_col() + 1).c_str()    \
      << ')' << "\n[Debug] line:" << __LINE__                               \
      << ", function: " << __FUNCTION__;
#define REPORT_SYNTAX_ERROR(parser, message)        \
  __BASE_DEBUG_REPORT_SYNTAX_ERROR(parser, message) \
  return Nothing<Ast*>();
#define REPORT_SYNTAX_ERROR_NO_RETURN(parser, message) \
  __BASE_DEBUG_REPORT_SYNTAX_ERROR(parser, message)
#define REPORT_SYNTAX_ERROR_WITH_RETURN(parser, message, retVal) \
  __BASE_DEBUG_REPORT_SYNTAX_ERROR(parser, message)              \
  return retVal;
#else
#define REPORT_SYNTAX_ERROR(parser, message) \
  BASE_REPORT_SYNTAX_ERROR_(parser) << message return nullptr;
#define REPORT_SYNTAX_ERROR_WITH_RETURN(parser, message, retVal) \
  BASE_REPORT_SYNTAX_ERROR_(parser) << message return retVal;
#define REPORT_SYNTAX_ERROR_NO_RETURN(parser, message) \
  __BASE_DEBUG_REPORT_SYNTAX_ERROR(parser, message)
#endif

#define EXPECT_NOT_ADVANCE(parser, n, expect)                   \
  if (n != expect) {                                            \
    REPORT_SYNTAX_ERROR(parser, "'" << expect << "' expected"); \
  }

#define EXPECT_NOT_ADVANCE_WITH_RETURN(parser, n, expect, retVal)          \
  if (n != expect) {                                                       \
    REPORT_SYNTAX_ERROR_WITH_RETURN(parser, "'" << expect << "' expected", \
                                    retVal);                               \
  }

#define EXPECT(parser, n, expect)       \
  EXPECT_NOT_ADVANCE(parser, n, expect) \
  advance();

#define EXPECT_WITH_RETURN(parser, n, expect, retVal)       \
  EXPECT_NOT_ADVANCE_WITH_RETURN(parser, n, expect, retVal) \
  advance();

const char* Ast::kNodeTypeStringList[] = {
#define AST_STRING_DEF(V, v) #V,
    REGEXP_AST_TYPES(AST_STRING_DEF)
#undef AST_STRING_DEF
};

#define AST_VISIT(Name) void Visitor::Visit##Name(Name* node)

#define __ builder()->
using Label = BytecodeLabel;

Visitor::Visitor(uint16_t captured_count, BytecodeBuilder* bytecode_builder,
                 ZoneAllocator* zone_allocator)
    : captured_count_(captured_count),
      bytecode_builder_(bytecode_builder),
      zone_allocator_(zone_allocator) {}

AST_VISIT(Root) {
  Var offset;
  __ RegexComment("RegExp");

  if (node->is_from_start()) {
    disable_retry();
    __ RegexDisableRetry();
  }

  __ RegexReserveCapture(captured_count());
  node->regexp()->Visit(this);
  if (node->is_to_end()) {
    __ RegexCheckEnd();
  }
  __ RegexJumpIfFailed(&failed_);

  __ Bind(&matched_);
  __ RegexMatched();

  __ Bind(&failed_);
  __ RegexFailed();
}

AST_VISIT(Conjunction) {
  __ RegexComment("Conjunction");
  for (auto& a : *node) {
    a->Visit(this);
  }
}

AST_VISIT(Group) {
  __ RegexComment("Group");

  if (node->IsCapturable()) {
    __ RegexStartCapture(node->captured_index());
  }

  for (auto& a : *node) {
    a->Visit(this);
  }

  if (node->IsCapturable()) {
    __ RegexUpdateCapture();
  }
}

AST_VISIT(CharClass) {
  __ RegexComment("CharClass");
  EnableSearchIf();

  Label ok, pop;

  __ RegexSome(node->value());
  __ RegexBranch(&ok, &pop);

  __ Bind(&pop);
  { __ RegexPopThread(); }

  __ Bind(&ok);
}

AST_VISIT(Alternate) {
  __ RegexComment("Alternate");

  Label next, exit, pop;

  __ RegexPushThread(&next);
  jump_point_ = Just(&pop);
  node->left()->Visit(this);
  __ RegexBranch(&exit, &pop);

  __ Bind(&pop);
  { __ RegexPopThread(); }

  __ Bind(&next);
  {
    Label pop;
    jump_point_ = Just(&exit);
    node->right()->Visit(this);
    __ RegexJump(&exit);
  }

  __ Bind(&exit);
}

AST_VISIT(Repeat) {
  __ RegexComment("Repeat");
  auto count = node->more_than();

  Label loop, pop, next;

  if (count == 1) {
    __ RegexCheckPosition(&pop);
  }

  __ Bind(&loop);
  __ RegexCheckPosition(&next);
  if (node->type() == Repeat::GREEDY) {
    __ RegexPushThread(&next);
    jump_point_ = Just(&pop);
    node->target()->Visit(this);
    __ RegexJumpIfMatched(&loop);
  } else {
    INVALIDATE(node->type() == Repeat::SHORTEST);
    Label push;
    jump_point_ = Just(&pop);
    node->target()->Visit(this);
    __ Branch(&push, &next);
    __ Bind(&push);
    { __ RegexPushThread(&loop); }
  }

  __ Bind(&pop);
  __ RegexPopThread();

  __ Bind(&next);
  if (count == 1) {
    Label ok;
    __ RegexJumpIfMatchedCountLT(count, &pop);
    __ RegexJump(&ok);

    __ Bind(&ok);
  }
}

AST_VISIT(RepeatRange) {
  __ RegexComment("RepeatRange");

  auto least_count = node->more_than();
  auto max_count = node->less_than();

  Label loop, next, pop;

  if (least_count > 0) {
    __ RegexCheckPosition(&pop);
  }

  __ Bind(&loop);
  {
    __ RegexJumpIfMatchedCountEqual(max_count, &next);
    __ RegexCheckPosition(&next);
    __ RegexPushThread(&next);
    jump_point_ = Just(&pop);
    node->target()->Visit(this);
    __ RegexJumpIfMatched(&loop);

    __ Bind(&pop);
    __ RegexPopThread();

    __ Bind(&next);
    {
      if (least_count > 0) {
        Label ok;
        __ RegexJumpIfMatchedCountLT(least_count, &pop);
        __ RegexResetMatchedCount();
        __ RegexJump(&ok);

        __ Bind(&ok);
      }
    }
  }
}

AST_VISIT(Char) {
  auto value = node->value();
  if (value.IsSurrogatePair()) {
    EmitCompare(value.ToLowSurrogate());
    EmitCompare(value.ToHighSurrogate());
  } else {
    EmitCompare(value.code());
  }
}

AST_VISIT(CharSequence) {
  __ RegexComment("CharSequence");
  EnableSearchIf();

  Label ok, pop;

  __ RegexEvery(node->value());
  jump_point_ >>= [&](auto point) { __ RegexBranch(&ok, point); };

  __ Bind(&ok);
}

AST_VISIT(EscapeSequence) {
  __ RegexComment("EscapeSequence");
  __ RegexEscapeSequence(static_cast<uint8_t>(node->type()));

  Label ok;

  jump_point_ >>= [&](auto point) { __ RegexBranch(&ok, point); };
  __ Bind(&ok);
}

AST_VISIT(Any) {
  EnableSearchIf();
  bool fall_through = false;

  switch (node->type()) {
    case Any::EAT_ANY:
      __ RegexComment("Any");
      __ RegexMatchAny();
      break;
    case Any::EAT_MINIMUM_L1: {
      fall_through = true;
      __ RegexComment("Any-Shortest-L1");
      __ RegexMatchAny();
      // FALL THROUGH
    }
    case Any::EAT_MINIMUM: {
      if (!fall_through) {
        __ RegexComment("Any-Shortest");
      }
      Label loop, pop, next;

      __ Bind(&loop);
      __ RegexCheckPosition(&next);
      __ RegexMatchAny();
      __ Bind(&next);
    } break;
    case Any::EAT_GREEDY_L1: {
      fall_through = true;
      __ RegexComment("Any-Greedy-L1");
      __ RegexMatchAny();
      // FALL THROUGH
    }
    case Any::EAT_GREEDY: {
      if (!fall_through) {
        __ RegexComment("Any-Greedy");
      }
      Label loop, pop, next;

      __ Bind(&loop);
      __ RegexCheckPosition(&next);
      __ RegexMatchAny();
      __ RegexJump(&loop);
      __ Bind(&next);
    } break;
    default:
      UNREACHABLE();
  }
}

void Visitor::EmitCompare(u16 code) {
  EnableSearchIf();

  Label ok, pop;
  __ RegexRune(code);
  __ RegexBranch(&ok, &pop);
  __ Bind(&pop);
  __ RegexPopThread();
  __ Bind(&ok);
}

void Visitor::EnableSearchIf() {
  if (is_retryable()) {
    __ RegexEnableSearch();
  }
}

bool IsRepeatChar(u16 ch) {
  return ch == '*' || ch == '+' || ch == '{' || ch == '?';
}

void Parser::Parse() {
  ENTER();
  if (cur() == '^') {
    advance();
    root_.set_config(Config::FROM_START);
  }

  auto regexp = ParseRegExp();

  if (cur() == '$') {
    advance();
    root_.set_config(Config::TO_END);
  }

  root_.set_regexp(regexp);
}

Ast* Parser::ParseRegExp() {
  ENTER();
  auto cj = new (zone()) Conjunction();
  while (has_more() && cur() != '$' && !has_pending_error()) {
    ParseDisjunction() >>= [&](Ast* a) { cj->Push(a); };
  }
  return cj;
}

Maybe<Ast*> Parser::ParseDisjunction() {
  ENTER();
  update_start_pos();
  auto atom = ParseAtom() >>= [&](Ast* node) {
    if (IsRepeatChar(cur().code())) {
      return ParseRepeat(node) >>=
             [&](Ast* repeat) { return ParseRepeat(repeat); };
    }

    return Just(node);
  };

  return atom >>= [&](Ast* node) {
    while (cur() == '|' && !has_pending_error()) {
      advance();
      ParseDisjunction() >>=
          [&](Ast* n) { node = new (zone()) Alternate(node, n); };
    }

    return node;
  };
}  // namespace regexp

Maybe<Ast*> Parser::ParseAtom() {
  switch (cur()) {
    case '(':
      return ParseGroup();
    case '[':
      return ParseCharClass();
    case '.':
      return Just(new (zone()) Any());
    default:
      return ParseChar();
  }
}

Maybe<Ast*> Parser::ParseGroup() {
  ENTER();
  update_start_pos();
  INVALIDATE(cur() == '(');
  advance();
  Group::Type t = Group::CAPTURE;
  auto group_specifier_names = Nothing<JSString*>();

  if (cur() == '?') {
    advance();
    switch (cur()) {
      case ':':
        advance();
        t = Group::UNCAPTURE;
        break;
      case '=':
        advance();
        t = Group::POSITIVE_LOOKAHEAD;
        break;
      case '!':
        advance();
        t = Group::NEGATIVE_LOOKAHEAD;
        break;
      case '<':
        group_specifier_names = ParseGroupSpecifierName();
        if (!group_specifier_names) {
          return Nothing<Ast*>();
        }
        update_start_pos();
      default:
        break;  // TODO(Taketoshi Aono): Report Error.
    }
  }

  auto ret = new (zone())
      Group(t, capture_count(),
            group_specifier_names ? group_specifier_names.value() : nullptr);
  if (t != Group::UNCAPTURE) {
    Capture();
  }
  while (has_more() && cur() != ')' && cur() != '$' && !has_pending_error()) {
    ParseDisjunction() >>= [&](Ast* n) { ret->Push(n); };
  }
  update_start_pos();
  EXPECT(this, cur(), ')');
  return Just(ret);
}

Utf16CodePoint Parser::DecodeHexEscape(bool* ok, int len) {
  auto unicode_hex_start = cur();
  u32 ret = 0;
  if (unicode_hex_start == '{') {
    advance();
    while (cur() != '}' && cur() != Unicode::InvalidCodePoint()) {
      ret = ret * 16 + Chars::ToHexValue(cur());
      advance();
    }
    if (cur() != '}') {
      REPORT_SYNTAX_ERROR_NO_RETURN(this, "'}' expected.");
    } else {
      advance();
    }
  } else {
    for (int i = 0; i < len; i++) {
      if (Chars::IsHexDigit(cur())) {
        ret = ret * 16 + Chars::ToHexValue(cur());
      } else {
        *ok = false;
        return Utf16CodePoint(0);
      }
      advance();
    }
  }

  return Utf16CodePoint(ret);
}

Utf16CodePoint Parser::DecodeAsciiEscapeSequence(bool* ok) {
  auto u = DecodeHexEscape(ok, 2);
  if (u > 127) {
    *ok = false;
  }

  return u;
}

Maybe<JSString*> Parser::ParseGroupSpecifierName() {
  EXPECT_WITH_RETURN(this, cur(), '<', Nothing<JSString*>());
  if (!Chars::IsIdentifierStart(cur())) {
    REPORT_SYNTAX_ERROR_WITH_RETURN(this, "Invalid Group specifier name found.",
                                    Nothing<JSString*>());
  }
  std::vector<Utf16CodePoint> buf;
  auto value = cur();
  while (has_more() &&
         (Chars::IsIdentifierPart(cur(), false) || value == '\\') &&
         value != '>') {
    value = cur();
    if (value == '\\') {
      std::vector<Utf16CodePoint> unicode_identifier;
      advance();
      auto unicode_keyword = cur();
      if (Chars::IsStartUnicodeEscapeSequence(unicode_keyword)) {
        advance();
        bool ok = true;
        value = DecodeHexEscape(&ok);
        if (!ok) {
          REPORT_SYNTAX_ERROR_WITH_RETURN(this, "Invalid Unicode sequence.",
                                          Nothing<JSString*>());
        }
      }
    } else {
      advance();
    }
    buf.push_back(value);
  }

  if (cur() == '>') {
    advance();
  } else {
    REPORT_SYNTAX_ERROR_WITH_RETURN(this, "'>' expected.",
                                    Nothing<JSString*>());
  }

  return Just(*JSString::New(isolate_, buf.data(), buf.size()));
}

Maybe<Ast*> Parser::ParseCharClass() {
  ENTER();
  update_start_pos();
  INVALIDATE(cur() == '[');
  advance();
  bool exclude = false;
  if (cur() == '^') {
    exclude = true;
    advance();
  }
  bool escaped = false;
  bool success = false;
  std::vector<Utf16CodePoint> v;
  while (has_more()) {
    if ((cur() == ']' && !escaped)) {
      advance();
      success = true;
      break;
    } else if (!escaped && cur() == '\\') {
      if (escaped) {
        v.push_back(cur());
      }
      escaped = !escaped;
    } else {
      v.push_back(cur());
    }
    advance();
  }
  Handle<JSString> js_str = JSString::New(isolate_, v.data(), v.size());
  auto cc = new (zone()) CharClass(exclude, *js_str);
  update_start_pos();
  if (!success) {
    REPORT_SYNTAX_ERROR(this, "] expected.");
  }

  return Just(cc);
}

Maybe<Ast*> Parser::ParseChar(bool allow_selection) {
  ENTER();
  bool escaped = false;
  std::vector<Utf16CodePoint> buf;
  auto conj = new (zone()) Conjunction();
  while (has_more() && (!IsSpecialChar(cur()) || escaped)) {
    Maybe<EscapeSequence*> special = Nothing<EscapeSequence*>();
    auto current = advance();
    if (escaped) {
      switch (current) {
        case 'c': {
          if (Chars::IsCtrlSuccessor(current)) {
            current = Utf16CodePoint(Chars::GetAsciiCodeFromCarretWord(cur()));
            advance();
          } else {
            buf.push_back(Utf16CodePoint(0x005c));
          }
          break;
        }
        case 'x': {
          bool ok = true;
          current = DecodeAsciiEscapeSequence(&ok);
          if (!ok) {
            REPORT_SYNTAX_ERROR(this, "Invalid escape sequence.");
          }
          break;
        }
#define DEF_REGEXP_ESCAPE_SEQUENCE_AST_BUILDER(NAME, value, ch)              \
  case ch:                                                                   \
    special = Just(new (zone()) EscapeSequence(RegexSpecialCharType::NAME)); \
    break;
          REGEXP_ESCAPE_SEQUENCES(DEF_REGEXP_ESCAPE_SEQUENCE_AST_BUILDER)
#undef DEF_REGEXP_ESCAPE_SEQUENCE_AST_BUILDER
        default:
          auto code = Chars::GetAsciiCtrlCodeFromWord(current.code());
          if (code) {
            current = Utf16CodePoint(code);
          }
      }
      special >>= [&](EscapeSequence* special) {
        if (buf.size() == 1) {
          conj->Push(new (zone()) Char(buf.back()));
        } else if (buf.size() > 1) {
          auto str = JSString::New(isolate_, buf.data(), buf.size());
          conj->Push(new (zone()) CharSequence(*str));
        }
        conj->Push(special);
      };
    }
    if (current == '\\') {
      escaped = !escaped;
      if (escaped) {
        continue;
      }
    } else {
      escaped = false;
    }
    if (!special) {
      buf.push_back(current);
    } else {
      buf.clear();
    }
  }

  if (buf.size() == 0 && conj->size() == 0 && IsRepeatChar(cur().code())) {
    advance();
    update_start_pos();
    REPORT_SYNTAX_ERROR(this, "Nothing to repeat.");
  }

  if (conj->size() == 0) {
    if (buf.size() == 1) {
      return Just(new (zone()) Char(buf.back()));
    }
    auto str = JSString::New(isolate_, buf.data(), buf.size());
    return Just(new (zone()) CharSequence(*str));
  }

  if (conj->size() == 1) {
    return Just(conj->at(0));
  }
  return Just(conj);
}

Maybe<Ast*> Parser::ParseRepeat(Ast* node) {
  ENTER();
  update_start_pos();

  switch (cur()) {
    case '?': {
      if (node->IsRepeat()) {
        node->UncheckedCastToRepeat()->set_type(Repeat::Type::SHORTEST);
      } else if (node->IsAny()) {
        auto any = node->UncheckedCastToAny();
        if (any->Is(Any::Type::EAT_GREEDY)) {
          any->set_type(Any::Type::EAT_MINIMUM);
        } else {
          any->set_type(Any::Type::EAT_MINIMUM_L1);
        }
      }
      advance();
      return SplitCharSequenceIf(node, [&](Ast* c) {
        return Just(new (zone()) RepeatRange(0, 1, c));
      });
    }
    case '{': {
      if (IsRepeat(node)) {
        advance();
        update_start_pos();
        REPORT_SYNTAX_ERROR(this, "Nothing to repeat");
      }
      return SplitCharSequenceIf(node,
                                 [&](Ast* c) { return ParseRangeRepeat(c); });
    }
    case '*': {
      advance();
      if (IsRepeat(node)) {
        update_start_pos();
        REPORT_SYNTAX_ERROR(this, "Nothing to repeat");
      }
      if (node->IsAny()) {
        return Just(new (zone()) Any(Any::EAT_GREEDY));
      }
      return SplitCharSequenceIf(node, [&](Ast* c) -> Maybe<Ast*> {
        return Just(new (zone()) Repeat(Repeat::GREEDY, 0, c));
      });
    }
    case '+': {
      advance();
      if (IsRepeat(node)) {
        update_start_pos();
        REPORT_SYNTAX_ERROR(this, "Nothing to repeat");
      }
      if (node->IsAny()) {
        return Just(new (zone()) Any(Any::EAT_GREEDY_L1));
      }
      return SplitCharSequenceIf(node, [&](Ast* a) -> Maybe<Ast*> {
        return Just(new (zone()) Repeat(Repeat::GREEDY, 1, a));
      });
    }
    default:
      return Just(node);
  }

  return Just(node);
}

template <typename T>
Maybe<Ast*> Parser::SplitCharSequenceIf(Ast* node, T factory) {
  if (!node->IsCharSequence()) {
    return factory(node);
  }
  auto char_sequence = node->UncheckedCastToCharSequence();
  auto value = char_sequence->value();
  auto last_char = value->Slice(isolate_, value->length() - 1, value->length());
  auto conjunction = new (zone()) Conjunction();
  auto start = value->Slice(isolate_, 0, value->length() - 1);
  auto char_item = new (zone()) Char(last_char->at(0));
  if (start->length() == 1) {
    conjunction->Push(new (zone()) Char(start->at(0)));
  } else {
    conjunction->Push(new (zone()) CharSequence(*start));
  }
  return factory(char_item) >>= [&](Ast* ast) {
    conjunction->Push(ast);
    return Just(conjunction);
  };
}

Utf16String::ParseIntResult Parser::ToInt() {
  std::vector<Utf16CodePoint> num;
  if (utf16::IsNumericRange(cur())) {
    while (utf16::IsNumericRange(cur())) {
      num.push_back(Utf16CodePoint(cur()));
      advance();
    }
    return Utf16String::FromVector(num).ParseInt();
  }
  return Utf16String::ParseIntResult::Failure();
}

Maybe<Ast*> Parser::ParseRangeRepeat(Ast* node) {
  ENTER()
  update_start_pos();
  INVALIDATE(cur() == '{');
  advance();
  uint32_t start = 0;
  uint32_t end = 0;

  SkipWhiteSpace();

  auto ret = ToInt();
  if (ret.IsNaN()) {
    REPORT_SYNTAX_ERROR(this, "number expected.");
  }
  start = ret.value();

  SkipWhiteSpace();

  bool has_end_range = false;
  if (cur() == ',') {
    update_start_pos();
    advance();
    SkipWhiteSpace();
    if (cur() == '}') {
      advance();
      update_start_pos();
      return Just(new (zone()) Repeat(
          cur() == '?' ? Repeat::Type::SHORTEST : Repeat::Type::GREEDY, start,
          node));
    }
    ret = ToInt();
    if (ret.IsNaN()) {
      REPORT_SYNTAX_ERROR(this, "number expected.");
    }
    end = ret.value();
    SkipWhiteSpace();
    has_end_range = true;
  }

  update_start_pos();
  EXPECT(this, cur(), '}');

  if (!has_end_range) {
    end = start;
  }
  return Just(new (zone()) RepeatRange(start, end, node));
}

void Parser::SkipWhiteSpace() {
  while (Chars::IsWhiteSpace(cur())) {
    advance();
    update_start_pos();
  }
}

bool Parser::IsSpecialChar(Utf16CodePoint cp) const {
  return cp == '^' || cp == '$' || cp == '[' || cp == ']' || cp == '{' ||
         cp == '}' || cp == '*' || cp == '+' || cp == '{' || cp == '.' ||
         cp == '?' || cp == '|' || cp == '(' || cp == ')';
}

Handle<JSRegExp> Compiler::Compile(const char* source, uint8_t flag) {
  HandleScope scope;
  auto regexp = JSString::New(isolate_, source);
  Parser parser(isolate_, error_reporter_, sp_, regexp);
  parser.Parse();
  ZoneAllocator zone_allocator;
  BytecodeBuilder bytecode_builder(isolate_, &zone_allocator);
  Visitor visitor(parser.capture_count(), &bytecode_builder, &zone_allocator);
  parser.node()->Visit(&visitor);
  auto executable = bytecode_builder.flush();
  return JSRegExp::New(isolate_, *scope.Return(executable), flag);
}
}  // namespace regexp
}  // namespace lux
