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
#define ENTER_LOG()                              \
  printf("%s %c\n", __FUNCTION__, cur().code()); \
  const char* name = __FUNCTION__;               \
  LUX_SCOPED([&]() { printf("%sEnd %c\n", name, cur().code()); })

#define ENTER()  // ENTER_LOG()

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
    update_start_pos();                                         \
    REPORT_SYNTAX_ERROR(parser, "'" << expect << "' expected"); \
  }

#define EXPECT_NOT_ADVANCE_WITH_RETURN(parser, n, expect, retVal)          \
  if (n != expect) {                                                       \
    update_start_pos();                                                    \
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

#define AST_VISIT(Name) \
  void Visitor::Visit##Name(Name* node, AstContext* context)

#define __ builder()->
using Label = BytecodeLabel;

Visitor::Visitor(Isolate* isolate, uint32_t captured_count,
                 uint32_t matches_count_count,
                 BytecodeBuilder* bytecode_builder,
                 ZoneAllocator* zone_allocator, uint8_t flag)
    : flag_(flag),
      captured_count_(captured_count),
      matches_count_count_(matches_count_count),
      isolate_(isolate),
      bytecode_builder_(bytecode_builder),
      zone_allocator_(zone_allocator) {}

AST_VISIT(Root) {
  Var offset;
  AstContext new_context = {&matched_, &failed_};
  __ RegexComment("RegExp");

  if (node->is_from_start()) {
    disable_retry();
    __ RegexDisableRetry();
  }

  if (!IsGlobal()) {
    __ RegexReserveCapture(captured_count());
  }
  __ RegexReserveMatchesCount(matches_count_count());
  node->regexp()->Visit(this, &new_context);

  if (node->is_to_end()) {
    __ RegexCheckEnd();
    __ RegexJumpIfFailed(&failed_);
  }

  __ Bind(&matched_);
  __ RegexMatched();

  __ Bind(&failed_);
  __ RegexFailed();
}

AST_VISIT(Conjunction) {
  __ RegexComment("Conjunction");
  for (auto& a : *node) {
    a->Visit(this, context);
  }
}

AST_VISIT(Alternate) {
  __ RegexComment("Alternate");
  Label next, ok, pop, clean;
  AstContext then_context = {&clean, &pop};
  AstContext else_context = {&ok, context->failure_label};

  __ RegexComment("Alternate.PushThread");
  __ RegexPushThread(&next);
  node->left()->Visit(this, &then_context);
  __ RegexComment("Alternate.Branch");

  __ Bind(&clean);
  {
    __ RegexPopThread(1);
    __ RegexJump(&ok);
  }

  __ Bind(&pop);
  {
    __ RegexComment("Alternate.PopThread");
    __ RegexPopThread(0);
  }

  __ RegexComment("Alternative");
  __ Bind(&next);
  {
    node->right()->Visit(this, &else_context);
    __ RegexComment("Alternative.Branch");
  }

  __ Bind(&ok);
}

AST_VISIT(MatchRoot) {
  __ RegexComment("MatchRoot");

  __ RegexComment("MatchRoot.Store");
  __ RegexMatchPrologue();

  Label cleanup;

  AstContext new_context = {nullptr, &cleanup};

  for (auto& a : *node) {
    a->Visit(this, &new_context);
  }

  __ Bind(&cleanup);
  {
    __ RegexComment("MatchRoot.Load");
    __ RegexMatchEpilogue();
    EmitResultBranch(context);
  }
}

AST_VISIT(Group) {
  __ RegexComment("Group");

  Label ok;

  if (node->IsCapture() && !IsGlobal()) {
    __ RegexStartCapture(node->captured_index());
  }

  if (node->IsPositiveLookahead()) {
    __ RegexStoreMatchEndPosition();
  }

  AstContext nc = {nullptr, context->failure_label};
  for (auto& a : *node) {
    a->Visit(this, &nc);
  }

  if (node->IsCapture() && !IsGlobal()) {
    __ RegexUpdateCapture(node->captured_index());
  }

  if (node->IsPositiveLookahead()) {
    Label ok;
    __ RegexJumpIfMatched(&ok);

    __ Bind(&ok);
    { __ RegexLoadMatchEndPosition(); }
  }

  if (node->IsNegativeLookahead()) {
    Label ok;
    __ RegexJumpIfFailed(&ok);

    __ Bind(&ok);
    {
      __ RegexFlipResult();
      __ RegexLoadMatchEndPosition();
    }
  }

  if (context->success_label) {
    __ RegexJump(context->success_label);
  }
}

AST_VISIT(CharClass) {
  __ RegexComment("CharClassOpen");
  if (!node->size() && !node->IsExclude()) {
    __ RegexNotMatch();
    __ RegexJump(context->failure_label);
    return;
  } else if (!node->size() && node->IsExclude()) {
    __ RegexEmpty();
    EmitResultBranch(context);
    return;
  }

  __ RegexToggleClassMatch(1);

  Label exit, every, failed;

  std::vector<Utf16CodePoint> buf;
  for (auto& child : *node) {
    if (child->IsChar()) {
      buf.push_back(child->UncheckedCastToChar()->value());
    } else {
      Label next;
      if (buf.size() > 0) {
        EmitSomeOrRune(buf);
        __ RegexJumpIfMatched(&exit);
        buf.clear();
      }

      __ Bind(&next);
      {
        AstContext nc = {&exit, nullptr};
        child->Visit(this, &nc);
      }
    }
  }

  __ Bind(&every);
  {
    if (buf.size()) {
      EmitSomeOrRune(buf);
    }
  }

  __ RegexComment("CharClassClose");
  __ Bind(&exit);
  {
    __ RegexToggleClassMatch(0);
    if (node->IsExclude()) {
      __ RegexFlipResult();
    }
    EmitResultBranch(context);
  }
}

AST_VISIT(BackReference) {
  __ RegexComment("BackReference");

  __ RegexBackReference(node->index());
}

AST_VISIT(CharRange) {
  __ RegexComment("CharRange");
  __ RegexCharRange(node->start(), node->end());
  EmitResultBranch(context);
}

AST_VISIT(Repeat) {
  __ RegexComment("Repeat");
  auto count = node->more_than();
  Label loop, pop, exit;

  auto target = node->target();
  if (target->IsGroup()) {
    auto g = target->UncheckedCastToGroup();
    if (g->size() == 1 && g->Last()->IsRepeat()) {
      auto r = g->Last()->UncheckedCastToRepeat();
      if (r->more_than() == 0 || (count == 1 && r->more_than() == 1)) {
        __ RegexComment("Repeat.Skip");
        target->Visit(this, context);
        return;
      }
    }
  }

  __ RegexStoreMatchedCount(node->index());
  __ RegexResetMatchedCount();

  if (count == 1) {
    __ RegexComment("Repeat.Check");
    __ RegexCheckPosition(&pop);
  }

  __ RegexComment("Repeat.Loop");
  __ Bind(&loop);
  __ RegexComment("Repeat.Check");
  __ RegexCheckPosition(&exit);

  if (node->type() == Repeat::GREEDY) {
    __ RegexComment("Repeat.Greedy");
    __ RegexPushThread(&exit);
    Label match, failure;
    AstContext nc = {&loop, &exit};
    node->target()->Visit(this, &nc);
  } else {
    __ RegexComment("Repeat.Ungreedy");
    INVALIDATE(node->type() == Repeat::SHORTEST);
    Label push;
    AstContext nc = {&push, &exit};
    node->target()->Visit(this, &nc);

    __ Bind(&push);
    {
      __ RegexComment("Repeat.PushThread");
      __ RegexPushThread(&loop);
      __ RegexComment("Repeat.Jump");
      __ RegexJump(&exit);
    }
  }

  __ Bind(&pop);
  __ RegexComment("Repeat.PopThread");
  __ RegexPopThread(0);

  __ Bind(&exit);
  {
    Label end;
    if (count == 0) {
      __ RegexComment("Repeat.SetMatch");
      __ RegexSetMatch();
    } else {
      __ RegexJumpIfMatchedCountGT(0, &end);
      __ RegexNotMatch();
    }

    __ Bind(&end);
    __ RegexLoadMatchedCount(node->index());
    __ RegexComment("Repeat.Branch");
    EmitResultBranch(context);
  }
}

AST_VISIT(RepeatRange) {
  __ RegexComment("RepeatRange");

  auto least_count = node->more_than();
  auto max_count = node->less_than();

  Label loop, check, exit;

  __ RegexComment("RepeatRange.PushMatchedCount");
  __ RegexStoreMatchedCount(node->index());
  __ RegexComment("RepeatRange.ResetMatchedCount");
  __ RegexResetMatchedCount();

  __ Bind(&loop);
  {
    Label check_length;
    __ RegexComment("RepeatRange.MCE");
    __ RegexJumpIfMatchedCountEqual(max_count, &exit);

    AstContext nc = {&loop, &check};
    node->target()->Visit(this, &nc);

    __ Bind(&check);
    {
      if (least_count > 0) {
        __ RegexComment("RepeatRange.MCGT");
        __ RegexJumpIfMatchedCountGT(least_count - 1, &exit);

        __ RegexComment("RepeatRange.NotMatch");
        __ RegexNotMatch();

        __ RegexComment("RepeatRange.Jump");
        __ RegexJump(&exit);
      } else {
        __ RegexSetMatch();
      }
    }
  }

  __ Bind(&exit);
  {
    __ RegexComment("RepeatRange.PopMatchedCount");
    __ RegexLoadMatchedCount(node->index());
    __ RegexStoreMatchedCount(node->index());
    __ RegexComment("RepeatRange.Jump");
    EmitResultBranch(context);
  }
}

AST_VISIT(Char) {
  auto value = node->value();
  if (value.IsSurrogatePair()) {
    __ RegexRune(value.ToLowSurrogate());
    __ RegexRune(value.ToHighSurrogate());
  } else {
    __ RegexRune(value.code());
  }
  EmitResultBranch(context);
}

AST_VISIT(CharSequence) {
  __ RegexComment("CharSequence");
  std::vector<Utf16CodePoint> buf;
  Label next, every, exit;

  for (auto& child : *node) {
    if (child->IsChar()) {
      buf.push_back(child->UncheckedCastToChar()->value());
    } else {
      if (buf.size() > 0) {
        __ RegexComment("CharSequence.Every");
        EmitEveryOrRune(buf, context);
        buf.clear();
      }

      __ RegexComment("CharSequence.Others");
      child->Visit(this, context);
    }
  }

  if (buf.size()) {
    __ RegexComment("CharSequence.Drain");
    EmitEveryOrRune(buf, context);
  }
}

AST_VISIT(EscapeSequence) {
  __ RegexComment("EscapeSequence");
  __ RegexEscapeSequence(static_cast<uint8_t>(node->type()));
  EmitResultBranch(context);
}

AST_VISIT(Any) {
  bool fall_through = false;

  switch (node->type()) {
    case Any::EAT_ANY:
      __ RegexComment("Any");
      __ RegexMatchAny();
      EmitResultBranch(context);
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
      Label next;

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

void Visitor::EmitEveryOrRune(const std::vector<Utf16CodePoint>& buf,
                              AstContext* context) {
  if (buf.size() > 1) {
    Handle<JSString> str = JSString::New(isolate_, buf.data(), buf.size());
    __ RegexEvery(*str);
  } else if (buf.size() == 1) {
    auto value = buf.at(0);
    if (value.IsSurrogatePair()) {
      __ RegexRune(value.ToLowSurrogate());
      __ RegexRune(value.ToHighSurrogate());
    } else {
      __ RegexRune(value.code());
    }
  }
  EmitResultBranch(context);
}

void Visitor::EmitSomeOrRune(const std::vector<Utf16CodePoint>& buf) {
  if (buf.size() > 1) {
    Handle<JSString> str = JSString::New(isolate_, buf.data(), buf.size());
    __ RegexSome(*str);
  } else if (buf.size() == 1) {
    __ RegexRune(buf.at(0).code());
  }
}

void Visitor::EmitResultBranch(AstContext* context) {
  if (context->success_label && context->failure_label) {
    __ RegexBranch(context->success_label, context->failure_label);
  } else if (context->failure_label) {
    __ RegexJumpIfFailed(context->failure_label);
  } else if (context->success_label) {
    __ RegexJumpIfMatched(context->success_label);
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
  auto mr = new (zone()) MatchRoot();
  while (has_more() && cur() != '$' && !has_pending_error()) {
    ParseDisjunction() >>= [&](Ast* node) { mr->Push(node); };
  }
  return mr;
}

LUX_INLINE Ast* PeelConjunction(Ast* node) {
  if (node->IsConjunction()) {
    auto c = node->UncheckedCastToConjunction();
    if (c->size() == 1) {
      return c->at(0);
    }
  }
  return node;
}

Maybe<Ast*> Parser::ParseDisjunction() {
  ENTER();
  update_start_pos();
  return ParseAtom() >>= [&](Ast* node) {
    if (IsRepeatChar(cur().code())) {
      ParseRepeat(node) >>= [&](Ast* a) { node = a; };
    }
    if (cur() == '?') {
      ParseRepeat(node) >>= [&](Ast* a) { node = a; };
    }

    while (cur() == '|' && !has_pending_error()) {
      advance();
      ParseDisjunction() >>= [&](Ast* n) {
        Ast* left = PeelConjunction(node);
        Ast* right = PeelConjunction(n);
        node = new (zone()) Alternate(left, right);
      };
    }

    return Just(node);
  };
}  // namespace regexp

Maybe<Ast*> Parser::ParseAtom() {
  ENTER();
  switch (cur()) {
    case '(':
      return ParseGroup();
    case '[':
      return ParseCharClass();
    case '.':
      advance();
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
        t = Group::SKIP_CAPTURING;
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
  if (t != Group::SKIP_CAPTURING && t != Group::POSITIVE_LOOKAHEAD &&
      t != Group::NEGATIVE_LOOKAHEAD) {
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
  if (u > 255) {
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
  auto char_class = new (zone()) CharClass(exclude);
  while (has_more() && !has_pending_error() && (escaped || cur() != ']')) {
    auto current = cur();

    if (current == '-' && char_class->size() > 0) {
      auto prev = char_class->Last();
      if (prev && prev->IsChar() &&
          Chars::IsCharRangeStart(
              prev->UncheckedCastToChar()->value().code())) {
        advance();
        auto prevChar = prev->UncheckedCastToChar();
        if (cur() == ']') {
          advance();
          char_class->Push(new (zone()) Char(Utf16CodePoint('-')));
          continue;
        }
        ParseSingleWord(true) >>= [&](Ast* next) {
          if (next->IsChar()) {
            auto nextChar = next->UncheckedCastToChar();
            if (nextChar->value() > prevChar->value()) {
              char_class->Set(char_class->size() - 1,
                              new (zone()) CharRange(prevChar->value().code(),
                                                     nextChar->value().code()));
            } else {
              REPORT_SYNTAX_ERROR_NO_RETURN(
                  this,
                  "First character class char is must be lower than sencond "
                  "char");
            }
          } else {
            char_class->Push(next);
          }
        };
      } else if (prev->type() == Ast::Type::BACK_REFERENCE) {
        char_class->Push(new (zone()) Char(Utf16CodePoint('-')));
        advance();
        continue;
      }
    } else {
      ParseSingleWord(true) >>= [&](Ast* a) { char_class->Push(a); };
    }
  }

  EXPECT_WITH_RETURN(this, cur(), ']', Nothing<CharClass*>());
  return Just(char_class);
}

Maybe<Ast*> Parser::ParseChar() {
  ENTER();
  bool escaped = false;
  auto char_sequence = new (zone()) CharSequence();

  while (has_more() && !has_pending_error() &&
         (escaped || !IsSpecialChar(cur()))) {
    ParseSingleWord(false) >>= [&](Ast* a) { char_sequence->Push(a); };
  }

  if (char_sequence->size() == 0 && IsRepeatChar(cur().code())) {
    advance();
    update_start_pos();
    REPORT_SYNTAX_ERROR(this, "Nothing to repeat.");
  }

  return Just(char_sequence);
}

Maybe<Ast*> Parser::ParseSingleWord(bool is_inner_char_class) {
  ENTER();
  bool escaped = false;

  while (has_more() && !has_pending_error()) {
    auto current = cur();
    if (escaped) {
      switch (current) {
        case 'c': {
          advance();
          if (Chars::IsCtrlSuccessor(cur())) {
            return Just(new (zone()) Char(
                Utf16CodePoint(Chars::GetAsciiCodeFromCarretWord(cur()))));
          }
          return Just(new (zone()) Char(Utf16CodePoint(0x005c)));
        }
        case 'u': {
          advance();
          bool ok = true;
          auto ret = DecodeHexEscape(&ok);
          if (!ok) {
            REPORT_SYNTAX_ERROR(this, "Invalid escape sequence.");
          }
          return Just(new (zone()) Char(ret));
        }
        case 'x': {
          advance();
          bool ok = true;
          auto ret = DecodeAsciiEscapeSequence(&ok);
          if (!ok) {
            REPORT_SYNTAX_ERROR(this, "Invalid escape sequence.");
          }
          return Just(new (zone()) Char(ret));
        }
#define DEF_REGEXP_ESCAPE_SEQUENCE_AST_BUILDER(NAME, value, ch) \
  case ch:                                                      \
    advance();                                                  \
    return Just(new (zone()) EscapeSequence(RegexSpecialCharType::NAME));
          REGEXP_ESCAPE_SEQUENCES(DEF_REGEXP_ESCAPE_SEQUENCE_AST_BUILDER)
#undef DEF_REGEXP_ESCAPE_SEQUENCE_AST_BUILDER
        default:
          if (Chars::IsDecimalDigit(current.code()) && current != '0') {
            bool ok = true;
            std::vector<Utf16CodePoint> buf;
            buf.push_back(current);
            advance();
            while (Chars::IsDecimalDigit(cur().code())) {
              buf.push_back(cur());
              advance();
            }
            auto ret = Chars::ParseInt(&buf, &ok);
            if (!ok) {
              REPORT_SYNTAX_ERROR(this, "Invalid Backreference.");
            }
            return Just(new (zone()) BackReference(static_cast<uint32_t>(ret)));
          } else {
            advance();
            auto code = Chars::GetAsciiCtrlCodeFromWord(current.code());
            if (code) {
              return Just(new (zone()) Char(Utf16CodePoint(code)));
            } else if (is_inner_char_class) {
              return Just(new (zone()) Char(Utf16CodePoint(current.code())));
            }
          }
      }
    }

    if (current == '\\') {
      escaped = !escaped;
      if (escaped) {
        advance();
        continue;
      }
    } else {
      escaped = false;
    }

    advance();
    return Just(new (zone()) Char(current));
  }

  return Nothing<Ast*>();
}

Maybe<Ast*> Parser::ParseRepeat(Ast* node) {
  ENTER();
  update_start_pos();
  std::string i;

  switch (cur()) {
    case '?': {
      advance();
      if (node->IsRepeat()) {
        auto r = node->UncheckedCastToRepeat();
        r->set_type(Repeat::Type::SHORTEST);
        r->set_more_than(0);
        return Just(node);
      } else if (node->IsAny()) {
        auto any = node->UncheckedCastToAny();
        if (any->Is(Any::Type::EAT_GREEDY)) {
          any->set_type(Any::Type::EAT_MINIMUM);
        } else {
          any->set_type(Any::Type::EAT_MINIMUM_L1);
        }
        return Just(node);
      }
      return SplitCharSequenceIf(node, [&](Ast* c) {
        return Just(new (zone())
                        Repeat(Repeat::SHORTEST, 0, c, RequireMatchesCount()));
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
        return Just(new (zone())
                        Repeat(Repeat::GREEDY, 0, c, RequireMatchesCount()));
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
        return Just(new (zone())
                        Repeat(Repeat::GREEDY, 1, a, RequireMatchesCount()));
      });
    }
    default:
      return Just(node);
  }

  return Just(node);
}

template <typename T>
Maybe<Ast*> Parser::SplitCharSequenceIf(Ast* node, T factory) {
  ENTER();

  if (!node->IsCharSequence()) {
    return factory(node);
  }
  auto char_sequence = node->UncheckedCastToCharSequence();
  if (char_sequence->size() == 0) {
    return factory(node);
  }

  if (char_sequence->size() == 1) {
    return factory(char_sequence->Last());
  }

  return factory(char_sequence->Last()) >>= [&](Ast* ast) -> Maybe<Ast*> {
    if (ast->IsRepeat()) {
      auto r = ast->UncheckedCastToRepeat();
      if (r->target()->IsChar()) {
        auto c = r->target()->UncheckedCastToChar();
        auto it = char_sequence->rbegin();
        auto end = char_sequence->rend();
        while (it != end && (*it)->IsChar() &&
               (*it)->UncheckedCastToChar()->value().code() ==
                   c->value().code()) {
          ++it;
        }
        char_sequence->erase(it.base(), char_sequence->end());
      }
    }

    if (char_sequence->size() == 0) {
      return Just(ast);
    }
    char_sequence->Set(char_sequence->size() - 1, ast);
    return Just(char_sequence);
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
    update_start_pos();
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
          node, RequireMatchesCount()));
    }
    ret = ToInt();
    if (ret.IsNaN()) {
      update_start_pos();
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
  return Just(new (zone())
                  RepeatRange(start, end, node, RequireMatchesCount()));
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
  Visitor visitor(isolate_, parser.capture_count(),
                  parser.matches_count_count(), &bytecode_builder,
                  &zone_allocator, flag);
  parser.node()->Visit(&visitor, nullptr);
  auto executable = bytecode_builder.flush();
  return JSRegExp::New(isolate_, *scope.Return(executable), flag);
}
}  // namespace regexp
}  // namespace lux
