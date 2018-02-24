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
#include "./objects/jsobject.h"
#include "./unicode.h"

namespace lux {
namespace regexp {
#define ENTER()   printf("%s\n", __FUNCTION__);

#define BASE_REPORT_SYNTAX_ERROR_(parser)                               \
  auto e = std::make_shared<lux::ErrorDescriptor>(parser->position());  \
  (reporter_->ReportSyntaxError(e))                                     \

#ifdef DEBUG
#define REPORT_SYNTAX_ERROR(parser, message)                            \
  JSString::Utf8String str(*source_);                                   \
  BASE_REPORT_SYNTAX_ERROR_(parser)                                     \
  << "Invalid regular expression: /"                                    \
  << str.value()                                                        \
  << "/: "                                                              \
  << message                                                            \
  << " ("                                                               \
  << std::to_string(parser->position().start_line_number() + 1).c_str() \
  << ":" << std::to_string(parser->position().start_col() + 1).c_str()  \
  << "~" << std::to_string(parser->position().end_col() + 1).c_str()    \
  << ')'                                                                \
  << "\n[Debug] line:" << __LINE__                                      \
  << ", function: " << __FUNCTION__;                                    \
  return nullptr;
#else
#define REPORT_SYNTAX_ERROR(parser, message)    \
  BASE_REPORT_SYNTAX_ERROR_(parser) << message  \
  return nullptr;
#endif

#define EXPECT_NOT_ADVANCE(parser, n, expect)         \
  if (n != expect) {                                  \
    REPORT_SYNTAX_ERROR(                              \
        parser, "'" << expect << "' expected");       \
  }

#define EXPECT(parser, n, expect)               \
  EXPECT_NOT_ADVANCE(parser, n, expect)         \
  advance();

const char* Ast::kNodeTypeStringList[] = {
#define AST_STRING_DEF(V, v) #V,
  REGEXP_AST_TYPES(AST_STRING_DEF)
#undef AST_STRING_DEF
};

#define AST_VISIT(Name)                           \
  void Visitor::Visit##Name(Name* node)

#define __ builder()->
using Label = BytecodeLabel;

Visitor::Visitor(uint16_t captured_count,
                 BytecodeBuilder* bytecode_builder,
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
  for (auto &a : *node) {
    a->Visit(this);
  }
}

AST_VISIT(Group) {
  __ RegexComment("Group");

  if (node->IsCapturable()) {
    __ RegexStartCapture(node->captured_index());
  }

  for (auto &a : *node) {
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
  {
    __ RegexPopThread();
  }

  __ Bind(&ok);
}

AST_VISIT(Alternate) {
  __ RegexComment("Alternate");

  Label next, exit;
  node->left()->Visit(this);
  __ RegexBranch(&exit, &next);

  __ Bind(&next);
  {
    Label pop;
    node->right()->Visit(this);
    __ RegexBranch(&exit, &pop);

    __ Bind(&pop);
    __ RegexPopThread();
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
    node->target()->Visit(this);
    __ RegexJumpIfMatched(&loop);
  } else {
    INVALIDATE(node->type() == Repeat::SHORTEST);
    Label push;
    node->target()->Visit(this);
    __ Branch(&push, &next);
    __ Bind(&push);
    {
      __ RegexPushThread(&loop);
    }
  }
  __ Bind(&next);
  if (count == 1) {
    Label ok;
    __ RegexJumpIfMatchedCountLT(count, &pop);
    __ RegexJump(&ok);

    __ Bind(&pop);
    __ RegexPopThread();

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
    node->target()->Visit(this);
    __ RegexJumpIfMatched(&loop);

    __ Bind(&next);
    {
      if (least_count > 0) {
        Label ok;
        __ RegexJumpIfMatchedCountLT(least_count, &pop);
        __ RegexResetMatchedCount();
        __ RegexJump(&ok);

        __ Bind(&pop);
        __ RegexPopThread();

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
  __ RegexBranch(&ok, &pop);

  __ Bind(&pop);
  {
    __ RegexPopThread();
  }

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
      __ RegexPushThread(&loop);
      __ Bind(&next);
    }
      break;
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
      __ RegexPushThread(&next);
      __ RegexMatchAny();
      __ RegexJump(&loop);
      __ Bind(&next);
    }
      break;
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
  if (first_op() && is_retryable()) {
    __ RegexEnableSearch();
    set_first_op();
  }
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
  auto cj = new(zone()) Conjunction();
  while (has_more() && cur() != '$') {
    auto a = ParseRoot();
    if (!a) {
      return cj;
    }
    cj->Push(a);
  }
  return cj;
}

Ast* Parser::ParseRoot() {
  ENTER();
  update_start_pos();
  switch (cur()) {
    case '(':
      return ParseGroup();
    case '[':
      return ParseCharClass();
    case '.':
      advance();
      return ParseSelection(new(zone()) Any());
    default:
      return ParseChar();
  }
}

Ast* Parser::ParseGroup() {
  ENTER();
  update_start_pos();
  INVALIDATE(cur() == '(');
  advance();
  Group::Type t = Group::CAPTURE;

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
      default:
        break;  // TODO(Taketoshi Aono): Report Error.
    }
  }

  auto ret = new(zone()) Group(t, capture_count());
  if (t != Group::UNCAPTURE) {
    Capture();
  }
  while (cur() != ')' && cur() != '$') {
    auto p = ParseRoot();
    ret->Push(p);
  }
  update_start_pos();
  EXPECT(this, cur(), ')');
  return ParseSelection(ret);
}

Ast* Parser::ParseCharClass() {
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
  auto cc = new(zone()) CharClass(exclude, *js_str);
  update_start_pos();
  if (!success) {
    REPORT_SYNTAX_ERROR(this, "] expected.");
  }

  return ParseSelection(cc);
}

bool IsRepeatChar(u16 ch) {
  return ch == '*'
    || ch == '+'
    || ch == '{';
}

Ast* Parser::ParseChar(bool allow_selection) {
  ENTER();
  bool escaped = false;
  std::vector<Utf16CodePoint> buf;
  while (has_more()
         && (!IsSpecialChar(cur()) || escaped)) {
    if (cur() == '\\') {
      escaped = !escaped;
    } else {
      escaped = false;
    }
    auto n = advance();
    buf.push_back(n);
  }

  if (buf.size() == 0 && IsRepeatChar(cur().code())) {
    advance();
    REPORT_SYNTAX_ERROR(this, "Nothing to repeat.");
  }

  Ast* node = nullptr;
  if (buf.size() == 1) {
    node = new(zone()) Char(buf.back());
  } else {
    auto str = JSString::New(isolate_, buf.data(), buf.size());
    node = new(zone()) CharSequence(*str);
  }

  return allow_selection? ParseSelection(node): node;
}

Ast* Parser::ParseSelection(Ast* node) {
  ENTER();
  update_start_pos();
  while (has_more()) {
    switch (cur()) {
      case '?': {
        advance();
        node = new(zone()) RepeatRange(0, 1, node);
        break;
      }
      case '{': {
        node = ParseRangeRepeat(node);
        break;
      }
      case '|': {
        advance();
        auto n = ParseChar();
        node = new(zone()) Alternate(node, n);
        break;
      }
      case '*': {
        advance();
        if (node->IsAny()) {
          if (cur() == '?') {
            advance();
            node = new(zone()) Any(Any::EAT_MINIMUM);
            break;
          }
          node = new(zone()) Any(Any::EAT_GREEDY);
          break;
        }

        auto type = Repeat::GREEDY;
        if (cur() == '?') {
          advance();
          type = Repeat::SHORTEST;
        }
        node = new(zone()) Repeat(type, 0, node);
        break;
      }
      case '+': {
        advance();
        if (node->IsAny()) {
          if (cur() == '?') {
            advance();
            node = new(zone()) Any(Any::EAT_MINIMUM_L1);
            break;
          }
          node = new(zone()) Any(Any::EAT_GREEDY_L1);
          break;
        }

        auto type = Repeat::GREEDY;
        if (cur() == '?') {
          advance();
          type = Repeat::SHORTEST;
        }
        node = new(zone()) Repeat(type, 1, node);
        break;
      }
      default:
        return node;
    }
    if (!node) { return node; }
  }

  return node;
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

Ast* Parser::ParseRangeRepeat(Ast* node) {
  ENTER()
  update_start_pos();
  INVALIDATE(cur() == '{');
  advance();
  uint32_t start = 0;
  uint32_t end = 0;

  while (cur() == ' ') {
    update_start_pos();
    advance();
  }

  auto ret = ToInt();
  if (ret.IsNaN()) {
    REPORT_SYNTAX_ERROR(this, "number expected.");
  }
  start = ret.value();

  while (cur() == ' ') {
    update_start_pos();
    advance();
  }

  bool has_end_range = false;
  if (cur() == ',') {
    update_start_pos();
    advance();
    ret = ToInt();
    if (ret.IsNaN()) {
      REPORT_SYNTAX_ERROR(this, "number expected.");
    }
    end = ret.value();
    while (cur() == ' ') {
      update_start_pos();
      advance();
    }
    has_end_range = true;
  }

  update_start_pos();
  EXPECT(this, cur(), '}');

  if (!has_end_range) {
    end = start;
  }

  return new(zone()) RepeatRange(start, end, node);
}

bool Parser::IsSpecialChar(Utf16CodePoint cp) const {
  return cp == '^'
    || cp == '$'
    || cp == '['
    || cp == ']'
    || cp == '{'
    || cp == '}'
    || cp == '*'
    || cp == '+'
    || cp == '{'
    || cp == '.'
    || cp == '?'
    || cp == '|'
    || cp == '('
    || cp == ')';
}

Handle<JSRegExp> Compiler::Compile(const char* source, uint8_t flag) {
  HandleScope scope;
  auto regexp = JSString::New(isolate_, source);
  Parser parser(isolate_, error_reporter_, sp_, regexp);
  parser.Parse();
  ZoneAllocator zone_allocator;
  BytecodeBuilder bytecode_builder(isolate_, &zone_allocator);
  Visitor visitor(parser.capture_count(),
                  &bytecode_builder, &zone_allocator);
  parser.node()->Visit(&visitor);
  auto executable = bytecode_builder.flush();
  return JSRegExp::New(isolate_, *scope.Return(executable), flag);
}
}  // namespace regexp
}  // namespace lux
