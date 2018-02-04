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
#define ENTER()  /*printf("%s\n", __FUNCTION__);*/

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
  void Visitor::Visit##Name(Name* node, BytecodeLabel* label)

#define __ builder()->
using Label = BytecodeLabel;

Visitor::Visitor(BytecodeBuilder* bytecode_builder,
                 ZoneAllocator* zone_allocator)
    : bytecode_builder_(bytecode_builder),
      zone_allocator_(zone_allocator) {
  input_register_ = RegisterAllocator::Parameter(0);
}

AST_VISIT(Root) {
  RegisterRef offset;
  __ Comment("RegExp");
  __ ImmI32(0, position_register());
  __ LoadRA(input_register());
  __ CallFastPropertyA(FastProperty::kLength);
  __ StoreAR(&input_size_register_);
  return node->regexp()->Visit(this, label);
}

AST_VISIT(Conjunction) {
  __ Comment("Conjunction");

  if (!label) {
    Label exit, next;
    __ Jmp(&next);
    __ Bind(&exit);
    {
      __ Return();
    }
    __ Bind(&next);
    label = &exit;
  }
  for (auto &a : *node) {
    a->Visit(this, label);
  }
}

AST_VISIT(Group) {
  return node->node()->Visit(this, label);
}

AST_VISIT(CharClass) {
  __ Comment("CharClass");

  auto index = __ StringConstant(node->value());
  RegisterRef value_reg, index_reg, char_reg,
    target_char_reg, length_reg;
  Label loop, if_failed;
  __ ConstantA(index);
  __ StoreAR(&value_reg);
  __ ImmI32(0, &index_reg);
  __ ImmI32(node->value()->length(), &length_reg);

  __ Bind(&loop);
  {
    Label if_continue;
    __ ICmpGTRRA(&length_reg, &index_reg);
    __ BranchA(&if_continue, &if_failed);
    __ Bind(&if_continue);
    {
      __ LoadRIxR(&value_reg, &index_reg, &char_reg);
      __ LoadRIxR(input_register(), &index_reg, &target_char_reg);
      __ ICmpRR(&char_reg, &target_char_reg);
      __ Inc(&index_reg);
      __ JmpIfFalse(&loop);
    }
  }

  __ Bind(&if_failed);
  {
    if (label) {
      __ Jmp(label);
    } else {
      __ Return();
    }
  }
}

AST_VISIT(Alternate) {
  __ Comment("Alternate");

  Label if_exit, if_first, if_next;
  RegisterRef save_reg;
  __ LoadRA(position_register());
  __ StoreAR(&save_reg);
  __ Jmp(&if_first);

  __ Bind(&if_first);
  {
    node->left()->Visit(this, &if_next);
    __ Jmp(&if_exit);
  }

  __ Bind(&if_next);
  {
    node->right()->Visit(this, label);
  }

  __ Bind(&if_exit);
}

AST_VISIT(Repeat) {
  return node->target()->Visit(this, label);
}

AST_VISIT(RepeatRange) {
  return node->target()->Visit(this, label);
}

AST_VISIT(Char) {
  auto value = node->value();
  if (value.IsSurrogatePair()) {
    EmitCompare(value.ToLowSurrogate(), label);
    EmitCompare(value.ToHighSurrogate(), label);
  } else {
    EmitCompare(value.code(), label);
  }
}

void Visitor::EmitCompare(u16 code, BytecodeLabel* label) {
  __ Comment("CompareChar");

  RegisterRef reg1, reg2;
  __ LoadRA(input_register());
  __ LoadAIxR(position_register(), &reg1);
  __ ImmI32(code, &reg2);
  __ LoadRA(&reg1);
  __ ICmpAR(&reg2);

  if (label) {
    __ JmpIfFalse(label);
  }

  __ Inc(position_register());
}

void Parser::Parse() {
  ENTER();
  if (cur() == '^') {
    advance();
    root_.set_config(Config::FROM_START);
  }

  root_.set_regexp(ParseRegExp());
}

Ast* Parser::ParseRegExp() {
  ENTER();
  auto cj = new(zone()) Conjunction();
  while (has_more()) {
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
  auto p = ParseRoot();
  if (!p) { return nullptr; }
  auto ret = new(zone()) Group(t, p);
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
  auto cj = new(zone()) Conjunction();
  bool escaped = false;
  while (has_more()
         && (!IsSpecialChar(cur()) || escaped)) {
    if (cur() == '\\') {
      escaped = !escaped;
    } else {
      escaped = false;
    }
    auto n = advance();
    auto c = new(zone()) Char(n);
    cj->Push(c);
  }
  if (cj->size() == 0 && IsRepeatChar(cur().code())) {
    advance();
    REPORT_SYNTAX_ERROR(this, "Nothing to repeat.");
  }
  return allow_selection? ParseSelection(cj): cj;
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
        node = new(zone()) Repeat(0, node);
        break;
      }
      case '+': {
        advance();
        node = new(zone()) Repeat(1, node);
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
    return Utf16String(num.data(), num.size()).ParseInt();
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

Handle<JSFunction> Compiler::Compile(const char* source) {
  HandleScope scope;
  auto regexp = JSString::New(isolate_, source);
  Parser parser(isolate_, error_reporter_, sp_, regexp);
  parser.Parse();
  ZoneAllocator zone_allocator;
  BytecodeBuilder bytecode_builder(isolate_, &zone_allocator);
  Visitor visitor(&bytecode_builder, &zone_allocator);
  parser.node()->Visit(&visitor, nullptr);
  auto executable = bytecode_builder.flush();
  return JSFunction::New(isolate_,
                         *scope.Return(
                             JSString::New(isolate_, "regexp-scratch-space")),
                         1,
                         *scope.Return(executable));
}
}  // namespace regexp
}  // namespace lux
