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

#include "./bytecode.h"
#include "./heap.h"
#include "./isolate.h"

namespace lux {

using bu = BytecodeUtil;

const BytecodeOperand BytecodeUtil::kOperandA = {0, 0xFF};
const BytecodeOperand BytecodeUtil::kOperandB = {8, 0xFF};
const BytecodeOperand BytecodeUtil::kOperandC = {16, 0xFF};
const BytecodeOperand BytecodeUtil::kOperandD = {24, 0xFF};
const BytecodeOperand BytecodeUtil::kOperandE = {32, 0xFF};
const BytecodeOperand BytecodeUtil::kOperandF = {40, 0xFF};
const BytecodeOperand BytecodeUtil::kOperandG = {48, 0xFF};
const BytecodeOperand BytecodeUtil::kOperandH = {56, ~static_cast<uint64_t>(0)};

const BytecodeOperand BytecodeUtil::kOperandAx = {0, 0xFFFF};
const BytecodeOperand BytecodeUtil::kOperandBx = {16, 0xFFFF};
const BytecodeOperand BytecodeUtil::kOperandCx = {32, 0xFFFF};
const BytecodeOperand BytecodeUtil::kOperandDx = {48,
                                                  ~static_cast<uint64_t>(0)};

const BytecodeOperand BytecodeUtil::kOperandEax = {0, 0xFFFFFFFF};
const BytecodeOperand BytecodeUtil::kOperandEbx = {32,
                                                   ~static_cast<uint64_t>(0)};

const BytecodeOperand BytecodeUtil::kOperandRax = {0,
                                                   ~static_cast<uint64_t>(0)};

#define BYTECODE_CHECK(Name, Layout, size, argc, ...)                                                  \
  static_assert(                                                                                       \
      BytecodeLayout::k##Layout == BytecodeLayout::kNone                                               \
          ? size == 1                                                                                  \
          : BytecodeLayout::k##Layout == BytecodeLayout::kShort1                                       \
                ? size == 2                                                                            \
                : BytecodeLayout::k##Layout == BytecodeLayout::kShort2                                 \
                      ? size == 3                                                                      \
                      : BytecodeLayout::k##Layout == BytecodeLayout::kShort3                           \
                            ? size == 4                                                                \
                            : BytecodeLayout::k##Layout ==                                             \
                                      BytecodeLayout::kDouble1                                         \
                                  ? size == 4                                                          \
                                  : BytecodeLayout::k##Layout ==                                       \
                                            BytecodeLayout::kDouble2                                   \
                                        ? size == 5                                                    \
                                        : BytecodeLayout::k##Layout ==                                 \
                                                  BytecodeLayout::kDouble3                             \
                                              ? size == 3                                              \
                                              : BytecodeLayout::k##Layout ==                           \
                                                        BytecodeLayout::kWord1                         \
                                                    ? size == 9                                        \
                                                    : BytecodeLayout::                                 \
                                                                  k##Layout ==                         \
                                                              BytecodeLayout::                         \
                                                                  kWide1                               \
                                                          ? size == 5                                  \
                                                          : BytecodeLayout::                           \
                                                                        k##Layout ==                   \
                                                                    BytecodeLayout::                   \
                                                                        kWide2                         \
                                                                ? size == 6                            \
                                                                : BytecodeLayout::                     \
                                                                              k##Layout ==             \
                                                                          BytecodeLayout::             \
                                                                              kWide3                   \
                                                                      ? size ==                        \
                                                                            9                          \
                                                                      : BytecodeLayout::               \
                                                                                    k##Layout ==       \
                                                                                BytecodeLayout::       \
                                                                                    kWide4             \
                                                                            ? size ==                  \
                                                                                  7                    \
                                                                            : BytecodeLayout::         \
                                                                                          k##Layout == \
                                                                                      BytecodeLayout:: \
                                                                                          kWide5       \
                                                                                  ? size ==            \
                                                                                        8              \
                                                                                  : false,             \
      "Bytecode " #Name " size is incorrect.");
REGEX_BYTECODE_LIST(BYTECODE_CHECK)
#undef BYTECODE_CHECK

const std::array<BytecodeLayout,
                 static_cast<uint8_t>(Bytecode::kLastSentinel__)>
    bu::kLayout = {{
#define BYTECODE_LAYOUT_DEF(Name, Layout, size, num, ...) \
  BytecodeLayout::k##Layout,
        BYTECODE_LIST(BYTECODE_LAYOUT_DEF)
#undef BYTECODE_LAYOUT_DEF
    }};

const std::array<uint8_t, static_cast<uint8_t>(Bytecode::kLastSentinel__)>
    bu::kSize = {{
#define BYTECODE_LAYOUT_DEF(Name, Layout, size, num, ...) \
  static_cast<uint8_t>(size),
        BYTECODE_LIST(BYTECODE_LAYOUT_DEF)
#undef BYTECODE_LAYOUT_DEF
    }};

#ifdef DEBUG
std::string bu::ToString(BytecodeFetcher* fetcher) {
  std::stringstream st;
  auto bc = fetcher->FetchBytecode();
  if (bc == Bytecode::kComment || bc == Bytecode::kRegexComment) {
    st << ";;" << fetcher->FetchNextPtrOperand<const char*>();
    return st.str();
  } else if (bc == Bytecode::kExit) {
    return "Exit";
  }

  switch (bc) {
#define BYTECODE_CASE(Name, Layout, size, n, ...)   \
  case Bytecode::k##Name: {                         \
    size_t len = sizeof(#Name) - 1;                 \
    st << #Name << ToStringField(bc, fetcher, len); \
    return st.str();                                \
  }
    BYTECODE_LIST(BYTECODE_CASE)
#undef BYTECODE_CASE
    default:
      UNREACHABLE();
      return "";
  }
}

std::string BytecodeConstantArray::ToString() const {
  std::stringstream st;
  auto max = std::to_string(length()).size();
  st << "---------------------------------------------------\n";
  st << "ConstantPool { size = " << length() << " }\n";
  for (int i = 0; i < length(); i++) {
    auto width = std::to_string(i).size();
    auto indent = std::string(max - width, ' ');
    st << "  (" << i << ')' << indent << ' ' << at(i)->ToString();
  }
  st << "\n---------------------------------------------------\n";
  return st.str();
}

std::string BytecodeArray::ToString(BytecodeConstantArray* constant_pool) {
  BytecodeFetcher fetcher(this);
  std::stringstream st;
  auto max = std::to_string(length()).size();
  for (auto i = 0; fetcher.HasMore(); i++) {
    auto size = std::to_string(i).size();
    auto start = fetcher.pc();
    auto start_size = std::to_string(start).size();
    auto detail = BytecodeUtil::ToString(&fetcher);
    auto end = fetcher.pc() - 1;
    auto end_size = std::to_string(end).size();
    std::string indent((max - size) + 1, ' ');
    std::string indent2((max - start_size) + 1, ' ');
    std::string indent3((max - end_size) + 1, ' ');
    st << (i > 0 ? "\n" : "") << start << indent2 << "- " << end << indent3
       << indent << i << ": " << detail;
  }
  st << "\n\n" << constant_pool->ToString();
  return st.str();
}

std::string BytecodeUtil::ToStringField(Bytecode bc, BytecodeFetcher* fetcher,
                                        size_t len) {
  std::stringstream st;
  std::string indent(28 - len, ' ');
  BytecodeLayout l = layout(bc);
  st << indent;
  switch (l) {
    case BytecodeLayout::kNone: {
      return "";
    }
    case BytecodeLayout::kShort1: {
      st << "  "
         << "A = " << +fetcher->FetchNextShortOperand();
      return st.str();
    }
    case BytecodeLayout::kShort2: {
      st << "  A = " << +fetcher->FetchNextShortOperand()
         << "  B = " << +fetcher->FetchNextShortOperand();
      return st.str();
    }
    case BytecodeLayout::kShort3: {
      st << "  A = " << +fetcher->FetchNextShortOperand()
         << "  B = " << +fetcher->FetchNextShortOperand()
         << "  C = " << +fetcher->FetchNextShortOperand();
      return st.str();
    }
    case BytecodeLayout::kDouble1: {
      st << "  Ax = " << +fetcher->FetchNextDoubleOperand()
         << "  C = " << +fetcher->FetchNextShortOperand();
      return st.str();
    }
    case BytecodeLayout::kDouble2: {
      st << "  Ax = " << +fetcher->FetchNextDoubleOperand()
         << "  Bx = " << +fetcher->FetchNextDoubleOperand();
      return st.str();
    }
    case BytecodeLayout::kDouble3: {
      st << "  Ax = " << +fetcher->FetchNextDoubleOperand();
      return st.str();
    }
    case BytecodeLayout::kWord1: {
      st << "  Rax = " << +fetcher->FetchNextWordOperand();
      return st.str();
    }
    case BytecodeLayout::kWide1: {
      st << "  Eax = " << +fetcher->FetchNextWideOperand();
      return st.str();
    }
    case BytecodeLayout::kWide2: {
      st << "  Eax = " << +fetcher->FetchNextWideOperand()
         << "  E = " << +fetcher->FetchNextShortOperand();
      return st.str();
    }
    case BytecodeLayout::kWide3: {
      st << "  Eax = " << +fetcher->FetchNextWideOperand()
         << "  Ebx = " << +fetcher->FetchNextWideOperand();
      return st.str();
    }
    case BytecodeLayout::kWide4: {
      st << "  Eax = " << +fetcher->FetchNextWideOperand()
         << "  Cx = " << +fetcher->FetchNextDoubleOperand();
      return st.str();
    }
    case BytecodeLayout::kWide5: {
      st << "  Eax = " << +fetcher->FetchNextWideOperand()
         << "  Cx = " << +fetcher->FetchNextDoubleOperand()
         << "  Dx = " << +fetcher->FetchNextDoubleOperand();
      return st.str();
    }
    default:
      UNREACHABLE();
  }
}
#endif

Var Var::kAcc = Var(RegisterAllocator::kAcc);
Var Var::kFlag = Var(RegisterAllocator::kFlag);

void BytecodeArrayWriter::Emit(BytecodeNode* bytecode) { Append(bytecode); }

void BytecodeArrayWriter::EmitJump(BytecodeLabel* label,
                                   BytecodeNode* bytecode) {
  Append(bytecode);
}

void BytecodeArrayWriter::Append(BytecodeNode* node) {
  if (!bytecode_top_) {
    bytecode_top_ = bytecode_list_ = node;
  } else {
    bytecode_list_->set_next(node);
    bytecode_list_ = node;
  }

  current_offset_ += bu::size(node->bytecode());
}

int32_t BytecodeArrayWriter::EmitConstant(BytecodeConstantNode* node) {
  if (!constant_top_) {
    constant_top_ = constant_list_ = node;
  } else {
    constant_list_->set_next(node);
    constant_list_ = node;
  }
  return constant_length_++;
}

void BytecodeArrayWriter::Bind(BytecodeLabel* label) {
  auto to = last_bytecode();
  label->set_to(to);
  label->set_bound_node(to);
  for (auto& from : label->from_list()) {
    from->set_jmp(to);
    jmps_.push_back(from);
  }
  for (auto& from : label->from2_list()) {
    from->set_jmp2(to);
    jmps_.push_back(from);
  }
  label->set_jmps(&jmps_);
}

Handle<BytecodeExecutable> BytecodeArrayWriter::Flush() {
  HandleScope scope;
  auto array = BytecodeArray::New(isolate_, current_offset_ + 2);
  size_t index = 0;

  auto node = bytecode_top_;
  while (node) {
    auto bytecode = node->bytecode();
    node->set_offset(index);
    array->write(index++, static_cast<uint8_t>(bytecode));
    switch (bu::layout(bytecode)) {
      case BytecodeLayout::kNone:
        INVALIDATE(bu::size(bytecode) == 1);
        break;
      case BytecodeLayout::kShort1:
        INVALIDATE(bu::size(bytecode) == 2);
        array->write(index++, node->DecodeOperand(bu::kOperandA));
        break;
      case BytecodeLayout::kShort2:
        INVALIDATE(bu::size(bytecode) == 3);
        array->write(index++, node->DecodeOperand(bu::kOperandA));
        array->write(index++, node->DecodeOperand(bu::kOperandB));
        break;
      case BytecodeLayout::kShort3:
        INVALIDATE(bu::size(bytecode) == 4);
        array->write(index++, node->DecodeOperand(bu::kOperandA));
        array->write(index++, node->DecodeOperand(bu::kOperandB));
        array->write(index++, node->DecodeOperand(bu::kOperandC));
        break;
      case BytecodeLayout::kDouble1:
        INVALIDATE(bu::size(bytecode) == 4);
        array->write(index++, node->DecodeOperand(bu::kOperandA));
        array->write(index++, node->DecodeOperand(bu::kOperandB));
        array->write(index++, node->DecodeOperand(bu::kOperandC));
        break;
      case BytecodeLayout::kDouble2:
        INVALIDATE(bu::size(bytecode) == 5);
        array->write(index++, node->DecodeOperand(bu::kOperandA));
        array->write(index++, node->DecodeOperand(bu::kOperandB));
        array->write(index++, node->DecodeOperand(bu::kOperandC));
        array->write(index++, node->DecodeOperand(bu::kOperandD));
        break;
      case BytecodeLayout::kDouble3:
        INVALIDATE(bu::size(bytecode) == 3);
        array->write(index++, node->DecodeOperand(bu::kOperandA));
        array->write(index++, node->DecodeOperand(bu::kOperandB));
        break;
      case BytecodeLayout::kWide1:
        INVALIDATE(bu::size(bytecode) == 5);
        array->write(index++, node->DecodeOperand(bu::kOperandA));
        array->write(index++, node->DecodeOperand(bu::kOperandB));
        array->write(index++, node->DecodeOperand(bu::kOperandC));
        array->write(index++, node->DecodeOperand(bu::kOperandD));
        break;
      case BytecodeLayout::kWide2:
        INVALIDATE(bu::size(bytecode) == 6);
        array->write(index++, node->DecodeOperand(bu::kOperandA));
        array->write(index++, node->DecodeOperand(bu::kOperandB));
        array->write(index++, node->DecodeOperand(bu::kOperandC));
        array->write(index++, node->DecodeOperand(bu::kOperandD));
        array->write(index++, node->DecodeOperand(bu::kOperandE));
        break;
      case BytecodeLayout::kWide4:
        INVALIDATE(bu::size(bytecode) == 7);
        array->write(index++, node->DecodeOperand(bu::kOperandA));
        array->write(index++, node->DecodeOperand(bu::kOperandB));
        array->write(index++, node->DecodeOperand(bu::kOperandC));
        array->write(index++, node->DecodeOperand(bu::kOperandD));
        array->write(index++, node->DecodeOperand(bu::kOperandE));
        array->write(index++, node->DecodeOperand(bu::kOperandF));
        break;
      case BytecodeLayout::kWide5:
      case BytecodeLayout::kWide3:
      case BytecodeLayout::kWord1:
        INVALIDATE(bu::size(bytecode) == 9);
        array->write(index++, node->DecodeOperand(bu::kOperandA));
        array->write(index++, node->DecodeOperand(bu::kOperandB));
        array->write(index++, node->DecodeOperand(bu::kOperandC));
        array->write(index++, node->DecodeOperand(bu::kOperandD));
        array->write(index++, node->DecodeOperand(bu::kOperandE));
        array->write(index++, node->DecodeOperand(bu::kOperandF));
        array->write(index++, node->DecodeOperand(bu::kOperandG));
        array->write(index++, node->DecodeOperand(bu::kOperandH));
        break;
      default:
        UNREACHABLE();
    }
    node = node->next();
  }

  for (auto& from : jmps_) {
    auto to = from->jmp();
    auto target = to->next() ? to->next()->offset() : to->offset() + 1;
    INVALIDATE(bu::size(from->bytecode()) >= 5);
    array->write(from->offset() + 1, bu::DecodeOperand(bu::kOperandA, target));
    array->write(from->offset() + 2, bu::DecodeOperand(bu::kOperandB, target));
    array->write(from->offset() + 3, bu::DecodeOperand(bu::kOperandC, target));
    array->write(from->offset() + 4, bu::DecodeOperand(bu::kOperandD, target));

    to = from->jmp2();
    if (to != nullptr) {
      INVALIDATE(bu::size(from->bytecode()) == 9);
      auto target = to->next() ? to->next()->offset() : to->offset() + 1;
      array->write(from->offset() + 5,
                   bu::DecodeOperand(bu::kOperandA, target));
      array->write(from->offset() + 6,
                   bu::DecodeOperand(bu::kOperandB, target));
      array->write(from->offset() + 7,
                   bu::DecodeOperand(bu::kOperandC, target));
      array->write(from->offset() + 8,
                   bu::DecodeOperand(bu::kOperandD, target));
    }
  }

  auto pool = BytecodeConstantArray::New(isolate_, constant_length_);
  auto bcn = constant_top_;
  index = 0;
  while (bcn) {
    auto obj = bcn->ptr();
    pool->write(index++, obj);
    bcn = bcn->next();
  }
  return BytecodeExecutable::New(isolate_, *scope.Return(array),
                                 *scope.Return(pool));
}

BytecodeNode* BytecodeBuilder::RegexComment(const char* comment) {
#ifdef DEBUG
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexComment,
                                     reinterpret_cast<uintptr_t>(comment));
  bytecode_array_writer_->Emit(n);
  return n;
#endif
  return nullptr;
}

BytecodeNode* BytecodeBuilder::RegexMatched() {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexMatched);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexFailed() {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexFailed);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexReserveCapture(uint32_t v) {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexReserveCapture, v);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexReserveMatchesCount(uint32_t v) {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexReserveMatchesCount, v);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexStartCapture(uint32_t v) {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexStartCapture, v);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexUpdateCapture(uint32_t v) {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexUpdateCapture, v);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexCharRange(u16 start, u16 end) {
  uint64_t x = end << 16;
  x |= start;
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexCharRange, x);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexDisableRetry() {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexDisableRetry);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexCheckEnd() {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexCheckEnd);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexMatchAny() {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexMatchAny);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexResetMatchedCount() {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexResetMatchedCount);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexJumpIfMatchedCountEqual(
    uint16_t count, BytecodeLabel* label) {
  auto operand = bu::EncodeOperand(bu::kOperandCx, count);
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexJumpIfMatchedCountEqual,
                                     operand, label->to());
  label->AddFrom(n);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexJumpIfMatchedCountRange(
    uint16_t countA, uint16_t countB, BytecodeLabel* label) {
  auto operand = bu::EncodeOperand(bu::kOperandCx, countA) |
                 bu::EncodeOperand(bu::kOperandCx, countB);
  auto n = new (zone())
      BytecodeNode(Bytecode::kRegexJumpIfMatchedCountGT, operand, label->to());
  label->AddFrom(n);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexJumpIfMatchedCountLT(uint16_t count,
                                                         BytecodeLabel* label) {
  auto operand = bu::EncodeOperand(bu::kOperandCx, count);
  auto n = new (zone())
      BytecodeNode(Bytecode::kRegexJumpIfMatchedCountLT, operand, label->to());
  label->AddFrom(n);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexJumpIfMatchedCountGT(uint16_t count,
                                                         BytecodeLabel* label) {
  auto operand = bu::EncodeOperand(bu::kOperandCx, count);
  auto n = new (zone())
      BytecodeNode(Bytecode::kRegexJumpIfMatchedCountGT, operand, label->to());
  label->AddFrom(n);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexCheckPosition(BytecodeLabel* label) {
  auto n =
      new (zone()) BytecodeNode(Bytecode::kRegexCheckPosition, label->to());
  label->AddFrom(n);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexPushThread(BytecodeLabel* label) {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexPushThread, label->to());
  label->AddFrom(n);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexPopThread(uint8_t f) {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexPopThread, f);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexRune(u16 u) {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexRune, u);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexEvery(JSString* input) {
  auto n = new (zone())
      BytecodeNode(Bytecode::kRegexEvery, reinterpret_cast<uintptr_t>(input));
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexSome(JSString* input) {
  auto n = new (zone())
      BytecodeNode(Bytecode::kRegexSome, reinterpret_cast<uintptr_t>(input));
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexBranch(BytecodeLabel* label,
                                           BytecodeLabel* label2) {
  auto n = new (zone())
      BytecodeNode(Bytecode::kRegexBranch, label->to(), label2->to());
  label->AddFrom(n);
  label2->AddFrom(n, true);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexJump(BytecodeLabel* label) {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexJump, label->to());
  label->AddFrom(n);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexJumpIfMatched(BytecodeLabel* label) {
  auto n =
      new (zone()) BytecodeNode(Bytecode::kRegexJumpIfMatched, label->to());
  label->AddFrom(n);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexJumpIfFailed(BytecodeLabel* label) {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexJumpIfFailed, label->to());
  label->AddFrom(n);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexEscapeSequence(uint8_t type) {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexEscapeSequence, type);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexBackReference(uint32_t index) {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexBackReference, index);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexToggleClassMatch(uint8_t flag) {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexToggleClassMatch, flag);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexMatchPrologue() {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexMatchPrologue);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexMatchEpilogue() {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexMatchEpilogue);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexLoadMatchEndPosition() {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexLoadMatchEndPosition);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexStoreMatchEndPosition() {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexStoreMatchEndPosition);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexLoadMatchedCount(uint32_t i) {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexLoadMatchedCount, i);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexStoreMatchedCount(uint32_t i) {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexStoreMatchedCount, i);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexEmpty() {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexEmpty);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexNotMatch() {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexNotMatch);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexSetMatch() {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexSetMatch);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::RegexFlipResult() {
  auto n = new (zone()) BytecodeNode(Bytecode::kRegexFlipResult);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::ExecIf() {
  auto n = new (zone()) BytecodeNode(Bytecode::kExecIf);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::NewEmptyJSArray(Var* var) {
  if (!var->has_id()) {
    var->set_id(allocate_regiseter(var->mode()));
  }
  auto n = new (zone()) BytecodeNode(Bytecode::kNewEmptyJSArray, var->id());
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::NewEmptyJSString(Var* var) {
  if (!var->has_id()) {
    var->set_id(allocate_regiseter(var->mode()));
  }
  auto n = new (zone()) BytecodeNode(Bytecode::kNewEmptyJSString, var->id());
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Return(Var* var) {
  if (!var->has_id()) {
    var->set_id(allocate_regiseter(var->mode()));
  }
  auto n = new (zone()) BytecodeNode(Bytecode::kReturn, var->id());
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::LoadConstant(uint16_t index, Var* reg1) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }

  auto operand = bu::EncodeOperand(bu::kOperandAx, index);
  operand |= bu::EncodeOperand(bu::kOperandC, reg1->id());
  auto n = new (zone()) BytecodeNode(Bytecode::kLoadConstant, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Comment(const char* comment) {
#ifdef DEBUG
  auto n = new (zone())
      BytecodeNode(Bytecode::kComment, reinterpret_cast<uintptr_t>(comment));
  bytecode_array_writer_->Emit(n);
  return n;
#endif
  return nullptr;
}

BytecodeNode* BytecodeBuilder::I8Constant(int8_t v, Var* reg1) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  auto operand = bu::EncodeOperand(bu::kOperandA, v);
  operand |= bu::EncodeOperand(bu::kOperandB, reg1->id());
  auto n = new (zone()) BytecodeNode(Bytecode::kI8Constant, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::I32Constant(int32_t t, Var* reg1) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  auto operand = bu::EncodeOperand(bu::kOperandEax, t);
  operand |= bu::EncodeOperand(bu::kOperandE, reg1->id());
  auto n = new (zone()) BytecodeNode(Bytecode::kI32Constant, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Append(Var* var_value, Var* reg1) {
  if (!var_value->has_id()) {
    var_value->set_id(allocate_regiseter(var_value->mode()));
  }
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  auto operand = bu::EncodeOperand(bu::kOperandA, var_value->id());
  operand |= bu::EncodeOperand(bu::kOperandB, reg1->id());
  auto n = new (zone()) BytecodeNode(Bytecode::kAppend, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::CallFastPropertyA(FastProperty t, Var* input,
                                                 Var* out) {
  if (!input->has_id()) {
    input->set_id(allocate_regiseter(input->mode()));
  }
  if (!out->has_id()) {
    out->set_id(allocate_regiseter(out->mode()));
  }
  auto operand = bu::EncodeOperand(bu::kOperandA, static_cast<uint8_t>(t));
  operand |= bu::EncodeOperand(bu::kOperandB, input->id());
  operand |= bu::EncodeOperand(bu::kOperandC, out->id());
  auto n = new (zone()) BytecodeNode(Bytecode::kCallFastPropertyA, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::LoadIx(Var* var_input, Var* var_index,
                                      Var* var_output) {
  if (!var_input->has_id()) {
    var_input->set_id(allocate_regiseter(var_input->mode()));
  }
  if (!var_index->has_id()) {
    var_index->set_id(allocate_regiseter(var_index->mode()));
  }
  if (!var_output->has_id()) {
    var_output->set_id(allocate_regiseter(var_output->mode()));
  }

  auto operand = bu::EncodeOperand(bu::kOperandA, var_input->id());
  operand |= bu::EncodeOperand(bu::kOperandB, var_index->id());
  operand |= bu::EncodeOperand(bu::kOperandC, var_output->id());
  auto n = new (zone()) BytecodeNode(Bytecode::kLoadIx, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Mov(Var* reg1, Var* reg2) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->has_id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }

  auto operand = bu::EncodeOperand(bu::kOperandA, reg1->id());
  operand |= bu::EncodeOperand(bu::kOperandB, reg2->id());
  auto n = new (zone()) BytecodeNode(Bytecode::kMov, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Cmp(Var* reg1, Var* reg2) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->has_id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }

  auto operand = bu::EncodeOperand(bu::kOperandA, reg1->id());
  operand |= bu::EncodeOperand(bu::kOperandB, reg2->id());
  auto n = new (zone()) BytecodeNode(Bytecode::kCmp, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Gt(Var* reg1, Var* reg2) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->has_id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }

  auto operand = bu::EncodeOperand(bu::kOperandA, reg1->id());
  operand |= bu::EncodeOperand(bu::kOperandB, reg2->id());
  auto n = new (zone()) BytecodeNode(Bytecode::kGt, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::GtEq(Var* reg1, Var* reg2) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->has_id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }

  auto operand = bu::EncodeOperand(bu::kOperandA, reg1->id());
  operand |= bu::EncodeOperand(bu::kOperandB, reg2->id());
  auto n = new (zone()) BytecodeNode(Bytecode::kGtEq, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Jmp(BytecodeLabel* label) {
  auto n = new (zone()) BytecodeNode(Bytecode::kJmp, label->to());
  label->AddFrom(n);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::JmpIfTrue(BytecodeLabel* label) {
  auto n = new (zone()) BytecodeNode(Bytecode::kJmpIfTrue, label->to());
  label->AddFrom(n);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::JmpIfFalse(BytecodeLabel* label) {
  auto n = new (zone()) BytecodeNode(Bytecode::kJmpIfFalse, label->to());
  label->AddFrom(n);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Print(Var* reg1) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  auto operand = bu::EncodeOperand(bu::kOperandA, reg1->id());
  auto n = new (zone()) BytecodeNode(Bytecode::kPrint, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Inc(Var* reg1) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  auto operand = bu::EncodeOperand(bu::kOperandA, reg1->id());
  auto n = new (zone()) BytecodeNode(Bytecode::kInc, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Dec(Var* reg1) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }

  auto operand = bu::EncodeOperand(bu::kOperandA, reg1->id());
  auto n = new (zone()) BytecodeNode(Bytecode::kDec, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Add(Var* reg1, Var* reg2) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->has_id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }

  auto operand = bu::EncodeOperand(bu::kOperandA, reg1->id());
  operand |= bu::EncodeOperand(bu::kOperandB, reg2->id());
  auto n = new (zone()) BytecodeNode(Bytecode::kAdd, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Sub(Var* reg1, Var* reg2) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->has_id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }

  auto operand = bu::EncodeOperand(bu::kOperandA, reg1->id());
  operand |= bu::EncodeOperand(bu::kOperandB, reg2->id());
  auto n = new (zone()) BytecodeNode(Bytecode::kSub, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Mul(Var* reg1, Var* reg2) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->has_id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }

  auto operand = bu::EncodeOperand(bu::kOperandA, reg1->id());
  operand |= bu::EncodeOperand(bu::kOperandB, reg2->id());
  auto n = new (zone()) BytecodeNode(Bytecode::kMul, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Div(Var* reg1, Var* reg2) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->has_id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }

  auto operand = bu::EncodeOperand(bu::kOperandA, reg1->id());
  operand |= bu::EncodeOperand(bu::kOperandB, reg2->id());
  auto n = new (zone()) BytecodeNode(Bytecode::kDiv, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

void BytecodeBuilder::Branch(BytecodeLabel* then_jmp, BytecodeLabel* else_jmp) {
  JmpIfTrue(then_jmp);
  JmpIfFalse(else_jmp);
}

uint32_t BytecodeBuilder::StringConstant(JSString* str, int32_t index) {
  auto bcn = new (zone()) BytecodeConstantNode(str);
  return bytecode_array_writer_->EmitConstant(bcn);
}

BytecodeScope::BytecodeScope(BytecodeBuilder* builder) : builder_(builder) {
  start_ = builder->last_bytecode();
}

BytecodeNode* BytecodeScope::end() { return builder_->last_bytecode(); }
}  // namespace lux
