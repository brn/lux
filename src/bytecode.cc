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
#include "./isolate.h"
#include "./heap.h"

namespace lux {

using bu = BytecodeUtil;

const BytecodeOperand BytecodeUtil::kOperandA = { 0, 0xFF };
const BytecodeOperand BytecodeUtil::kOperandB = { 8, 0xFF };
const BytecodeOperand BytecodeUtil::kOperandC = { 16, 0xFF };
const BytecodeOperand BytecodeUtil::kOperandD = { 24, 0xFF };
const BytecodeOperand BytecodeUtil::kOperandE = { 32, 0xFF };
const BytecodeOperand BytecodeUtil::kOperandF = { 40, 0xFF };
const BytecodeOperand BytecodeUtil::kOperandG = { 48, 0xFF };
const BytecodeOperand BytecodeUtil::kOperandH = {
    56, ~static_cast<uint64_t>(0) };

const BytecodeOperand BytecodeUtil::kOperandAx = { 0, 0xFFFF };
const BytecodeOperand BytecodeUtil::kOperandBx = { 16, 0xFFFF };
const BytecodeOperand BytecodeUtil::kOperandCx = { 32, 0xFFFF };
const BytecodeOperand BytecodeUtil::kOperandDx = {
    48, ~static_cast<uint64_t>(0) };

const BytecodeOperand BytecodeUtil::kOperandEax = { 0, 0xFFFFFFFF };
const BytecodeOperand BytecodeUtil::kOperandEbx = {
    32, ~static_cast<uint64_t>(0) };

const BytecodeOperand BytecodeUtil::kOperandRax = {
    0,  ~static_cast<uint64_t>(0) };

const std::array<
  BytecodeLayout,
  static_cast<uint8_t>(Bytecode::kLastSentinel__)> bu::kLayout = {{
#define BYTECODE_LAYOUT_DEF(Name, Layout, size, num, ...) \
    BytecodeLayout::k##Layout,
    BYTECODE_LIST(BYTECODE_LAYOUT_DEF)
#undef BYTECODE_LAYOUT_DEF
  }};

const std::array<
  uint8_t,
  static_cast<uint8_t>(Bytecode::kLastSentinel__)> bu::kSize = {{
#define BYTECODE_LAYOUT_DEF(Name, Layout, size, num, ...) \
    static_cast<uint8_t>(size),
    BYTECODE_LIST(BYTECODE_LAYOUT_DEF)
#undef BYTECODE_LAYOUT_DEF
  }};

#ifdef DEBUG
std::string bu::ToString(BytecodeFetcher* fetcher) {
  std::stringstream st;
  auto bc = fetcher->FetchBytecode();
  if (bc == Bytecode::kComment) {
    st << ";;" << fetcher->FetchNextPtrOperand<const char*>();
    return st.str();
  } else if (bc == Bytecode::kExit) {
    return "Exit";
  }

  switch (bc) {
#define BYTECODE_CASE(Name, Layout, size, n, ...)     \
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

std::string BytecodeArray::ToString(BytecodeConstantArray* constant_pool) {
  BytecodeFetcher fetcher(this);
  std::stringstream st;
  auto max = std::to_string(length()).size();
  for (auto i = 0; fetcher.HasMore(); i++) {
    auto size = std::to_string(i).size();
    auto start = fetcher.pc();
    auto start_size = std::to_string(start).size();
    auto detail = BytecodeUtil::ToString(&fetcher);;
    auto end = fetcher.pc() - 1;
    auto end_size = std::to_string(end).size();
    std::string indent((max - size) + 1, ' ');
    std::string indent2((max - start_size) + 1, ' ');
    std::string indent3((max - end_size) + 1, ' ');
    st << (i > 0? "\n": "") << start << indent2 << "- " << end << indent3
       << indent <<  i << ": "
       << detail;
  }
  return st.str();
}

std::string BytecodeUtil::ToStringField(Bytecode bc,
                                        BytecodeFetcher* fetcher,
                                        size_t len) {
  std::stringstream st;
  std::string indent(19 - len, ' ');
  BytecodeLayout l = layout(bc);
  st << indent;
  switch (l) {
    case BytecodeLayout::kNone: {
      return "";
    }
    case BytecodeLayout::kShort1: {
      st << "  " << "A = "
         << +fetcher->FetchNextShortOperand();
      return st.str();
    }
    case BytecodeLayout::kShort2: {
      st << "  A = "
         << +fetcher->FetchNextShortOperand()
         << "  B = "
         << +fetcher->FetchNextShortOperand();
      return st.str();
    }
    case BytecodeLayout::kShort3: {
      st << "  A = "
         << +fetcher->FetchNextShortOperand()
         << "  B = "
         << +fetcher->FetchNextShortOperand()
         << "  C = "
         << +fetcher->FetchNextShortOperand();
      return st.str();
    }
    case BytecodeLayout::kDouble1: {
      st << "  Ax = "
         << +fetcher->FetchNextDoubleOperand();
      return st.str();
    }
    case BytecodeLayout::kDouble2: {
      st << "  Ax = "
         << +fetcher->FetchNextDoubleOperand()
         << "  C = "
         << +fetcher->FetchNextDoubleOperand();
      return st.str();
    }
    case BytecodeLayout::kWord1: {
      st << "  Rax = "
         << +fetcher->FetchNextWordOperand();
      return st.str();
    }
    case BytecodeLayout::kWide1: {
      st << "  Eax = "
         << +fetcher->FetchNextWideOperand();
      return st.str();
    }
    case BytecodeLayout::kWide2: {
      st << "  Eax = "
         << +fetcher->FetchNextWideOperand()
         << "  E = "
         << +fetcher->FetchNextShortOperand();
      return st.str();
    }
    default:
      UNREACHABLE();
  }
}
#endif

void BytecodeArrayWriter::Emit(BytecodeNode* bytecode) {
  Append(bytecode);
}

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
  label->set_to(last_bytecode());
  label->set_bound_node(last_bytecode());
  for (auto &from : *label) {
    from->set_jmp(last_bytecode());
    jmps_.push_back(from);
  }
  label->set_jmps(&jmps_);
}

Handle<BytecodeExecutable> BytecodeArrayWriter::Flush() {
  HandleScope scope;
  auto array = BytecodeArray::New(isolate_, current_offset_ + 3);
  size_t index = 0;
  auto node = bytecode_top_;
  while (node) {
    auto bytecode = node->bytecode();
    node->set_offset(index);
    array->write(index++, static_cast<uint8_t>(bytecode));
    switch (bu::layout(bytecode)) {
      case BytecodeLayout::kNone:
        break;
      case BytecodeLayout::kShort1:
        array->write(index++, node->DecodeOperand(bu::kOperandA));
        break;
      case BytecodeLayout::kShort2:
        array->write(index++, node->DecodeOperand(bu::kOperandA));
        array->write(index++, node->DecodeOperand(bu::kOperandB));
        break;
      case BytecodeLayout::kShort3:
        array->write(index++, node->DecodeOperand(bu::kOperandA));
        array->write(index++, node->DecodeOperand(bu::kOperandB));
        array->write(index++, node->DecodeOperand(bu::kOperandC));
        break;
      case BytecodeLayout::kDouble1:
        array->write(index++, node->DecodeOperand(bu::kOperandA));
        array->write(index++, node->DecodeOperand(bu::kOperandB));
        break;
      case BytecodeLayout::kDouble2:
        array->write(index++, node->DecodeOperand(bu::kOperandA));
        array->write(index++, node->DecodeOperand(bu::kOperandB));
        array->write(index++, node->DecodeOperand(bu::kOperandC));
        break;
      case BytecodeLayout::kWord1:
        array->write(index++, node->DecodeOperand(bu::kOperandA));
        array->write(index++, node->DecodeOperand(bu::kOperandB));
        array->write(index++, node->DecodeOperand(bu::kOperandC));
        array->write(index++, node->DecodeOperand(bu::kOperandD));
        array->write(index++, node->DecodeOperand(bu::kOperandE));
        array->write(index++, node->DecodeOperand(bu::kOperandF));
        array->write(index++, node->DecodeOperand(bu::kOperandG));
        array->write(index++, node->DecodeOperand(bu::kOperandH));
        break;
      case BytecodeLayout::kWide1:
        array->write(index++, node->DecodeOperand(bu::kOperandA));
        array->write(index++, node->DecodeOperand(bu::kOperandB));
        array->write(index++, node->DecodeOperand(bu::kOperandC));
        array->write(index++, node->DecodeOperand(bu::kOperandD));
        break;
      case BytecodeLayout::kWide2:
        array->write(index++, node->DecodeOperand(bu::kOperandA));
        array->write(index++, node->DecodeOperand(bu::kOperandB));
        array->write(index++, node->DecodeOperand(bu::kOperandC));
        array->write(index++, node->DecodeOperand(bu::kOperandD));
        array->write(index++, node->DecodeOperand(bu::kOperandE));
        break;
      default:
        UNREACHABLE();
    }
    node = node->next();
  }
  array->write(index, static_cast<uint8_t>(Bytecode::kExit));
  for (auto &from : jmps_) {
    auto to = from->jmp();
    auto target = to->next()? to->next()->offset(): to->offset() + 1;
    array->write(from->offset() + 1,
                 bu::EncodeOperand(bu::kOperandA, target));
    array->write(from->offset() + 2,
                 bu::EncodeOperand(bu::kOperandB, target));
    array->write(from->offset() + 3,
                 bu::EncodeOperand(bu::kOperandC, target));
    array->write(from->offset() + 4,
                 bu::EncodeOperand(bu::kOperandD, target));
  }
  auto pool = BytecodeConstantArray::New(isolate_, constant_length_);
  auto bcn = constant_top_;
  index = 0;
  while (bcn) {
    auto obj = bcn->ptr();
    pool->write(index++, obj);
    bcn = bcn->next();
  }
  return BytecodeExecutable::New(isolate_,
                                 *scope.Return(array),
                                 *scope.Return(pool));
}

BytecodeNode* BytecodeBuilder::Return() {
  auto n = new(zone()) BytecodeNode(Bytecode::kReturn);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::ConstantA(uint16_t index) {
  auto n = new(zone()) BytecodeNode(Bytecode::kConstantA, index);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::ConstantR(uint16_t index, RegisterRef* reg1) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }

  auto operand = bu::EncodeOperand(bu::kOperandAx, index);
  operand |= bu::EncodeOperand(bu::kOperandC, reg1->id());
  auto n = new(zone()) BytecodeNode(Bytecode::kConstantR, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Comment(const char* comment) {
#ifdef DEBUG
  auto n = new(zone()) BytecodeNode(Bytecode::kComment,
                                    reinterpret_cast<uintptr_t>(comment));
  bytecode_array_writer_->Emit(n);
  return n;
#endif
  return nullptr;
}

BytecodeNode* BytecodeBuilder::ImmI8(int8_t v, RegisterRef* reg1) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  auto operand = bu::EncodeOperand(bu::kOperandA, v);
  operand |= bu::EncodeOperand(bu::kOperandB, reg1->id());
  auto n = new(zone()) BytecodeNode(Bytecode::kImmI8, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::ImmI32(int32_t t, RegisterRef* reg1) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  auto operand = bu::EncodeOperand(bu::kOperandEax, t);
  operand |= bu::EncodeOperand(bu::kOperandE, reg1->id());
  auto n = new(zone()) BytecodeNode(Bytecode::kImmI32, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::CallFastPropertyA(FastProperty t) {
  auto operand = bu::EncodeOperand(bu::kOperandA, static_cast<uint8_t>(t));
  auto n = new(zone()) BytecodeNode(Bytecode::kCallFastPropertyA, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::StoreAR(RegisterRef* reg1) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  auto operand = bu::EncodeOperand(bu::kOperandA, reg1->id());
  auto n = new(zone()) BytecodeNode(Bytecode::kStoreAR, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::LoadRA(RegisterRef* reg1) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  auto operand = bu::EncodeOperand(bu::kOperandA, reg1->id());
  auto n = new(zone()) BytecodeNode(Bytecode::kLoadRA, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::LoadAIxR(RegisterRef* reg1, RegisterRef* reg2) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->has_id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }

  auto operand = bu::EncodeOperand(bu::kOperandA, reg1->id());
  operand |= bu::EncodeOperand(bu::kOperandB, reg2->id());
  auto n = new(zone()) BytecodeNode(Bytecode::kLoadAIxR, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::LoadRIxR(RegisterRef* acc_reg,
                                        RegisterRef* index_reg,
                                        RegisterRef* reg) {
  if (!acc_reg->has_id()) {
    acc_reg->set_id(allocate_regiseter(acc_reg->mode()));
  }
  if (!index_reg->has_id()) {
    index_reg->set_id(allocate_regiseter(index_reg->mode()));
  }
  if (!reg->has_id()) {
    reg->set_id(allocate_regiseter(reg->mode()));
  }

  auto operand = bu::EncodeOperand(bu::kOperandA, acc_reg->id());
  operand |= bu::EncodeOperand(bu::kOperandB, index_reg->id());
  operand |= bu::EncodeOperand(bu::kOperandC, reg->id());
  auto n = new(zone()) BytecodeNode(Bytecode::kLoadRIxR, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::ICmpAR(RegisterRef* reg) {
  if (!reg->has_id()) {
    reg->set_id(allocate_regiseter(reg->mode()));
  }
  auto operand = bu::EncodeOperand(bu::kOperandA, reg->id());
  auto n = new(zone()) BytecodeNode(Bytecode::kICmpAR, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::ICmpRR(RegisterRef* reg1, RegisterRef* reg2) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->has_id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }

  auto operand = bu::EncodeOperand(bu::kOperandA, reg1->id());
  operand |= bu::EncodeOperand(bu::kOperandB, reg2->id());
  auto n = new(zone()) BytecodeNode(Bytecode::kICmpRR, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::ICmpGTRRA(RegisterRef* reg1, RegisterRef* reg2) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->has_id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }

  auto operand = bu::EncodeOperand(bu::kOperandA, reg1->id());
  operand |= bu::EncodeOperand(bu::kOperandB, reg2->id());
  auto n = new(zone()) BytecodeNode(Bytecode::kICmpGTRRA, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Jmp(BytecodeLabel* label) {
  auto n = new(zone()) BytecodeNode(Bytecode::kJmp, label->to());
  label->AddFrom(n);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::JmpIfTrue(BytecodeLabel* label) {
  auto n = new(zone()) BytecodeNode(Bytecode::kJmpIfTrue, label->to());
  label->AddFrom(n);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::JmpIfFalse(BytecodeLabel* label) {
  auto n = new(zone()) BytecodeNode(Bytecode::kJmpIfFalse, label->to());
  label->AddFrom(n);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Inc(RegisterRef* reg1) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  auto operand = bu::EncodeOperand(bu::kOperandA, reg1->id());
  auto n = new(zone()) BytecodeNode(Bytecode::kInc, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Dec(RegisterRef* reg1) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }

  auto operand = bu::EncodeOperand(bu::kOperandA, reg1->id());
  auto n = new(zone()) BytecodeNode(Bytecode::kDec, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Add(RegisterRef* reg1, RegisterRef* reg2) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->has_id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }

  auto operand = bu::EncodeOperand(bu::kOperandA, reg1->id());
  operand |= bu::EncodeOperand(bu::kOperandB, reg2->id());
  auto n = new(zone()) BytecodeNode(Bytecode::kAdd, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Sub(RegisterRef* reg1, RegisterRef* reg2) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->has_id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }

  auto operand = bu::EncodeOperand(bu::kOperandA, reg1->id());
  operand |= bu::EncodeOperand(bu::kOperandB, reg2->id());
  auto n = new(zone()) BytecodeNode(Bytecode::kSub, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Mul(RegisterRef* reg1, RegisterRef* reg2) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->has_id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }

  auto operand = bu::EncodeOperand(bu::kOperandA, reg1->id());
  operand |= bu::EncodeOperand(bu::kOperandB, reg2->id());
  auto n = new(zone()) BytecodeNode(Bytecode::kMul, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Div(RegisterRef* reg1, RegisterRef* reg2) {
  if (!reg1->has_id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->has_id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }

  auto operand = bu::EncodeOperand(bu::kOperandA, reg1->id());
  operand |= bu::EncodeOperand(bu::kOperandB, reg2->id());
  auto n = new(zone()) BytecodeNode(Bytecode::kDiv, operand);
  bytecode_array_writer_->Emit(n);
  return n;
}

void BytecodeBuilder::Branch(RegisterRef* reg,
                             BytecodeLabel* then_jmp,
                             BytecodeLabel* else_jmp) {
  RegisterRef r;
  ImmI8(1, &r);
  ICmpRR(reg, &r);
  JmpIfTrue(then_jmp);
  JmpIfFalse(else_jmp);
}

void BytecodeBuilder::BranchA(BytecodeLabel* then_jmp,
                              BytecodeLabel* else_jmp) {
  RegisterRef r_target, r_true;

  StoreAR(&r_target);
  ImmI8(1, &r_true);
  ICmpRR(&r_target, &r_true);
  JmpIfTrue(then_jmp);
  JmpIfFalse(else_jmp);
}

uint32_t BytecodeBuilder::StringConstant(JSString* str, int32_t index) {
  auto bcn = new(zone()) BytecodeConstantNode(str);
  return bytecode_array_writer_->EmitConstant(bcn);
}

BytecodeScope::BytecodeScope(BytecodeBuilder* builder)
    : builder_(builder) {
  start_ = builder->last_bytecode();
}

BytecodeNode* BytecodeScope::end() {
  return builder_->last_bytecode();
}

}  // namespace lux
