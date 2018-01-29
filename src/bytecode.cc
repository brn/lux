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

const size_t BytecodeConstantPool::kDefaultPoolSize;

BytecodeConstantPool::BytecodeConstantPool(Isolate* isolate)
    : size_(kDefaultPoolSize), used_(0),
      isolate_(isolate) {
  constant_pool_ = Allocate(kDefaultPoolSize);
}

uint32_t BytecodeConstantPool::Register(Address value, int32_t index) {
  if (used_ >= kPointerSize) {
    Grow();
  }

  if (index >= 0) {
    constant_pool_[index] = value;
    auto use = kPointerSize * index;
    if (used_ < use) {
      used_ += use;
    }
  } else {
    *constant_pool_ = value;
    constant_pool_ += kPointerSize;
    used_ += kPointerSize;
  }

  return (used_ / kPointerSize) - 1;
}

void BytecodeConstantPool::Grow() {
  auto next_size = size_ * 2;
  auto new_pool = Allocate(next_size);
  memcpy(new_pool, constant_pool_, size_);
  size_ = next_size;
  constant_pool_ = new_pool;
}

Address* BytecodeConstantPool::Allocate(size_t size) {
  return reinterpret_cast<Address*>(
      isolate_->heap()->Allocate(size));
}

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
  current_offset_++;
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

BytecodeArray BytecodeArrayWriter::Flush() {
  auto array = reinterpret_cast<Bytecode*>(
      isolate_->heap()->Allocate(current_offset_ * sizeof(Bytecode)));
  size_t index = 0;
  auto node = bytecode_top_;
  while (node) {
    auto bytecode = node->bytecode();
    node->set_offset(index);
    array[index++] = bytecode;
    node = node->next();
  }
  for (auto &from : jmps_) {
    auto to = from->jmp();
    BytecodeUpdater updater(from->bytecode());
    updater.UpdateSJ(to->offset() - from->offset());
    array[from->offset()] = updater.bytecode();
  }
  return BytecodeArray(array, index);
}

BytecodeNode* BytecodeBuilder::Return() {
  auto b = BytecodeFactory<BytecodeFormat::kOpAx>::New(
      Bytecode::kReturn, 0);
  auto n = new(zone()) BytecodeNode(b);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::ConstantA(uint16_t index) {
  auto b = BytecodeFactory<BytecodeFormat::kOpAx>::New(
      Bytecode::kConstantA, index);
  auto n = new(zone()) BytecodeNode(b);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::ConstantR(uint16_t index, RegisterRef* reg1) {
  if (!reg1->id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  auto b = BytecodeFactory<BytecodeFormat::kOpABx>::New(
      Bytecode::kConstantR, reg1->id(), index);
  auto n = new(zone()) BytecodeNode(b);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::ImmI8(int8_t v, RegisterRef* reg1) {
  if (!reg1->id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  auto b = BytecodeFactory<BytecodeFormat::kOpABx>::New(
      Bytecode::kImmI8, v, reg1->id());
  auto n = new(zone()) BytecodeNode(b);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::ImmI32(int32_t t, RegisterRef* reg1) {
  if (!reg1->id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  auto b = BytecodeFactory<BytecodeFormat::kOpABx>::New(
      Bytecode::kImmI32, t, reg1->id());
  auto n = new(zone()) BytecodeNode(b);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::StoreAR(RegisterRef* reg1) {
  if (!reg1->id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  auto b = BytecodeFactory<BytecodeFormat::kOpABx>::New(
      Bytecode::kStoreAR, reg1->id(), 0);
  auto n = new(zone()) BytecodeNode(b);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::LoadRA(RegisterRef* reg1) {
  if (!reg1->id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  auto b = BytecodeFactory<BytecodeFormat::kOpABx>::New(
      Bytecode::kLoadRA, reg1->id(), 0);
  auto n = new(zone()) BytecodeNode(b);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::LoadAIxR(RegisterRef* reg1, RegisterRef* reg2) {
  if (!reg1->id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }
  auto b = BytecodeFactory<BytecodeFormat::kOpABx>::New(
      Bytecode::kLoadAIxR, reg1->id(), reg2->id());
  auto n = new(zone()) BytecodeNode(b);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::LoadRIxR(RegisterRef* acc_reg,
                                        RegisterRef* index_reg,
                                        RegisterRef* reg) {
  if (!acc_reg->id()) {
    acc_reg->set_id(allocate_regiseter(acc_reg->mode()));
  }
  if (!index_reg->id()) {
    index_reg->set_id(allocate_regiseter(index_reg->mode()));
  }
  if (!reg->id()) {
    reg->set_id(allocate_regiseter(reg->mode()));
  }
  auto b = BytecodeFactory<BytecodeFormat::kOpABCk>::New(
      Bytecode::kLoadRIxR, acc_reg->id(), index_reg->id(), reg->id(), 0);
  auto n = new(zone()) BytecodeNode(b);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::ICmpAR(RegisterRef* reg) {
  if (!reg->id()) {
    reg->set_id(allocate_regiseter(reg->mode()));
  }
  auto b = BytecodeFactory<BytecodeFormat::kOpABx>::New(
      Bytecode::kICmpAR, reg->id(), 0);
  auto n = new(zone()) BytecodeNode(b);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::ICmpRR(RegisterRef* reg1, RegisterRef* reg2) {
  if (!reg1->id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }
  auto b = BytecodeFactory<BytecodeFormat::kOpABx>::New(
      Bytecode::kICmpRR, reg1->id(), reg2->id());
  auto n = new(zone()) BytecodeNode(b);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::ICmpGTRRA(RegisterRef* reg1, RegisterRef* reg2) {
  if (!reg1->id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }
  auto b = BytecodeFactory<BytecodeFormat::kOpABx>::New(
      Bytecode::kICmpGTRRA, reg1->id(), reg2->id());
  auto n = new(zone()) BytecodeNode(b);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::ICmpLTRRA(RegisterRef* reg1, RegisterRef* reg2) {
  if (!reg1->id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }
  auto b = BytecodeFactory<BytecodeFormat::kOpABx>::New(
      Bytecode::kICmpLTRRA, reg1->id(), reg2->id());
  auto n = new(zone()) BytecodeNode(b);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Jmp(BytecodeLabel* label) {
  auto b = BytecodeFactory<BytecodeFormat::kOpsJk>::New(
      Bytecode::kJmp, 0, 0);
  auto n = new(zone()) BytecodeNode(b, label->to());
  label->AddFrom(n);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::JmpIfTrue(BytecodeLabel* label) {
  auto b = BytecodeFactory<BytecodeFormat::kOpsJk>::New(
      Bytecode::kJmpIfTrue, 0, 0);
  auto n = new(zone()) BytecodeNode(b, label->to());
  label->AddFrom(n);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::JmpIfFalse(BytecodeLabel* label) {
  auto b = BytecodeFactory<BytecodeFormat::kOpsJk>::New(
      Bytecode::kJmpIfFalse, 0, 0);
  auto n = new(zone()) BytecodeNode(b, label->to());
  label->AddFrom(n);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Inc(RegisterRef* reg1) {
  if (!reg1->id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  auto b = BytecodeFactory<BytecodeFormat::kOpABx>::New(
      Bytecode::kInc, reg1->id(), 0);
  auto n = new(zone()) BytecodeNode(b);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Dec(RegisterRef* reg1) {
  if (!reg1->id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  auto b = BytecodeFactory<BytecodeFormat::kOpABx>::New(
      Bytecode::kDec, reg1->id(), 0);
  auto n = new(zone()) BytecodeNode(b);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Add(RegisterRef* reg1, RegisterRef* reg2) {
  if (!reg1->id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }
  auto b = BytecodeFactory<BytecodeFormat::kOpABx>::New(
      Bytecode::kAdd, reg1->id(), reg2->id());
  auto n = new(zone()) BytecodeNode(b);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Sub(RegisterRef* reg1, RegisterRef* reg2) {
  if (!reg1->id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }
  auto b = BytecodeFactory<BytecodeFormat::kOpABx>::New(
      Bytecode::kSub, reg1->id(), reg2->id());
  auto n = new(zone()) BytecodeNode(b);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Mul(RegisterRef* reg1, RegisterRef* reg2) {
  if (!reg1->id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }
  auto b = BytecodeFactory<BytecodeFormat::kOpABx>::New(
      Bytecode::kMul, reg1->id(), reg2->id());
  auto n = new(zone()) BytecodeNode(b);
  bytecode_array_writer_->Emit(n);
  return n;
}

BytecodeNode* BytecodeBuilder::Div(RegisterRef* reg1, RegisterRef* reg2) {
  if (!reg1->id()) {
    reg1->set_id(allocate_regiseter(reg1->mode()));
  }
  if (!reg2->id()) {
    reg2->set_id(allocate_regiseter(reg2->mode()));
  }
  auto b = BytecodeFactory<BytecodeFormat::kOpABx>::New(
      Bytecode::kDiv, reg1->id(), reg2->id());
  auto n = new(zone()) BytecodeNode(b);
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

uint32_t BytecodeBuilder::StringConstant(Utf16String str, int32_t index) {
  auto data = str.data();
  return constant_pool_->Register(
      reinterpret_cast<Address>(reinterpret_cast<intptr_t>(
          data)), index);
}

BytecodeScope::BytecodeScope(BytecodeBuilder* builder)
    : builder_(builder) {
  start_ = builder->last_bytecode();
}

BytecodeNode* BytecodeScope::end() {
  return builder_->last_bytecode();
}

}  // namespace lux
