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


#include "./vm.h"
#include "./objects/object.h"
#include "./isolate.h"
#include "./utils.h"

namespace lux {

using VM = VirtualMachine;
#define VM_OP(Name, ...) exec->Name(__VA_ARGS__)


#define HANDLER(Name)                                   \
  struct Name##BytecodeHandler {                        \
    inline void Execute(                                \
        Isolate* isolate,                               \
        VM::Executor* exec,                             \
        Bytecode bc,                                    \
        BytecodeFetcher* fetcher,                       \
        BytecodeConstantArray* constant);               \
  };                                                    \
  void Name##BytecodeHandler::Execute(                  \
      Isolate* isolate,                                 \
      VM::Executor* exec,                               \
      Bytecode bytecode,                                \
      BytecodeFetcher* fetcher,                         \
      BytecodeConstantArray* constant)

HANDLER(Comment) {
  USE(fetcher->FetchNextWordOperand());
  return;
}

HANDLER(Jmp) {
  auto next = fetcher->FetchNextWideOperand();
  fetcher->UpdatePC(next);
}

HANDLER(JmpIfTrue) {
  auto v = exec->load_accumulator();
  auto boolean_value = JSSpecials::ToBoolean(v);
  auto next = fetcher->FetchNextWideOperand();
  if (boolean_value) {
    fetcher->UpdatePC(next);
  }
}

HANDLER(JmpIfFalse) {
  auto v = exec->load_accumulator();
  auto boolean_value = JSSpecials::ToBoolean(v);
  auto next = fetcher->FetchNextWideOperand();
  if (!boolean_value) {
    fetcher->UpdatePC(next);
  }
}

HANDLER(ImmI8) {
  auto value = fetcher->FetchNextShortOperand();
  auto reg = fetcher->FetchNextShortOperand();
  exec->store_register_value_at(reg, Smi::FromInt(value));
}

HANDLER(ImmI32) {
  auto value = fetcher->FetchNextWideOperand();
  auto reg = fetcher->FetchNextShortOperand();
  exec->store_register_value_at(reg, Smi::FromInt(value));
}

HANDLER(CallFastPropertyA) {
  auto property = fetcher->FetchNextShortOperand();
  auto obj = exec->load_accumulator();
  switch (static_cast<FastProperty>(property)) {
    case FastProperty::kLength:
      exec->store_accumulator(JSObject::GetLength(isolate, obj));
  }
}

HANDLER(ConstantA) {
  auto index = fetcher->FetchNextDoubleOperand();
  auto obj = constant->at(index);
  exec->store_accumulator(obj);
}

HANDLER(ConstantR) {
  auto index = fetcher->FetchNextDoubleOperand();
  auto reg = fetcher->FetchNextShortOperand();
  auto obj = constant->at(index);
  exec->store_register_value_at(reg, obj);
}

HANDLER(StoreAR) {
  auto reg = fetcher->FetchNextShortOperand();
  auto obj = VM_OP(load_accumulator);
  exec->store_register_value_at(reg, obj);
}

HANDLER(LoadRA) {
  auto reg = fetcher->FetchNextShortOperand();
  auto obj = exec->load_register_value_at(reg);
  exec->store_accumulator(obj);
}

HANDLER(LoadAIxR) {
  auto reg1 = fetcher->FetchNextShortOperand();
  auto reg2 = fetcher->FetchNextShortOperand();
  auto position = exec->load_register_value_at(reg1);
  auto obj = exec->load_accumulator();
  auto o = Object::Cast(obj);
  if (o->IsHeapObject()) {
    auto v = HeapObject::Cast(o);
    switch (v->shape()->instance_type()) {
      case InstanceType::JS_STRING: {
        auto ret = JSString::Cast(v)->at(Smi::Cast(position)->value());
        exec->store_register_value_at(reg2, Smi::FromInt(ret.code()));
        break;
      }
      default:
        return;
    }
  }
}

HANDLER(LoadRIxR) {
  auto reg1 = fetcher->FetchNextShortOperand();
  auto index = fetcher->FetchNextShortOperand();
  auto reg2 = fetcher->FetchNextShortOperand();
  auto obj = exec->load_register_value_at(reg1);

  if (obj->IsHeapObject()) {
    auto v = HeapObject::Cast(obj);
    switch (v->shape()->instance_type()) {
      case InstanceType::JS_STRING: {
        auto ret = JSString::Cast(v)->at(index);
        exec->store_register_value_at(reg2, Smi::FromInt(ret));
        break;
      }
      default:
        return;
    }
  }
}

HANDLER(ICmpRR) {
  auto reg1 = fetcher->FetchNextShortOperand();
  auto reg2 = fetcher->FetchNextShortOperand();
  auto valueA = exec->load_register_value_at(reg1);
  auto valueB = exec->load_register_value_at(reg2);
  auto int_a = Smi::Cast(valueA);
  auto int_b = Smi::Cast(valueB);
  if (int_a->Equals(int_b)) {
    exec->store_accumulator(Smi::FromInt(1));
  } else {
    exec->store_accumulator(Smi::FromInt(0));
  }
}

HANDLER(ICmpAR) {
  auto reg1 = fetcher->FetchNextShortOperand();
  auto valueA = exec->load_register_value_at(reg1);
  auto valueB = exec->load_accumulator();
  auto int_a = Smi::Cast(valueA);
  auto int_b = Smi::Cast(valueB);
  if (int_a->Equals(int_b)) {
    exec->store_accumulator(Smi::FromInt(1));
  } else {
    exec->store_accumulator(Smi::FromInt(0));
  }
}

HANDLER(ICmpGTRRA) {
  auto reg1 = fetcher->FetchNextShortOperand();
  auto reg2 = fetcher->FetchNextShortOperand();
  auto valueA = exec->load_register_value_at(reg1);
  auto valueB = exec->load_register_value_at(reg2);
  exec->store_accumulator(Smi::FromInt(valueA > valueB));
}

HANDLER(ICmpLTRRA) {
  auto reg1 = fetcher->FetchNextShortOperand();
  auto reg2 = fetcher->FetchNextShortOperand();
  auto valueA = exec->load_register_value_at(reg1);
  auto valueB = exec->load_register_value_at(reg2);
  exec->store_accumulator(Smi::FromInt(valueA < valueB));
}

HANDLER(Inc) {
  auto reg1 = fetcher->FetchNextShortOperand();
  auto value = exec->load_register_value_at(reg1);
  exec->store_register_value_at(
      reg1, Smi::FromInt(Smi::Cast(value)->value() + 1));
}

HANDLER(Dec) {
  auto reg1 = fetcher->FetchNextShortOperand();
  auto value = exec->load_register_value_at(reg1);
  exec->store_register_value_at(
      reg1, Smi::FromInt(Smi::Cast(value)->value() - 1));
}

HANDLER(Add) {
  auto reg1 = fetcher->FetchNextShortOperand();
  auto add = fetcher->FetchNextShortOperand();
  auto value = exec->load_register_value_at(reg1);
  if (value->IsSmi()) {
    exec->store_accumulator(
        Smi::FromInt(Smi::Cast(value)->value() + add));
  }
}

HANDLER(Sub) {
  auto reg1 = fetcher->FetchNextShortOperand();
  auto sub = fetcher->FetchNextShortOperand();
  auto value = exec->load_register_value_at(reg1);
  exec->store_accumulator(Smi::FromInt(
      Smi::Cast(value)->value() - sub));
}

HANDLER(Mul) {
  auto reg1 = fetcher->FetchNextShortOperand();
  auto mul = fetcher->FetchNextShortOperand();
  auto value = exec->load_register_value_at(reg1);
  exec->store_accumulator(Smi::FromInt(
      Smi::Cast(value)->value() * mul));
}

HANDLER(Div) {
  auto reg1 = fetcher->FetchNextShortOperand();
  auto div = fetcher->FetchNextShortOperand();
  auto value = exec->load_register_value_at(reg1);
  exec->store_accumulator(Smi::FromInt(
      Smi::Cast(value)->value() * div));
}

Object* VirtualMachine::Execute(BytecodeExecutable* bytecode_executable,
                                std::initializer_list<Object*> argv) {
  Executor exec(isolate_, argv.size(), argv, bytecode_executable);
  exec.Execute();
  return exec.return_value();
}

void VirtualMachine::Executor::Execute() {
#define VM_STATIC_HANDLER(Name, Layout, size, n, ...)        \
  static Name##BytecodeHandler Name##_bytecode_handler;
  BYTECODE_LIST_WITHOUT_RETURN(VM_STATIC_HANDLER)
#undef VM_STATIC_HANDLER

  static const std::array<
    void*,
    static_cast<uint8_t>(Bytecode::kExit) + 1> kDispatchTable = {{
#define VM_JMP_TABLES(Name, Layout, size, n, ...) &&Label_##Name,
      BYTECODE_LIST(VM_JMP_TABLES)
#undef VM_JMP_TABLES
      &&Label_Exit,
      &&Label_Exit,
    }};

  auto bc = fetcher_->FetchBytecodeAsInt();
  goto *kDispatchTable[bc];
#define VM_EXECUTE(Name, Layout, size, n, ...)        \
  Label_##Name: {                                     \
    Name##_bytecode_handler.Execute(                  \
        isolate_,                                     \
        this,                                         \
        static_cast<Bytecode>(bc),                    \
        fetcher_.Get(),                               \
        constant_pool_);                              \
    auto next = fetcher_->FetchBytecodeAsInt();       \
    goto *kDispatchTable[next];                       \
  }
  BYTECODE_LIST_WITHOUT_RETURN(VM_EXECUTE)
#undef VM_EXECUTE
  Label_Return: {
    return;
    //  DO NOTHING
  }
  Label_Exit: {
    return;
    //  DO NOTHING
  }
}
}  // namespace lux
