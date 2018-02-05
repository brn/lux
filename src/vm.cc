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
#define DISPATCH() exec->Dispatch()
#define JMP(jmp) exec->Jmp(jmp)


#define HANDLER(Name)                                   \
  struct Name##BytecodeHandler {                        \
    inline void Execute(                                \
        Isolate* isolate,                               \
        VM::Executor* exec,                             \
        Bytecode bc,                                    \
        BytecodeConstantArray* constant);               \
  };                                                    \
  void Name##BytecodeHandler::Execute(                  \
      Isolate* isolate,                                 \
      VM::Executor* exec,                               \
      Bytecode bytecode,                                \
      BytecodeConstantArray* constant)

HANDLER(Comment) {
  DISPATCH();
}

HANDLER(Jmp) {
  auto next = bytecode.sJ();
  JMP(next);
}

HANDLER(JmpIfTrue) {
  auto v = exec->load_accumulator();
  auto boolean_value = JSSpecials::ToBoolean(v);
  if (boolean_value) {
    auto next = bytecode.sJ();
    return JMP(next);
  }
  DISPATCH();
}

HANDLER(JmpIfFalse) {
  auto v = exec->load_accumulator();
  auto boolean_value = JSSpecials::ToBoolean(v);
  if (!boolean_value) {
    auto next = bytecode.sJ();
    return JMP(next);
  }
  DISPATCH();
}

HANDLER(Return) {
  return;
}

HANDLER(ImmI8) {
  auto reg = bytecode.A();
  auto value = bytecode.Bx();
  exec->store_register_value_at(reg, Smi::FromInt(value));
  DISPATCH();
}

HANDLER(ImmI32) {
  auto reg = bytecode.A();
  auto value = bytecode.Bx();
  exec->store_register_value_at(reg, Smi::FromInt(value));
  DISPATCH();
}

HANDLER(CallFastPropertyA) {
  auto property = bytecode.A();
  auto obj = exec->load_accumulator();
  switch (static_cast<FastProperty>(property)) {
    case FastProperty::kLength:
      exec->store_accumulator(JSObject::GetLength(isolate, obj));
  }
  DISPATCH();
}

HANDLER(ConstantA) {
  auto index = bytecode.A();
  auto obj = constant->at(index);
  exec->store_accumulator(obj);
  DISPATCH();
}

HANDLER(ConstantR) {
  auto index = bytecode.A();
  auto reg = bytecode.Bx();
  auto obj = constant->at(index);
  exec->store_register_value_at(reg, obj);
  DISPATCH();
}

HANDLER(StoreAR) {
  auto reg = bytecode.A();
  auto obj = VM_OP(load_accumulator);
  exec->store_register_value_at(reg, obj);
  DISPATCH();
}

HANDLER(LoadRA) {
  auto reg = bytecode.A();
  auto obj = exec->load_register_value_at(reg);
  exec->store_accumulator(obj);
  DISPATCH();
}

HANDLER(LoadAIxR) {
  auto reg1 = bytecode.A();
  auto reg2 = bytecode.Bx();
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
        DISPATCH();
    }
  }

  DISPATCH();
}

HANDLER(LoadRIxR) {
  auto reg1 = bytecode.A();
  auto index = bytecode.B();
  auto reg2 = bytecode.B();
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
        DISPATCH();
    }
  }

  DISPATCH();
}

HANDLER(ICmpRR) {
  auto reg1 = bytecode.A();
  auto reg2 = bytecode.Bx();
  auto valueA = exec->load_register_value_at(reg1);
  auto valueB = exec->load_register_value_at(reg2);
  auto int_a = Smi::Cast(valueA);
  auto int_b = Smi::Cast(valueB);
  if (int_a->Equals(int_b)) {
    exec->store_accumulator(Smi::FromInt(1));
  } else {
    exec->store_accumulator(Smi::FromInt(0));
  }
  DISPATCH();
}

HANDLER(ICmpAR) {
  auto reg1 = bytecode.A();
  auto valueA = exec->load_register_value_at(reg1);
  auto valueB = exec->load_accumulator();
  auto int_a = Smi::Cast(valueA);
  auto int_b = Smi::Cast(valueB);
  if (int_a->Equals(int_b)) {
    exec->store_accumulator(Smi::FromInt(1));
  } else {
    exec->store_accumulator(Smi::FromInt(0));
  }
  DISPATCH();
}

HANDLER(ICmpGTRRA) {
  auto reg1 = bytecode.A();
  auto reg2 = bytecode.Bx();
  auto valueA = exec->load_register_value_at(reg1);
  auto valueB = exec->load_register_value_at(reg2);
  exec->store_accumulator(Smi::FromInt(valueA > valueB));
  DISPATCH();
}

HANDLER(ICmpLTRRA) {
  auto reg1 = bytecode.A();
  auto reg2 = bytecode.Bx();
  auto valueA = exec->load_register_value_at(reg1);
  auto valueB = exec->load_register_value_at(reg2);
  exec->store_accumulator(Smi::FromInt(valueA < valueB));
  DISPATCH();
}

HANDLER(Inc) {
  auto reg1 = bytecode.A();
  auto value = exec->load_register_value_at(reg1);
  exec->store_register_value_at(
      reg1, Smi::FromInt(Smi::Cast(value)->value() + 1));
  DISPATCH();
}

HANDLER(Dec) {
  auto reg1 = bytecode.A();
  auto value = exec->load_register_value_at(reg1);
  exec->store_register_value_at(
      reg1, Smi::FromInt(Smi::Cast(value)->value() - 1));
  DISPATCH();
}

HANDLER(Add) {
  auto reg1 = bytecode.A();
  auto add = bytecode.Bx();
  auto value = exec->load_register_value_at(reg1);
  if (value->IsSmi()) {
    exec->store_accumulator(
        Smi::FromInt(Smi::Cast(value)->value() + add));
  }
  DISPATCH();
}

HANDLER(Sub) {
  auto reg1 = bytecode.A();
  auto sub = bytecode.Bx();
  auto value = exec->load_register_value_at(reg1);
  exec->store_accumulator(Smi::FromInt(
      Smi::Cast(value)->value() - sub));
  DISPATCH();
}

HANDLER(Mul) {
  auto reg1 = bytecode.A();
  auto mul = bytecode.Bx();
  auto value = exec->load_register_value_at(reg1);
  exec->store_accumulator(Smi::FromInt(
      Smi::Cast(value)->value() * mul));
  DISPATCH();
}

HANDLER(Div) {
  auto reg1 = bytecode.A();
  auto div = bytecode.Bx();
  auto value = exec->load_register_value_at(reg1);
  exec->store_accumulator(Smi::FromInt(
      Smi::Cast(value)->value() * div));
  DISPATCH();
}

Object* VirtualMachine::Execute(BytecodeExecutable* bytecode_executable,
                                std::initializer_list<Object*> argv) {
  Executor exec(isolate_, argv.size(), argv, bytecode_executable);
  exec.Dispatch();
  return exec.return_value();
}

void VirtualMachine::Executor::Dispatch() {
#define VM_STATIC_HANDLER(n, Name, i, ...)                \
  static Name##BytecodeHandler Name##_bytecode_handler;
  BYTECODE_LIST(VM_STATIC_HANDLER)
#undef VM_STATIC_HANDLER

  auto pc = pc_++;
  if (bytecode_array_->length() <= pc) {
    return;
  }

  auto bc = bytecode_array_->at(pc);
  auto next = bc.instruction();
  switch (next) {
#define DEF_BJ(i, Name, n, ...)                                   \
    case Bytecode::k##Name: {                                     \
      Name##_bytecode_handler.Execute(isolate_,                   \
                                       this, bc, constant_pool_); \
      break;                                                      \
    }
    BYTECODE_LIST(DEF_BJ)
#undef DEF_BJ
  }
}
}  // namespace lux
