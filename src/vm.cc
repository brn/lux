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

#define REGEX_HANDLER(Name)                     \
    struct Name##BytecodeHandler {              \
      inline void Execute(                      \
          Isolate* isolate,                     \
          VM::RegexExecutor* exec,              \
          Bytecode bc,                          \
          BytecodeFetcher* fetcher,             \
          BytecodeConstantArray* constant);     \
    };                                          \
    void Name##BytecodeHandler::Execute(        \
        Isolate* isolate,                       \
        VM::RegexExecutor* exec,                \
        Bytecode bytecode,                      \
        BytecodeFetcher* fetcher,               \
        BytecodeConstantArray* constant)

REGEX_HANDLER(RegexStartGroup) {
  exec->EnterGroup();
  if (exec->collect_matched_word()) {
    exec->PrepareStringBuffer();
  }
}

REGEX_HANDLER(RegexEndGroup) {
  exec->LeaveGroup();
  if (exec->collect_matched_word() &&
      exec->current_matched_string()->length() > 0) {
    exec->matched_array()->Push(isolate,
                                exec->current_matched_string());
  }
}

REGEX_HANDLER(RegexRune) {
  u32 ch = fetcher->FetchNextDoubleOperand();
  auto p = exec->position();
  auto subject = exec->input();
  if (subject->length() > p) {
    auto matched = ch == subject->at(p).code();
    if (matched) {
      exec->Advance();
      if (exec->collect_matched_word() && exec->IsInGroup()) {
        INVALIDATE(exec->current_matched_string());
        exec->current_matched_string()->Append(isolate, subject->at(p));
        JSString::Utf8String str(exec->current_matched_string());
      }
    }
    return exec->store_flag(Smi::FromInt(matched));
  }
  exec->store_flag(Smi::FromInt(1));
}

REGEX_HANDLER(RegexSome) {
  auto chars = JSString::Cast(
      reinterpret_cast<Object*>(fetcher->FetchNextWordOperand()));
  INVALIDATE(chars->shape()->IsJSString());
  auto p = exec->position();
  auto subject = exec->input();
  if (subject->length() > p) {
    auto code = subject->at(p);
    for (auto &ch : *chars) {
      if (ch == code) {
        exec->Advance();
        if (exec->collect_matched_word() && exec->IsInGroup()) {
          INVALIDATE(exec->current_matched_string());
          exec->current_matched_string()->Append(isolate, subject->at(p));
        }
        return exec->store_flag(Smi::FromInt(1));
      }
    }
    return exec->store_flag(Smi::FromInt(0));
  }

  exec->store_flag(Smi::FromInt(1));
}

REGEX_HANDLER(RegexRepeatRangeStart) {
  auto start = fetcher->FetchNextDoubleOperand();
  auto end = fetcher->FetchNextDoubleOperand();
  exec->StartRepeatRange(start, end);
}

REGEX_HANDLER(RegexRepeatStart) {
  auto flag = fetcher->FetchNextShortOperand();
  auto c = flag? 1: 0;
  exec->StartRepeat(c);
}

REGEX_HANDLER(RegexRepeatEnd) {
  auto jmp = fetcher->FetchNextWideOperand();
  if (!exec->repeat_acc().IsSatisfied()) {
    fetcher->UpdatePC(jmp);
  }
  exec->EndRepeat();
}

REGEX_HANDLER(RegexJumpIfMatched) {
  auto jmp = fetcher->FetchNextWideOperand();
  if (exec->load_flag()->value()) {
    fetcher->UpdatePC(jmp);
  }
}

REGEX_HANDLER(RegexJumpIfFailed) {
  auto jmp = fetcher->FetchNextWideOperand();
  if (!exec->load_flag()->value()) {
    fetcher->UpdatePC(jmp);
  }
}

REGEX_HANDLER(RegexComment) {
  USE(fetcher->FetchNextWordOperand());
  return;
}

HANDLER(Comment) {
  USE(fetcher->FetchNextWordOperand());
  return;
}

HANDLER(Print) {
  auto i = fetcher->FetchNextShortOperand();
  auto str = exec->load_register_value_at(i)->ToString();
  printf("VMDebugPrint: %s\n", str.c_str());
}

HANDLER(ExecIf) {
  auto v = exec->load_flag();
  auto boolean_value = JSSpecials::ToBoolean(v);
  if (!boolean_value) {
    fetcher->UpdatePC(fetcher->pc() + 1);
  }
}

HANDLER(Jmp) {
  auto next = fetcher->FetchNextWideOperand();
  fetcher->UpdatePC(next);
}

HANDLER(JmpIfTrue) {
  auto v = exec->load_flag();
  auto next = fetcher->FetchNextWideOperand();
  auto boolean_value = JSSpecials::ToBoolean(v);
  if (boolean_value) {
    fetcher->UpdatePC(next);
  }
}

HANDLER(JmpIfFalse) {
  auto v = exec->load_flag();
  auto next = fetcher->FetchNextWideOperand();
  auto boolean_value = JSSpecials::ToBoolean(v);
  if (!boolean_value) {
    fetcher->UpdatePC(next);
  }
}

HANDLER(I8Constant) {
  auto value = fetcher->FetchNextShortOperand();
  auto reg = fetcher->FetchNextShortOperand();
  exec->store_register_value_at(reg, Smi::FromInt(value));
}

HANDLER(I32Constant) {
  auto value = fetcher->FetchNextWideOperand();
  auto reg = fetcher->FetchNextShortOperand();
  if (value < Smi::kMaxValue) {
    exec->store_register_value_at(reg, Smi::FromInt(value));
  } else {
    exec->store_register_value_at(reg, *JSNumber::New(isolate, value));
  }
}

HANDLER(Append) {
  auto value_reg = fetcher->FetchNextShortOperand();
  auto reg = fetcher->FetchNextShortOperand();
  auto value = exec->load_register_value_at(value_reg);
  auto obj = exec->load_register_value_at(reg);
  INVALIDATE(obj->IsHeapObject());
  auto ho = HeapObject::Cast(obj);
  switch (ho->shape()->instance_type()) {
    case InstanceType::JS_STRING: {
      JSString::Cast(ho)->Append(
          isolate, static_cast<u32>(JSNumber::GetIntValue(value)));
      break;
    }
    case InstanceType::JS_ARRAY: {
      JSArray::Cast(obj)->Push(isolate, value);
      break;
    }
    default: {
      return;
    }
  }
}

HANDLER(CallFastPropertyA) {
  auto property = fetcher->FetchNextShortOperand();
  auto input = fetcher->FetchNextShortOperand();
  auto output = fetcher->FetchNextShortOperand();
  auto obj = exec->load_register_value_at(input);
  switch (static_cast<FastProperty>(property)) {
    case FastProperty::kLength:
      exec->store_register_value_at(output,
                                    JSObject::GetLength(isolate, obj));
  }
}

HANDLER(LoadConstant) {
  auto index = fetcher->FetchNextDoubleOperand();
  auto reg = fetcher->FetchNextShortOperand();
  auto obj = constant->at(index);
  exec->store_register_value_at(reg, obj);
}

HANDLER(NewEmptyJSArray) {
  auto out = fetcher->FetchNextShortOperand();
  auto jsa = JSArray::NewEmptyArray(isolate, 0);
  exec->store_register_value_at(out, *jsa);
}

HANDLER(NewEmptyJSString) {
  auto out = fetcher->FetchNextShortOperand();
  auto jss = JSString::New(isolate, "");
  exec->store_register_value_at(out, *jss);
}

HANDLER(LoadIx) {
  auto reg1 = fetcher->FetchNextShortOperand();
  auto index_reg = fetcher->FetchNextShortOperand();
  auto reg2 = fetcher->FetchNextShortOperand();
  auto obj = exec->load_register_value_at(reg1);
  auto index_obj = exec->load_register_value_at(index_reg);

  int index;
  if (index_obj->IsSmi()) {
    index = Smi::Cast(index_obj)->value();
  } else {
    auto jsn = JSNumber::Cast(index_obj);
    if (jsn->IsNaN()) {
      return exec->store_register_value_at(reg2, isolate->jsval_undefined());
    }
    index = jsn->int_value();
  }

  if (obj->IsHeapObject()) {
    auto v = HeapObject::Cast(obj);
    switch (v->shape()->instance_type()) {
      case InstanceType::JS_STRING: {
        auto ret = JSString::Cast(v)->at(index);
        Object* v = nullptr;
        if (ret < Smi::kMaxValue) {
          v = Smi::FromInt(ret);
        } else {
          v = JSNumber::NewWithoutHandle(isolate, ret);
        }
        exec->store_register_value_at(reg2, v);
        break;
      }
      default:
        return;
    }
  }
}

HANDLER(Cmp) {
  auto reg1 = fetcher->FetchNextShortOperand();
  auto reg2 = fetcher->FetchNextShortOperand();
  auto valueA = exec->load_register_value_at(reg1);
  auto valueB = exec->load_register_value_at(reg2);
  exec->store_flag(Smi::FromInt(valueA->Equals(valueB)));
}

HANDLER(Mov) {
  auto reg1 = fetcher->FetchNextShortOperand();
  auto reg2 = fetcher->FetchNextShortOperand();
  auto value = exec->load_register_value_at(reg1);
  exec->store_register_value_at(reg2, value);
}

HANDLER(Gt) {
  auto reg1 = fetcher->FetchNextShortOperand();
  auto reg2 = fetcher->FetchNextShortOperand();
  auto valueA = exec->load_register_value_at(reg1);
  auto valueB = exec->load_register_value_at(reg2);
  exec->store_flag(Smi::FromInt(valueA->GreaterThan(valueB)));
}

HANDLER(GtEq) {
  auto reg1 = fetcher->FetchNextShortOperand();
  auto reg2 = fetcher->FetchNextShortOperand();
  auto valueA = exec->load_register_value_at(reg1);
  auto valueB = exec->load_register_value_at(reg2);
  exec->store_flag(Smi::FromInt(valueA->GreaterThan(valueB) ||
                                valueA->Equals(valueB)));
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
    exec->store_register_value_at(
        RegisterAllocator::kAcc,
        Smi::FromInt(Smi::Cast(value)->value() + add));
  }
}

HANDLER(Sub) {
  auto reg1 = fetcher->FetchNextShortOperand();
  auto sub = fetcher->FetchNextShortOperand();
  auto value = exec->load_register_value_at(reg1);
  exec->store_register_value_at(
      RegisterAllocator::kAcc,
      Smi::FromInt(
          Smi::Cast(value)->value() - sub));
}

HANDLER(Mul) {
  auto reg1 = fetcher->FetchNextShortOperand();
  auto mul = fetcher->FetchNextShortOperand();
  auto value = exec->load_register_value_at(reg1);
  exec->store_register_value_at(
      RegisterAllocator::kAcc,
      Smi::FromInt(
          Smi::Cast(value)->value() * mul));
}

HANDLER(Div) {
  auto reg1 = fetcher->FetchNextShortOperand();
  auto div = fetcher->FetchNextShortOperand();
  auto value = exec->load_register_value_at(reg1);
  exec->store_register_value_at(
      RegisterAllocator::kAcc,
      Smi::FromInt(
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
    static_cast<uint8_t>(Bytecode::kExit)
    - static_cast<uint8_t>(Bytecode::kRegexMatched) + 1> kDispatchTable = {{
#define VM_JMP_TABLES(Name, Layout, size, n, ...) &&Label_##Name,
      BYTECODE_LIST_WITHOUT_RETURN(VM_JMP_TABLES)
#undef VM_JMP_TABLES
      &&Label_Return,
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
    auto ret_val = fetcher_->FetchNextShortOperand();
    return_value_ = load_register_value_at(ret_val);
    //  DO NOTHING
  }
  Label_Exit: {
    return;
    //  DO NOTHING
  }
}

Object* VirtualMachine::ExecuteRegex(BytecodeExecutable* executable,
                                     JSString* input,
                                     bool collect_matched_word) {
  RegexExecutor exec(
      isolate_, input, collect_matched_word, executable);
  exec.Execute();
  return !collect_matched_word?
      reinterpret_cast<Object*>(
          exec.is_matched()? isolate_->jsval_true(): isolate_->jsval_false())
      : reinterpret_cast<Object*>(exec.matched_array());
}

void VirtualMachine::RegexExecutor::Execute() {
#define VM_STATIC_HANDLER(Name, Layout, size, n, ...)        \
  static Name##BytecodeHandler Name##_bytecode_handler;
  REGEX_BYTECODE_LIST_WITOUT_MATCHED(VM_STATIC_HANDLER)
#undef VM_STATIC_HANDLER

  static const std::array<
    void*,
    static_cast<uint8_t>(Bytecode::kRegexMatched) + 1> kDispatchTable = {{
#define VM_JMP_TABLES(Name, Layout, size, n, ...) &&Label_##Name,
      REGEX_BYTECODE_LIST_WITOUT_MATCHED(VM_JMP_TABLES)
#undef VM_JMP_TABLES
      &&Label_Matched
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
    printf("%d %s\n", fetcher_->pc() - 1,BytecodeUtil::ToStringOpecode(static_cast<Bytecode>(next))); \
    goto *kDispatchTable[next];                       \
  }
  REGEX_BYTECODE_LIST_WITOUT_MATCHED(VM_EXECUTE)
#undef VM_EXECUTE
  Label_Matched: {
    set_matched();
    //  DO NOTHING
  }
}
}  // namespace lux
