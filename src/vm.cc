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

#ifdef DEBUG
#define COLLECT_EXECUTION_LOG(pos, bc) \
  CollectExecutionLog(pos, static_cast<Bytecode>(bc))
#else
#define COLLECT_EXECUTION_LOG(pos, bc)
#endif


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
      LUX_INLINE void Execute(                  \
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

REGEX_HANDLER(RegexStartCapture) {
  auto index = fetcher->FetchNextDoubleOperand();
  if (exec->collect_matched_word()) {
    exec->StartCapture(index);
  }
}

REGEX_HANDLER(RegexUpdateCapture) {
  if (exec->collect_matched_word()) {
    exec->UpdateCapture();
  }
}

REGEX_HANDLER(RegexDisableRetry) {
  exec->disable_retry();
}

REGEX_HANDLER(RegexCheckEnd) {
  if (exec->is_advanceable()) {
    fetcher->UpdatePCToRegexFailed();
  }
}

REGEX_HANDLER(RegexEnableSearch) {
  exec->enable_search();
}

REGEX_HANDLER(RegexMatchAny) {
  auto pos = exec->current_thread()->position();
  if (exec->is_advanceable()) {
    if (exec->input()->at(pos) != '\n') {
      exec->current_thread()->Matched();
      exec->store_flag(Smi::FromInt(1));
      return exec->Advance();
    }
    return exec->store_flag(Smi::FromInt(0));
  }

  exec->store_flag(Smi::FromInt(0));
}

REGEX_HANDLER(RegexReserveCapture) {
  auto size = fetcher->FetchNextDoubleOperand();
  if (exec->collect_matched_word()) {
    exec->ReserveCapture(size);
  }
}

REGEX_HANDLER(RegexJumpIfMatchedCountEqual) {
  auto jmp = fetcher->FetchNextWideOperand();
  auto max = fetcher->FetchNextDoubleOperand();
  if (max == exec->current_thread()->matched_count()) {
    fetcher->UpdatePC(jmp);
  }
}

REGEX_HANDLER(RegexJumpIfMatchedCountLT) {
  auto jmp = fetcher->FetchNextWideOperand();
  auto max = fetcher->FetchNextDoubleOperand();
  if (max > exec->current_thread()->matched_count()) {
    fetcher->UpdatePC(jmp);
  }
}

REGEX_HANDLER(RegexBranch) {
  auto j_then = fetcher->FetchNextWideOperand();
  auto j_else = fetcher->FetchNextWideOperand();
  if (exec->load_flag()->value()) {
    fetcher->UpdatePC(j_then);
  } else {
    fetcher->UpdatePC(j_else);
  }
}

REGEX_HANDLER(RegexResetMatchedCount) {
  exec->current_thread()->ResetMatchedCount();
}

REGEX_HANDLER(RegexRune) {
  LUX_SCOPED([exec]() mutable { exec->disable_search(); });
  u32 ch = fetcher->FetchNextDoubleOperand();

  do {
    auto p = exec->current_thread()->position();
    auto subject = exec->input();
    if (subject->length() > p) {
      auto matched = ch == subject->at(p).code();
      if (matched) {
        exec->current_thread()->Matched();
        exec->Advance();
        return exec->store_flag(Smi::FromInt(1));
      }
      if (exec->is_search_enabled()) {
        exec->current_thread()->Advance();
        exec->ResetCurrentCapturedStartPosition();
        continue;
      }
      return exec->store_flag(Smi::FromInt(0));
    }
  } while(exec->is_advanceable()
          && exec->is_search_enabled());

  exec->store_flag(Smi::FromInt(0));
}

REGEX_HANDLER(RegexSome) {
  LUX_SCOPED([exec]() mutable { exec->disable_search(); });

  auto chars = JSString::Cast(
      reinterpret_cast<Object*>(fetcher->FetchNextWordOperand()));
  INVALIDATE(chars->shape()->IsJSString());

  do {
    auto p = exec->current_thread()->position();
    auto subject = exec->input();
    if (subject->length() > p) {
      auto code = subject->at(p);
      for (auto &ch : *chars) {
        if (ch == code) {
          exec->current_thread()->Matched();
          exec->Advance();
          return exec->store_flag(Smi::FromInt(1));
        }
      }

      if (exec->is_search_enabled()) {
        exec->current_thread()->Advance();
        exec->ResetCurrentCapturedStartPosition();
        continue;
      }

      return exec->store_flag(Smi::FromInt(0));
    }

    return exec->store_flag(Smi::FromInt(1));
  } while (exec->is_advanceable()
           && exec->is_search_enabled());

  exec->store_flag(Smi::FromInt(0));
}

REGEX_HANDLER(RegexEvery) {
  LUX_SCOPED([exec]() mutable { exec->disable_search(); });

  auto chars = JSString::Cast(
      reinterpret_cast<Object*>(fetcher->FetchNextWordOperand()));
  INVALIDATE(chars->shape()->IsJSString());
  auto subject = exec->input();
  auto subject_length = (subject->length() - 1);

  int matched = 0;
  do {
    auto p = exec->current_thread()->position();
    for (auto &ch : *chars) {
      if (subject_length < p) {
        return exec->store_flag(Smi::FromInt(0));
      }
      auto code = subject->at(p);
      if (ch == code) {
        matched++;
        exec->Advance();
        p++;
      } else {
        if (exec->is_search_enabled()) {
          exec->ResetCurrentCapturedStartPosition();
          if (matched == 0) {
            exec->Advance();
          } else {
            matched = 0;
          }
          goto NEXT;
        }
        exec->current_thread()->set_matched_count(matched);
        return exec->store_flag(Smi::FromInt(0));
      }
    }
 NEXT:
    USE(0);
  } while(exec->is_advanceable()
          && exec->is_search_enabled());

  exec->store_flag(Smi::FromInt(1));
}

REGEX_HANDLER(RegexCheckPosition) {
  auto j = fetcher->FetchNextWideOperand();
  if (exec->current_thread()->position() == exec->input()->length()) {
    if (exec->load_flag()->value()) {
      return fetcher->UpdatePC(j);
    }
    return fetcher->UpdatePCToRegexFailed();
  }
}

REGEX_HANDLER(RegexPushThread) {
  auto pc = fetcher->FetchNextWideOperand();
  exec->NewThread(pc);
}

REGEX_HANDLER(RegexPopThread) {
  auto r = exec->PopThread();
  if (!r) {
    fetcher->UpdatePCToRegexFailed();
  }
}

REGEX_HANDLER(RegexJump) {
  auto jmp = fetcher->FetchNextWideOperand();
  fetcher->UpdatePC(jmp);
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

Handle<JSArray> VirtualMachine::RegexExecutor::FixCaptured() {
  auto arr = JSArray::NewEmptyArray(isolate_, 0);
  auto end = current_thread()->position();

  if (end == input()->length()) {
    arr->Push(isolate_, input());
  } else {
    arr->Push(isolate_, *input()->Slice(isolate_, 0, end));
  }

  for (auto &c : captured_stack_) {
    std::cout << c->start() << c->end() << std::endl;
    auto str = input_->Slice(isolate_, c->start(), c->end());
    arr->Push(isolate_, *str);
  }
  return arr;
}

Object* VirtualMachine::ExecuteRegex(BytecodeExecutable* executable,
                                     JSString* input,
                                     bool collect_matched_word) {
  RegexExecutor exec(
      isolate_, input, collect_matched_word, executable);
  exec.Execute();
  exec.PrintLog();
  return !collect_matched_word?
    reinterpret_cast<Object*>(
        exec.is_matched()? isolate_->jsval_true(): isolate_->jsval_false())
    : reinterpret_cast<Object*>(exec.captured_array());
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
      &&Label_Failed,
      &&Label_Matched
    }};

  NewThread(0);
  PopThread();

Label_Retry:
  auto bc = fetcher_->FetchBytecodeAsInt();
  COLLECT_EXECUTION_LOG(fetcher_->pc() - 1, bc);
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
    COLLECT_EXECUTION_LOG(fetcher_->pc() - 1, next);  \
    printf("%s\n", BytecodeUtil::ToStringOpecode(static_cast<Bytecode>(next))); \
    goto *kDispatchTable[next];                       \
  }
  REGEX_BYTECODE_LIST_WITOUT_MATCHED(VM_EXECUTE)
#undef VM_EXECUTE
  Label_Failed: {
    unset_matched();
    if (is_retryable() &&
        input_->length() > current_thread()->position()) {
      auto next = current_thread()->position() + 1;
      ClearAllThread();
      ClearCapture();
      fetcher_->UpdatePC(0);
      NewThread(0);
      PopThread();
      current_thread()->set_position(next);
      goto Label_Retry;
    }
    return;
  }
  Label_Matched: {
    set_matched();
    if (collect_matched_word()) {
      captured_array_ = *FixCaptured();
    }
    //  DO NOTHING
  }
}
}  // namespace lux
