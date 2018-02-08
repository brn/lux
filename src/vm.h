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

#ifndef SRC_VM_H_
#define SRC_VM_H_

#include <array>
#include <vector>
#include "./bytecode.h"
#include "./utils.h"

namespace lux {
class Isolate;
class BytecodeExecutable;

class VirtualMachine {
 public:
  class Registers {
   public:
    void store(int index, Object* value) {
      reg_[index] = value;
    }

    Object* load(int index) {
      INVALIDATE(index < 256);
      return reg_[index];
    }

    void Clear() {
      for (int i = 0; i< 256; i++) {
        reg_[i] = nullptr;
      }
    }

   private:
    Object* reg_[256];
  };

  class Executor {
   public:
    Executor(Isolate* isolate,
             size_t argc,
             std::initializer_list<Object*> argv,
             BytecodeExecutable* executable)
        : isolate_(isolate),
          constant_pool_(executable->constant_pool()) {
      int i = RegisterAllocator::kParameter1;
      for (auto &r : argv) {
        store_register_value_at(i++, r);
      }
      for (; i <= RegisterAllocator::kParameter10; i++) {
        store_register_value_at(i, isolate->jsval_undefined());
      }
      fetcher_(executable->bytecode_array());
    }

    LUX_INLINE Smi* load_flag() {
      return Smi::Cast(
          registers_.load(RegisterAllocator::kFlag));
    }

    LUX_INLINE void store_flag(Smi* value) {
      registers_.store(RegisterAllocator::kFlag, value);
    }

    LUX_INLINE Object* load_register_value_at(int index) {
      return registers_.load(index);
    }

    LUX_INLINE void store_register_value_at(int index, Object* value) {
      registers_.store(index, value);
    }

    Object* return_value() const {
      return return_value_;
    }

    void Execute();

   private:
    size_t argc_;
    Object* argv_;
    Object* return_value_;
    Registers registers_;
    Isolate* isolate_;
    BytecodeConstantArray* constant_pool_;
    LazyInitializer<BytecodeFetcher> fetcher_;
  };

  class RegexExecutor {
   public:
    RegexExecutor(Isolate* isolate,
                  JSString* input,
                  bool collect_matched_word,
                  BytecodeExecutable* executable)
        : flag_(Smi::FromInt(0)),
          group_(0),
          input_(input),
          position_(0),
          matched_array_(nullptr),
          current_matched_string_(nullptr),
          isolate_(isolate),
          constant_pool_(executable->constant_pool()) {
      if (collect_matched_word) {
        vm_flag_.set(0);
        matched_array_ = *JSArray::NewEmptyArray(isolate, 0);
      }
      fetcher_(executable->bytecode_array());
    }

    class RepeatAccumulator {
     public:
      RepeatAccumulator(int least_match_count,
                        int max_match_count)
          : matched_count_(0),
            least_match_count_(least_match_count),
            max_match_count_(max_match_count) {}

      void matched() { matched_count_++; }

      inline bool IsSatisfied() const {
        return matched_count_ >= least_match_count_
          && max_match_count_ > 0? matched_count_ <= max_match_count_: true;
      }
     private:
      uint32_t matched_count_;
      uint16_t least_match_count_;
      int max_match_count_;
    };

    LUX_CONST_GETTER(bool, collect_matched_word, vm_flag_.get(0))
    LUX_INLINE void set_matched() {
      vm_flag_.set(1);
    }
    LUX_CONST_GETTER(bool, is_matched, vm_flag_.get(1))

    LUX_INLINE Smi* load_flag() {
      return flag_;
    }

    LUX_INLINE void store_flag(Smi* value) {
      flag_ = value;
    }

    LUX_GETTER(JSString*, input, input_);
    LUX_GETTER(uint32_t, position, position_);
    inline void Advance() {
      position_++;
    }
    LUX_GETTER(JSArray*, matched_array, matched_array_)
    LUX_GETTER(JSString*, current_matched_string, current_matched_string_)

    void StartRepeat(int least_count) {
      repeat_acc_stack_.push_back(RepeatAccumulator(least_count, -1));
    }

    void EndRepeat() {
      repeat_acc_stack_.pop_back();
    }

    void StartRepeatRange(int least_count, int max_count) {
      repeat_acc_stack_.push_back(RepeatAccumulator(least_count, max_count));
    }

    const RepeatAccumulator& repeat_acc() const {
      return repeat_acc_stack_.back();
    }

    inline void PrepareStringBuffer() {
      current_matched_string_ = *JSString::New(isolate_, "");
    }

    void EnterGroup() {
      group_++;
    }

    void LeaveGroup() {
      group_--;
    }

    bool IsInGroup() const {
      return group_ > 0;
    }

    void Execute();

#ifdef DEBUG
    struct ExecutionLog {
      uint32_t start_position;
      Bytecode bytecode;
    };

    void CollectExecutionLog(uint32_t start_position,
                             Bytecode bytecode) {
      ExecutionLog e = {start_position, bytecode};
      exec_log_list_.push_back(e);
    }

    const std::vector<ExecutionLog>& exec_log() const {
      return exec_log_list_;
    }

    void PrintLog() const {
      for (auto &log : exec_log_list_) {
        printf("%d %s\n", log.start_position,
               BytecodeUtil::ToStringOpecode(static_cast<Bytecode>(
                   log.bytecode)));
      }
    }
#endif

   private:
    std::vector<RepeatAccumulator> repeat_acc_stack_;
    Smi* flag_;
    Bitset<uint8_t> vm_flag_;
    uint8_t group_;
    JSString* input_;
    uint32_t position_;
    JSArray* matched_array_;
    JSString* current_matched_string_;
    Isolate* isolate_;
    BytecodeConstantArray* constant_pool_;
    LazyInitializer<BytecodeFetcher> fetcher_;

#ifdef DEBUG
    std::vector<ExecutionLog> exec_log_list_;
#endif
  };

  explicit VirtualMachine(Isolate* isolate)
          : isolate_(isolate) {}

  Object* Execute(BytecodeExecutable* executable,
                  std::initializer_list<Object*> argv);

  Object* ExecuteRegex(BytecodeExecutable* executable,
                       JSString* input,
                       bool collect_matched_word);


 private:
  Isolate* isolate_;
};
}  // namespace lux

#endif  // SRC_VM_H_
