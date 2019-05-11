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
#include <string>
#include <vector>
#include "./bytecode.h"
#include "./chars.h"
#include "./maybe.h"
#include "./platform/os.h"
#include "./utils.h"

namespace lux {
class Isolate;
class BytecodeExecutable;
class JSString;

class VirtualMachine {
 public:
  class Registers {
   public:
    void store(int index, Object* value) { reg_[index] = value; }

    Object* load(int index) {
      INVALIDATE(index < 256);
      return reg_[index];
    }

    void Clear() {
      for (int i = 0; i < 256; i++) {
        reg_[i] = nullptr;
      }
    }

   private:
    Object* reg_[256];
  };

  class Executor {
   public:
    Executor(Isolate* isolate, size_t argc, std::initializer_list<Object*> argv,
             BytecodeExecutable* executable)
        : isolate_(isolate), constant_pool_(executable->constant_pool()) {
      int i = RegisterAllocator::kParameter1;
      for (auto& r : argv) {
        store_register_value_at(i++, r);
      }
      for (; i <= RegisterAllocator::kParameter10; i++) {
        store_register_value_at(i, isolate->jsval_undefined());
      }
      fetcher_(executable->bytecode_array());
      USE(argc_);
      USE(argv_);
    }

    LUX_INLINE Smi* load_flag() {
      return Smi::Cast(registers_.load(RegisterAllocator::kFlag));
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

    Object* return_value() const { return return_value_; }

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

  enum Flag {
    kCollectWords,
    kMatched,
    kClassMatch,
    kRetry,
    kMultiline,
    kGlobal,
    kIgnoreCase,
    kSticky
  };

  class RegexExecutor {
   public:
    RegexExecutor(Isolate* isolate, JSString* input, bool collect_matched_word,
                  BytecodeExecutable* executable)
        : saved_position_(0),
          captured_array_(isolate->jsval_null()),
          current_capture_(nullptr),
          flag_(Smi::FromInt(0)),
          input_(input),
          isolate_(isolate),
          constant_pool_(executable->constant_pool()) {
      if (collect_matched_word) {
        vm_flag_.set(kCollectWords);
      }
      current_thread_ = new (zone()) Thread(0, 0, 0, 0, 0, Smi::FromInt(0));
      unset_matched();
      fetcher_(executable->bytecode_array());
    }

    class Captured : public Zone {
     public:
      explicit Captured(uint32_t start) : start_(start), end_(0) {}

      LUX_CONST_PROPERTY(uint32_t, start, start_)
      LUX_CONST_PROPERTY(uint32_t, end, end_)

      bool IsCaptured() const { return start_ < end_; }

     private:
      uint32_t start_;
      uint32_t end_;
    };

    class Thread : public Zone {
     public:
      Thread(uint32_t pc, uint32_t start_position, uint32_t position,
             uint32_t matched_count, uint16_t capture_index, Smi* flag)
          : start_position_(start_position),
            pc_(pc),
            position_(position),
            matched_count_(matched_count),
            capture_index_(0),
            flag_(flag) {}

      LUX_CONST_PROPERTY(uint32_t, start_position, start_position_)
      LUX_CONST_PROPERTY(uint32_t, pc, pc_)
      LUX_CONST_PROPERTY(uint32_t, position, position_)
      LUX_CONST_PROPERTY(uint32_t, matched_count, matched_count_)
      LUX_CONST_PROPERTY(Smi*, flag, flag_)
      LUX_CONST_PROPERTY(uint16_t, capture_index, capture_index_)
      void Matched() { matched_count_++; }
      void Advance() { position_++; }
      void ResetMatchedCount() { matched_count_ = 0; }

     private:
      uint32_t start_position_;
      uint32_t pc_;
      uint32_t position_;
      uint32_t matched_count_;
      uint32_t capture_index_;
      Smi* flag_;
    };

    LUX_CONST_PROPERTY(uint32_t, saved_position, saved_position_)
    LUX_CONST_GETTER(Object*, captured_array, captured_array_)
    LUX_CONST_GETTER(bool, collect_matched_word, vm_flag_.get(0))
    LUX_INLINE void set_matched() { vm_flag_.set(kMatched); }
    LUX_INLINE void unset_matched() { vm_flag_.unset(kMatched); }
    LUX_CONST_GETTER(bool, is_matched, vm_flag_.get(1))

    LUX_INLINE bool is_advanceable() {
      return input()->length() > (current_thread()->position() + 1);
    }

    LUX_INLINE void store_position() {
      saved_position_ = current_thread()->position();
    }
    LUX_INLINE void load_position() {
      current_thread()->set_start_position(saved_position_);
    }

    void EnableClassMatch() { vm_flag_.set(kClassMatch); }

    bool is_class_match_enabled() { return vm_flag_.get(kClassMatch); }

    void DisableClassMatch() { vm_flag_.unset(kClassMatch); }

    void disable_retry() { vm_flag_.set(kRetry); }

    bool is_retryable() { return !vm_flag_.get(kRetry); }

    void set_multiline() { vm_flag_.set(kMultiline); }

    bool is_multiline() { return vm_flag_.get(kMultiline); }

    void set_global() { vm_flag_.set(kGlobal); }

    bool is_global() { return vm_flag_.get(kGlobal); }

    void set_ignore_case() { vm_flag_.set(kIgnoreCase); }

    bool is_ignore_case() { return vm_flag_.get(kIgnoreCase); }

    void set_sticky() { vm_flag_.set(kSticky); }

    bool is_sticky() { return vm_flag_.get(kSticky); }

    LUX_INLINE Smi* load_flag() { return flag_; }

    LUX_INLINE void store_flag(Smi* value) { flag_ = value; }

    LUX_GETTER(JSString*, input, input_);
    inline void Advance() { current_thread_->Advance(); }

    void Execute();

    inline bool HasThread() const { return thread_stack_.size() > 0; }

    inline Thread* current_thread() { return current_thread_; }

    inline void ClearAllThread() { thread_stack_.clear(); }

    inline void NewThread(uint32_t pc) {
      auto t = new (zone())
          Thread(pc, current_thread_->start_position(),
                 current_thread_->position(), current_thread_->matched_count(),
                 current_thread_->capture_index(), flag_);
      thread_stack_.push_back(t);
    }

    Thread* PopThread() {
      if (thread_stack_.size()) {
        auto ret = thread_stack_.back();
        thread_stack_.pop_back();
        current_thread_ = ret;
        flag_ = ret->flag();
        fetcher_->UpdatePC(ret->pc());
        StartCapture(ret->capture_index(), false);
        return current_thread_;
      }
      return nullptr;
    }

    void FixInterCaptured();
    Handle<JSArray> FixCaptured();

    JSString* SliceMatchedInput(uint32_t start);

    inline void StartCapture(uint16_t index, bool set_start = true) {
      if (captured_stack_.size()) {
        if (current_capture_) {
          current_captured_index_stack_.push_back(index - 1);
        }
        current_capture_ = captured_stack_[index];
        if (set_start) {
          current_capture_->set_start(current_thread()->position());
        }
        current_thread()->set_capture_index(index);
      }
    }

    inline Maybe<Captured*> GetCaptured(uint32_t index) {
      if (index < captured_stack_.size()) {
        return Just(captured_stack_[index]);
      }
      return Nothing<Captured*>();
    }

    inline void UpdateCapture() {
      INVALIDATE(current_capture_);
      current_capture_->set_end(current_thread()->position());
      if (current_captured_index_stack_.size()) {
        auto next = current_captured_index_stack_.back();
        current_captured_index_stack_.pop_back();
        current_capture_ = captured_stack_[next];
        current_thread()->set_capture_index(next);
      } else {
        current_capture_ = nullptr;
      }
    }

    inline Captured* current_captured() { return current_capture_; }

    inline void ReserveCapture(uint16_t size) {
      captured_stack_.resize(size);
      for (auto i = 0; i < size; i++) {
        captured_stack_[i] = new (zone()) Captured(0);
      }
    }

    inline void ClearCapture() { captured_stack_.clear(); }

    int FindNextPosition() {
      if (!is_retryable() && is_multiline()) {
        int i = -1;
        for (auto& c : *input()) {
          if (c == '\n') {
            break;
          }
          i++;
        }
        return i;
      }
      return current_thread()->position() + 1;
    }

    void Reset() {
      ClearAllThread();
      ClearCapture();
      fetcher_->UpdatePC(0);
      NewThread(0);
      PopThread();
    }

    bool PrepareNextMatch(bool round_matched);

#ifdef DEBUG
    struct ExecutionLog {
      u8 value;
      uint32_t position;
      uint32_t start_position;
      bool matched;
      Bytecode bytecode;
    };

    void CollectExecutionLog(uint32_t start_position, Bytecode bytecode,
                             bool is_print) {
      u8 value = current_thread_->position() >= input_->length()
                     ? '?'
                     : input_->at(current_thread_->position()).ToAscii();
      ExecutionLog e = {value, current_thread_->position(), start_position,
                        JSSpecials::ToBoolean(load_flag()), bytecode};
      exec_log_list_.push_back(e);
      if (is_print) {
        auto s = DoPrintLog(e);
        printf("%s\n", s.c_str());
      }
    }

    const std::vector<ExecutionLog>& exec_log() const { return exec_log_list_; }

    void PrintLog() const {
      for (auto& log : exec_log_list_) {
        auto s = DoPrintLog(log);
        printf("%s\n", s.c_str());
      }
    }

    std::string DoPrintLog(const ExecutionLog& log) const {
      u32 u = Chars::GetCarretWordFromAsciiCodeIf(log.value);
      std::string v(u == '\n' ? "\\n"
                              : u == '\r' ? "\\r" : u == '\t' ? "\\t" : "");
      if (!v.size()) {
        if (u != log.value) {
          SPrintf(&v, false, "^%c", u);
        } else {
          SPrintf(&v, false, "%c", u);
        }
      }
      std::string result;
      SPrintf(
          &result, false, "%s %d %d %s %s", v.c_str(), log.position,
          log.start_position, log.matched ? "matched" : "unmatched",
          BytecodeUtil::ToStringOpecode(static_cast<Bytecode>(log.bytecode)));

      return result;
    }
#endif

   private:
    ZoneAllocator* zone() { return &zone_allocator_; }

    std::vector<uint32_t> current_captured_index_stack_;
    std::vector<Captured*> captured_stack_;
    std::vector<Thread*> thread_stack_;
    std::vector<JSString*> matched_words_;
    uint32_t saved_position_;
    Object* captured_array_;
    Captured* current_capture_;
    Thread* current_thread_;
    Smi* flag_;
    Bitset<uint8_t> vm_flag_;
    JSString* input_;
    Isolate* isolate_;
    BytecodeConstantArray* constant_pool_;
    LazyInitializer<BytecodeFetcher> fetcher_;
    ZoneAllocator zone_allocator_;

#ifdef DEBUG
    std::vector<ExecutionLog> exec_log_list_;
#endif
  };

  explicit VirtualMachine(Isolate* isolate) : isolate_(isolate) {}

  Object* Execute(BytecodeExecutable* executable,
                  std::initializer_list<Object*> argv);

  Object* ExecuteRegex(BytecodeExecutable* executable, JSString* input,
                       uint8_t flag, bool collect_matched_word);

 private:
  Isolate* isolate_;
};
}  // namespace lux

#endif  // SRC_VM_H_
