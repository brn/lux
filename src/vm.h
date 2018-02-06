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
      int i = 0;
      for (auto &r : argv) {
        store_register_value_at(i++, r);
      }
      fetcher_(executable->bytecode_array());
    }

    LUX_INLINE Object* load_accumulator() {
      return acc_;
    }

    LUX_INLINE void store_accumulator(Object* value) {
      acc_ = value;
    }

    LUX_INLINE Object* load_register_value_at(int index) {
      return registers_.load(index);
    }

    LUX_INLINE void store_register_value_at(int index, Object* value) {
      registers_.store(index, value);
    }

    Object* return_value() const {
      return acc_;
    }

    void Execute();

   private:
    size_t argc_;
    Object* argv_;
    Object* acc_;
    Registers registers_;
    Isolate* isolate_;
    BytecodeConstantArray* constant_pool_;
    LazyInitializer<BytecodeFetcher> fetcher_;
  };

  explicit VirtualMachine(Isolate* isolate)
          : isolate_(isolate) {}

  Object* Execute(BytecodeExecutable* executable,
                  std::initializer_list<Object*> argv);


 private:
  Isolate* isolate_;
};
}  // namespace lux

#endif  // SRC_VM_H_
