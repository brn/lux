/**
 * The MIT License (MIT)
 * Copyright (c) Taketoshi Aono
 *  
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *  
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *  
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * @fileoverview 
 * @author Taketoshi Aono
 */

#include "./stacktrace.h"

#include <stdio.h>
#include <execinfo.h>
#include <cxxabi.h>
#include <dlfcn.h>
#include <stdlib.h>

namespace lux {
namespace debug {

#define MAX_DEPTH 32

StackTrace::StackTrace(int num_discards) {
  // retrieve call-stack
  void* trace[MAX_DEPTH];
  int stack_depth = backtrace(trace, MAX_DEPTH);

  for (int i = num_discards + 1; i < stack_depth; i++) {
    Dl_info dlinfo;
    if (!dladdr(trace[i], &dlinfo)) {
      break;
    }

    const char * symname = dlinfo.dli_sname;

    int status;
    char* demangled = abi::__cxa_demangle(symname, NULL, 0, &status);
    if (status == 0 && demangled) {
      symname = demangled;
    }

    // store entry to stack
    if (dlinfo.dli_fname && symname) {
      Entry e;
      e.file = dlinfo.dli_fname;
      e.line = 0;
      e.function = symname;
      stack_.push_back(e);
    } else {
      break;
    }

    if (demangled) {
      free(demangled);
    }
  }
}

}  // namespace debug
}  // namespace lux
