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

#ifndef SRC_STACKTRACE_H_
#define SRC_STACKTRACE_H_

#include <sstream>
#include <string>
#include <vector>

namespace lux {
namespace debug {

// Based on https://sourceforge.net/p/stacktrace/code/HEAD/tree/stacktrace/
class StackTrace {
 public:
  struct Entry {
    Entry()
        : line(0) {}

    std::string file;
    size_t      line;
    std::string function;

    std::string ToString() const {
      std::ostringstream os;
      os << file << " (" << line << "): " << function;
      return os.str();
    }
  };

  // Creates a stacktrace from the current location.
  explicit StackTrace(int num_discards = 0);
  void Print() const {
    std::ostringstream os;
    for (size_t i = 0; i < stack_.size(); i++) {
      os << stack_[i].ToString() << std::endl;
    }
    printf("%s\n", os.str().c_str());
  }

 private:
  std::vector<Entry> stack_;
};
}  // namespace debug
}  // namespace lux

#endif  // SRC_STACKTRACE_H_
