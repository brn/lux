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

#ifndef SRC_REPORTER_H_
#define SRC_REPORTER_H_

#include <sstream>
#include <string>
#include <utility>
#include <vector>

#include "./source_position.h"

namespace lux {

class ErrorDescriptor {
 public:
  ErrorDescriptor()
      : ignore_(true) {}

  explicit ErrorDescriptor(const SourcePosition& source_position)
      : ignore_(false),
        source_position_(source_position) {}

  ErrorDescriptor(ErrorDescriptor&& error_descriptor)
      : ignore_(error_descriptor.ignore_),
        source_position_(std::move(error_descriptor.source_position_)),
        error_message_(std::move(error_descriptor.error_message_)) {}


  template <typename T>
  ErrorDescriptor& operator << (T&& message) {
    if (!ignore_) {
      error_message_ << message;
    }
    return *this;
  }

  std::string message() const {
    return error_message_.str();
  }

  const SourcePosition& source_position() const {
    return source_position_;
  }

 private:
  bool ignore_;
  SourcePosition source_position_;
  std::stringstream error_message_;
  std::string cached_error_message_;
};

class ErrorReporter {
 public:
  using PendingErrors = std::vector<Shared<ErrorDescriptor>>;
  using iterator = PendingErrors::iterator;
  ErrorDescriptor& ReportSyntaxError(Shared<ErrorDescriptor>);

  void PrintError();

  LUX_INLINE bool HasPendingError() const {
    return size() > 0;
  }

  LUX_INLINE size_t size() const {
    return pending_errors_.size();
  }

  LUX_INLINE iterator begin() {
    return pending_errors_.begin();
  }

  LUX_INLINE iterator end() {
    return pending_errors_.end();
  }

 private:
  PendingErrors pending_errors_;
};
}  // namespace lux

#endif  // SRC_REPORTER_H_
