// The MIT License (MIT)
//
// Copyright (c) 2013 Taketoshi Aono(brn)
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

#ifndef SRC_SOURCE_POSITION_H_
#define SRC_SOURCE_POSITION_H_

#include <string>
#include "./utils.h"

namespace lux {
class SourcePosition : private Unmovable {
 public:
  typedef uint32_t Id;

  SourcePosition()
      : start_col_(0),
        end_col_(0),
        start_line_number_(0),
        end_line_number_(0) {}

  SourcePosition(size_t start_col, size_t end_col, size_t start_line_number,
                 size_t end_line_number)
      : start_col_(start_col),
        end_col_(end_col),
        start_line_number_(start_line_number),
        end_line_number_(end_line_number) {}

  SourcePosition(std::initializer_list<size_t> list) {
    auto begin = list.begin();
    auto end = list.end();
    start_col_ = begin != end ? *(begin++) : 0;
    end_col_ = begin != end ? *(begin++) : 0;
    start_line_number_ = begin != end ? *(begin++) : 0;
    end_line_number_ = begin != end ? *begin : 0;
  }

  SourcePosition(const SourcePosition& source_position)
      : start_col_(source_position.start_col_),
        end_col_(source_position.end_col_),
        start_line_number_(source_position.start_line_number_),
        end_line_number_(source_position.end_line_number_) {}

  inline friend std::ostream& operator<<(
      std::ostream& stream, const SourcePosition& source_position) {
    stream << "{\n  start_col: " << source_position.start_col_ << ","
           << "\n  end_col: " << source_position.end_col_ << ","
           << "\n  start_line: " << source_position.start_line_number_ << ","
           << "\n  end_line: " << source_position.end_line_number_ << "\n}";
    return stream;
  }

  inline void operator=(const SourcePosition& source_position) {
    start_col_ = source_position.start_col_;
    end_col_ = source_position.end_col_;
    start_line_number_ = source_position.start_line_number_;
    end_line_number_ = source_position.end_line_number_;
  }

  inline bool operator>(const SourcePosition& source_position) const {
    if (start_line_number_ == source_position.start_line_number_) {
      return start_col_ > source_position.start_col_;
    }
    return start_line_number_ > source_position.start_line_number_;
  }

  inline bool operator<(const SourcePosition& source_position) const {
    if (start_line_number_ == source_position.start_line_number_) {
      return start_col_ < source_position.start_col_;
    }
    return start_line_number_ < source_position.start_line_number_;
  }

  inline bool operator>=(const SourcePosition& source_position) const {
    if (start_line_number_ == source_position.start_line_number_) {
      return start_col_ >= source_position.start_col_;
    }
    return start_line_number_ >= source_position.start_line_number_;
  }

  inline bool operator<=(const SourcePosition& source_position) const {
    if (start_line_number_ == source_position.start_line_number_) {
      return start_col_ <= source_position.start_col_;
    }
    return start_line_number_ <= source_position.start_line_number_;
  }

  inline bool operator==(const SourcePosition& source_position) const {
    return start_line_number_ == source_position.start_line_number_ &&
           start_col_ == source_position.start_col_ &&
           end_line_number_ == source_position.end_line_number_ &&
           end_col_ == source_position.end_col_;
  }

  LUX_CONST_PROPERTY(size_t, start_col, start_col_)
  LUX_INLINE void add_start_col(int index = 1) { start_col_ += index; }
  LUX_CONST_PROPERTY(size_t, end_col, end_col_)
  LUX_INLINE void add_end_col(int index = 1) { end_col_ += index; }
  LUX_CONST_PROPERTY(size_t, start_line_number, start_line_number_)
  LUX_INLINE void add_start_line_number(int index = 1) {
    start_line_number_ += index;
  }
  LUX_CONST_PROPERTY(size_t, end_line_number, end_line_number_)
  LUX_INLINE void add_end_line_number(int index = 1) {
    end_line_number_ += index;
  }

  std::string ToString() const {
    std::stringstream ss;
    ss << "[" << start_col() << ',' << end_col() << ',' << start_line_number()
       << ',' << end_line_number() << ']';
    return ss.str();
  }

 private:
  size_t start_col_;
  size_t end_col_;
  size_t start_line_number_;
  size_t end_line_number_;
};
}  // namespace lux

#endif  // SRC_SOURCE_POSITION_H_
