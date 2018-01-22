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

#ifndef SRC_ZONE_H_
#define SRC_ZONE_H_

#include <stdint.h>
#include "./utils.h"

namespace lux {
class ZoneAllocator {
 public:
  static const int32_t kDefaultSize = 1 KB;
  ZoneAllocator();
  Address Allocate(size_t size);

 private:
  class Chunk {
   public:
    Chunk() = delete;
    explicit Chunk(size_t size);
    ~Chunk();
    Address Allocate(size_t size);
    LUX_INLINE bool HasEnoughCapacity(size_t size) const {
      return remains_ >= size;
    }
    LUX_INLINE void set_next(Shared<Chunk> next) {
      next_ = next;
    }
   private:
    void AllocateInternal();
    Address addr_;
    Address top_;
    Shared<Chunk> next_;
    size_t size_;
    size_t remains_;
  };
  void Grow(size_t size);
  Shared<Chunk> chunk_;
};

class Zone {
  friend class ZoneAllocator;
 public:
  void* operator new(size_t size, ZoneAllocator* zone_allocator);
  void operator delete(void* ptr);
  void operator delete(void* ptr, ZoneAllocator* zone_allocator);
};
}  // namespace lux

#endif  // SRC_ZONE_H_
