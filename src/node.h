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

#ifndef SRC_NODE_H_
#define SRC_NODE_H_

#include <stdint.h>
#include "./utils.h"

namespace lux {
#define FIELD_ADDR(p, offset)                             \
  (reinterpret_cast<Address>(p) + offset)

#define FIELD_ADDR_CONST(p, offset)                             \
  (reinterpret_cast<const byte*>(p) + offset)

class ZoneAllocator {
 public:
  static const int32_t kDefaultSize = 1 KB;
  ZoneAllocator();
  Address Allocate(size_t size);
 private:
  void Grow();
  size_t remains_;
  size_t current_size_;
  Address addr_;
};

// Node represented by below layout.
// [ 16bit size field]
// [ 8bit type field]
template <typename Type>
class Node {
 public:
  static const uint8_t kNodeSize = 3;
  static const uint8_t kSizeFieldOffset = 0;
  static const uint8_t kTypeFieldOffset = sizeof(uint16_t);
  template <typename T>
  T New(Type t, ZoneAllocator* zone_allocator, uint16_t size) {
    auto p = zone_allocator->Allocate(kNodeSize + size);
    auto n = reinterpret_cast<T*>(p);
    n->set_size(size);
    n->set_type(t);
    return n;
  }

  uint16_t size() const {
    auto ret = FIELD_ADDR_CONST(this, 0);
    return *ret;
  }

  Type type() const {
    return static_cast<Type>(*(FIELD_ADDR_CONST(this, sizeof(uint16_t))));
  }

 private:
  void set_type(Type type) {
    *(FIELD_ADDR(this, sizeof(uint16_t))) = type;
  }

  void set_size(uint16_t size) {
    *(FIELD_ADDR(this, 0)) = size;
  }
};
}  // namespace lux

#endif  // SRC_NODE_H_
