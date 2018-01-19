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

#include "./node.h"

namespace i6 {
ZoneAllocator::ZoneAllocator()
    : remains_(kDefaultSize), current_size_(kDefaultSize) {
  addr_ = reinterpret_cast<Address>(malloc(sizeof(kDefaultSize)));
}

Address ZoneAllocator::Allocate(size_t size) {
  while (remains_ < size) {
    Grow();
  }
  auto n = addr_;
  addr_ += size;
  remains_ -= size;
  return n;
}

void ZoneAllocator::Grow() {
  auto before_size = current_size_;
  current_size_ += kDefaultSize;
  remains_ += kDefaultSize;
  auto n = reinterpret_cast<Address>(malloc(current_size_));
  memcpy(n, addr_, before_size);
  free(addr_);
  addr_ = n;
}

const int32_t ZoneAllocator::kDefaultSize;

template <typename Type>
const uint8_t Node<Type>::kNodeSize;
template <typename Type>
const uint8_t Node<Type>::kSizeFieldOffset;
template <typename Type>
const uint8_t Node<Type>::kTypeFieldOffset;
}  // namespace i6
