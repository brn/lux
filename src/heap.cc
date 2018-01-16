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

#include <sys/mman.h>
#include "./heap.h"

namespace i6 {
Address MMap(size_t size) {
  return reinterpret_cast<Address>(mmap(
      nullptr,
      size,
      PROT_READ | PROT_WRITE | PROT_EXEC ,
      MAP_ANON | MAP_PRIVATE,
      -1, 0));
}

Heap::Heap(size_t default_size)
    : size_(default_size) {
  arena_ = MMap(default_size);
}

Address Heap::Allocate(size_t size) {
  if (size_ - used_ >= size) {
    Address ret = arena_;
    arena_ += size;
    used_ += size;
    return ret;
  }
  Grow();
  return Allocate(size);
}

Heap* Heap::GetHeap() {
  if (heap_ == nullptr) {
    heap_ = new Heap(kDefaultSize);
  }
  return heap_;
}

Heap* Heap::heap_ = nullptr;

void Heap::Grow() {
  size_ = size_ * 2;
  arena_ = MMap(size_);
}
}  // namespace i6
