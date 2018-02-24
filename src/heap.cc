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
#include <string>
#include "./heap.h"
#include "./platform/os.h"

namespace lux {
HandleScope::HandleSet HandleScope::handle_set_;

Address MMap(size_t size) {
  auto ret = mmap(
      nullptr,
      size,
      PROT_READ | PROT_WRITE | PROT_EXEC,
      MAP_ANONYMOUS | MAP_PRIVATE,
      -1, 0);
  if (ret == reinterpret_cast<void*>(-1)) {
    std::string b;
    GetLastError(&b);
    FATAL(b.c_str());
  }
#ifdef DEBUG
  memset(ret, 0xCE, size);
  mprotect(ret, size, PROT_NONE);
#endif
  return reinterpret_cast<Address>(ret);
}

Heap::Heap(size_t default_size)
    : size_(default_size), used_(0) {
  arena_ = MMap(default_size);
}

Address Heap::Allocate(size_t size) {
//   auto size = sizeof(T);
//   size = LUX_ALIGN_OFFSET(size, kAlignment);
//   while (1) {
//     if (size_ - used_ > size) {
// #ifdef DEBUG
//       mprotect(arena_, size, PROT_READ | PROT_WRITE | PROT_EXEC);
// #endif
//       INVALIDATE(*arena_ == 0xCE);
//       Address ret = arena_;
//       arena_ += size;
//       used_ += size;
//       return Handle<T>(ret);
//     }
//     Grow();
//   }
  return AllocArray<byte>(size);
}

Heap* Heap::GetHeap() {
  if (heap_ == nullptr) {
    heap_ = Alloc<Heap>(kDefaultSize);
  }
  return heap_;
}

Heap* Heap::heap_ = nullptr;

void Heap::Grow() {
  auto before_size = size_;
  size_ *= 2;
  auto n = reinterpret_cast<Address>(MMap(size_));
  memcpy(n, arena_, before_size);
  munmap(arena_, before_size);
  arena_ = n;
#ifdef DEBUG
  mprotect(arena_, used_, PROT_READ | PROT_WRITE | PROT_EXEC);
#endif
}

const size_t Heap::kDefaultSize;
}  // namespace lux
