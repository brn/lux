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

#include <stdlib.h>
#include <string.h>
#include <utility>
#include "./zone.h"

namespace lux {
ZoneAllocator::ZoneAllocator()
    : chunk_(std::make_shared<Chunk>(kDefaultSize)) {}

ZoneAllocator::Chunk::Chunk(size_t size)
    : size_(size), remains_(size_) {
  AllocateInternal();
}

ZoneAllocator::Chunk::~Chunk() {
  INVALIDATE(top_ != nullptr);
  delete top_;
  top_ = nullptr;
}

Address ZoneAllocator::Chunk::Allocate(size_t size) {
  INVALIDATE(HasEnoughCapacity(size));
  INVALIDATE(*addr_ == 0xCE);
  remains_ -= size;
  auto ret = addr_;
  addr_ += size;
  return ret;
}

void ZoneAllocator::Chunk::AllocateInternal() {
  top_ = addr_ = new byte[kDefaultSize];
#ifdef DEBUG
  memset(addr_, 0xCE, size_);
#endif
}

Address ZoneAllocator::Allocate(size_t size) {
  size_t allocate_size = kDefaultSize;
  if (size > kDefaultSize) {
    Grow(size);
  }
  auto target_size = LUX_ALIGN_OFFSET(allocate_size, kAlignment);
  if (chunk_->HasEnoughCapacity(target_size)) {
    return chunk_->Allocate(target_size);
  }
  Grow(kDefaultSize);
  return chunk_->Allocate(target_size);
}

void ZoneAllocator::Grow(size_t size) {
  auto next = std::make_shared<Chunk>(size);
  next->set_next(chunk_);
  chunk_ = next;
}

const int32_t ZoneAllocator::kDefaultSize;

void* Zone::operator new(size_t size, ZoneAllocator* zone_allocator) {
  return zone_allocator->Allocate(size);
}

void Zone::operator delete(void* ptr) {
  UNREACHABLE();
}

void Zone::operator delete(void* ptr, ZoneAllocator* zone_allocator) {
  UNREACHABLE();
}
}  // namespace lux
