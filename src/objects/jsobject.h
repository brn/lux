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

#ifndef SRC_OBJECTS_JSOBJECT_H_
#define SRC_OBJECTS_JSOBJECT_H_

#include <string>
#include <utility>
#include "./object.h"
#include "./heap_object.h"
#include "./shape.h"
#include "../heap.h"
#include "../unicode.h"

namespace lux {
class Isolate;
class Utf16CodePoint;

class FixedArrayBase: public HeapObject {
 public:
  enum {
    kFixedArrayLengthOffset = HeapObject::kHeapObjectOffset,
    kFixedArrayLengthSize = sizeof(uint32_t),
    kFixedArrayPtrOffset = kFixedArrayLengthOffset
    + kFixedArrayLengthSize
  };

  uint32_t size() const {
    return *reinterpret_cast<uint32_t*>(
        FIELD_ADDR(this, kFixedArrayLengthOffset));
  }

 protected:
  static FixedArrayBase* NewArray(Isolate* isolate, uint32_t size);

  Address* data() const {
    return reinterpret_cast<Address*>(
        FIELD_ADDR(this, kFixedArrayPtrOffset));
  }
};

template <typename T, typename U, size_t object_size>
class GenericFixedArray;

template <typename T>
class FixedArrayIterator:
      public std::iterator<std::forward_iterator_tag, T> {
  template <typename Tg, typename Ug, size_t object_size>
  friend class GenericFixedArray;
 public:
  FixedArrayIterator()
      : index_(0), array_(nullptr) {}
  FixedArrayIterator(const T* array, uint32_t length)
      : index_(0), length_(length), array_(array) {}

  const T& operator*() const {
    return array_[index_];
  }
  const T* operator->() const {
    return array_[index_];
  }
  FixedArrayIterator<T>& operator=(const FixedArrayIterator<T>& it) {
    index_ = it.index_;
    length_ = it.length_;
    array_ = it.array_;
    return *this;
  }

  LUX_INLINE const bool operator ==(const FixedArrayIterator& it) {
    return index_ == it.index_ && array_ == it.array_
      && length_ == it.length_;
  }

  LUX_INLINE const bool operator !=(const FixedArrayIterator& it) {
    return !this->operator==(it);
  }

  LUX_INLINE FixedArrayIterator& operator++() {
    if (index_ < length_) {
      index_++;
    }
    return *this;
  }

  LUX_INLINE FixedArrayIterator operator++(int value) {
    FixedArrayIterator tmp = *this;
    ++*this;
    return tmp;
  }

  LUX_INLINE FixedArrayIterator operator+(int value) {
    return FixedArrayIterator(array_, index_ + value, length_);
  }

  LUX_INLINE FixedArrayIterator& operator+=(int value) {
    index_ += value;
    return *this;
  }

 private:
  FixedArrayIterator(const T* array, uint32_t index, uint32_t length)
      : index_(index), length_(length), array_(array) {}

  uint32_t index_;
  uint32_t length_;
  const T* array_;
};

template <typename T, typename U, size_t object_size>
class GenericFixedArray: public FixedArrayBase {
 public:
  using iterator = FixedArrayIterator<T>;
  static Handle<U> New(Isolate* isolate, uint32_t size) {
    auto a = reinterpret_cast<U*>(NewArray(isolate, size * object_size));
    return make_handle(a);
  }

  uint32_t length() const {
    return size() / object_size;
  }

  void write(uint32_t index, T values, size_t size) {
    INVALIDATE(index + size < length());
    for (int i = index; i < size; i++) {
      write(i, values[i]);
    }
  }

  void write(uint32_t index, T value) {
    INVALIDATE(index < length());
    reinterpret_cast<T*>(data())[index] = value;
  }

  T at(uint32_t index) const {
    INVALIDATE(index < length());
    return reinterpret_cast<T*>(data())[index];
  }

  LUX_INLINE FixedArrayIterator<T> begin() const {
    return FixedArrayIterator<T>(reinterpret_cast<T*>(data()), length());
  }

  LUX_INLINE FixedArrayIterator<T> end() const {
    return FixedArrayIterator<T>(
        reinterpret_cast<T*>(data()), length(), length());
  }

  LUX_INLINE FixedArrayIterator<T> begin() {
    return FixedArrayIterator<T>(reinterpret_cast<T*>(data()), length());
  }

  LUX_INLINE FixedArrayIterator<T> end() {
    return FixedArrayIterator<T>(
        reinterpret_cast<T*>(data()), length(), length());
  }
};

class FixedArray: public GenericFixedArray<
  HeapObject*, FixedArray, kPointerSize> {};

class JSObject: public HeapObject {
 public:
  static const size_t kSize;
};

class JSArray: public HeapObject {
 public:
  static const size_t kSize;
};

class JSString: public HeapObject {
 public:
  enum {
    kStringStartOffset = HeapObject::kHeapObjectOffset,
    kStringLengthOffset = kStringStartOffset,
    kStringLengthSize = sizeof(uint32_t),
    kStringPtrOffset = kStringLengthOffset + kStringLengthSize
  };
  static const size_t kSize;

  static JSString* Cast(HeapObject* h) {
    INVALIDATE(h->shape()->instance_type()
               == InstanceType::JS_STRING);
    return reinterpret_cast<JSString*>(h);
  }

  class Utf8String {
   public:
    explicit Utf8String(JSString* str);
    Utf8String(Utf8String&& u8s)
        : length_(u8s.length_),
          buffer_(std::move(u8s.buffer_)) {}

    Utf8String(const Utf8String& u8s)
        : length_(u8s.length_),
          buffer_(u8s.buffer_) {}

    Utf8String& operator=(Utf8String u8s) noexcept {
      using std::swap;
      swap(length_, u8s.length_);
      buffer_.swap(u8s.buffer_);
      return *this;
    }

    Utf8String& operator=(Utf8String&& u8s) noexcept {
      using std::swap;
      swap(length_, u8s.length_);
      buffer_ = std::move(u8s.buffer_);
      return *this;
    }

    LUX_CONST_GETTER(const char*, value, buffer_.c_str());

    LUX_CONST_GETTER(size_t, length, length_)

   private:
    size_t length_;
    std::string buffer_;
  };

  static Handle<JSString> New(Isolate* isolate, const char* data);

  LUX_INLINE uint32_t length() const {
    return *reinterpret_cast<uint32_t*>(
        FIELD_ADDR(this, kStringLengthOffset));
  }

  LUX_INLINE Utf16CodePoint at(int index) const {
    INVALIDATE(index < length());
    return data()[index];
  }

  bool Equals(const JSString* str) const;

  friend class Utf8String;
 private:
  LUX_INLINE Utf16CodePoint* data() const {
    return reinterpret_cast<Utf16CodePoint*>(
        FIELD_ADDR(this, kStringPtrOffset));
  }
};
}  // namespace lux

#endif  // SRC_OBJECTS_JSOBJECT_H_
