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
#include "../heap.h"
#include "../isolate.h"
#include "../unicode.h"
#include "../utils.h"
#include "./heap_object.h"
#include "./object.h"
#include "./shape.h"

namespace lux {
class Utf16CodePoint;
class BytecodeArray;
class BytecodeConstantArray;

#define JSOBJECT_FORWARD_DECL(NAME, Name, p) class Name;
OBJECT_TYPES(JSOBJECT_FORWARD_DECL)
#undef JSOBJECT_FORWARD_DECL

class BytecodeExecutable : public HeapObject {
 public:
  static const size_t kSize;
  enum {
    kOffset = HeapObject::kHeapObjectOffset,
    kBytecodeArrayOffset = kOffset,
    kBytecodeArraySize = kPointerSize,
    kBytecodeConstantPoolOffset = kBytecodeArrayOffset + kBytecodeArraySize,
    kBytecodeConstantPoolSize = kPointerSize,
  };

  static Handle<BytecodeExecutable> New(Isolate* isolate,
                                        BytecodeArray* bytecode_array,
                                        BytecodeConstantArray* constant_pool);

  BytecodeArray* bytecode_array() const {
    return *(reinterpret_cast<BytecodeArray**>(
        FIELD_ADDR(this, kBytecodeArrayOffset)));
  }

  BytecodeConstantArray* constant_pool() const {
    return *(reinterpret_cast<BytecodeConstantArray**>(
        FIELD_ADDR(this, kBytecodeConstantPoolOffset)));
  }

#ifdef DEBUG
  std::string ToString();
#endif
};

class FixedArrayBase : public HeapObject {
 public:
  enum {
    kFixedArrayLengthOffset = HeapObject::kHeapObjectOffset,
    kFixedArrayLengthSize = sizeof(uint32_t),
    kFixedArrayPtrOffset = kFixedArrayLengthOffset + kFixedArrayLengthSize,
    kSize = kFixedArrayLengthSize
  };

  uint32_t size() const {
    return *reinterpret_cast<uint32_t*>(
        FIELD_ADDR(this, kFixedArrayLengthOffset));
  }

  Address* data() const {
    return reinterpret_cast<Address*>(FIELD_ADDR(this, kFixedArrayPtrOffset));
  }

 protected:
  static FixedArrayBase* NewArray(Isolate* isolate, uint32_t size);
};

template <typename T, typename U, size_t object_size>
class GenericFixedArray;

template <typename T>
class FixedArrayIterator : public std::iterator<std::forward_iterator_tag, T> {
  template <typename Tg, typename Ug, size_t object_size>
  friend class GenericFixedArray;

 public:
  FixedArrayIterator() : index_(0), array_(nullptr) {}
  FixedArrayIterator(const T* array, uint32_t length)
      : index_(0), length_(length), array_(array) {}

  const T& operator*() const { return array_[index_]; }
  const T* operator->() const { return array_[index_]; }
  FixedArrayIterator<T>& operator=(const FixedArrayIterator<T>& it) {
    index_ = it.index_;
    length_ = it.length_;
    array_ = it.array_;
    return *this;
  }

  LUX_INLINE const bool operator==(const FixedArrayIterator& it) {
    return index_ == it.index_ && array_ == it.array_ && length_ == it.length_;
  }

  LUX_INLINE const bool operator!=(const FixedArrayIterator& it) {
    return !(this->operator==(it));
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
class GenericFixedArray : public FixedArrayBase {
 public:
  using iterator = FixedArrayIterator<T>;
  static Handle<U> New(Isolate* isolate, uint32_t size) {
    auto a = reinterpret_cast<U*>(NewArray(isolate, size * object_size));
    return make_handle(a);
  }

  static U* NewWithoutHandle(Isolate* isolate, uint32_t size) {
    auto a = reinterpret_cast<U*>(NewArray(isolate, size * object_size));
    return a;
  }

  void Fill(T* value) {
    for (int i = 0; i < length(); i++) {
      write(i, value);
    }
  }

  void Copy(GenericFixedArray<T, U, object_size>* arr) {
    auto len = arr->length() > length() ? arr->length() : length();
    for (int i = 0; i < len; i++) {
      write(i, arr->at(i));
    }
  }

  void Copy(GenericFixedArray<T, U, object_size>* arr, int start, int end) {
    auto len = end > length() ? length() : end;
    for (int i = start; i < len; i++) {
      write(i, arr->at(i));
    }
  }

  uint32_t length() const { return size() / object_size; }

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
    return FixedArrayIterator<T>(reinterpret_cast<T*>(data()), length(),
                                 length());
  }

  LUX_INLINE FixedArrayIterator<T> begin() {
    return FixedArrayIterator<T>(reinterpret_cast<T*>(data()), length());
  }

  LUX_INLINE FixedArrayIterator<T> end() {
    return FixedArrayIterator<T>(reinterpret_cast<T*>(data()), length(),
                                 length());
  }

  LUX_INLINE FixedArrayIterator<T> begin_with_size(uint32_t length) const {
    return FixedArrayIterator<T>(reinterpret_cast<T*>(data()), length);
  }

  LUX_INLINE FixedArrayIterator<T> end_with_size(uint32_t length) const {
    return FixedArrayIterator<T>(reinterpret_cast<T*>(data()), length, length);
  }

  LUX_INLINE FixedArrayIterator<T> begin_with_size(uint32_t length) {
    return FixedArrayIterator<T>(reinterpret_cast<T*>(data()), length);
  }

  LUX_INLINE FixedArrayIterator<T> end_with_size(uint32_t length) {
    return FixedArrayIterator<T>(reinterpret_cast<T*>(data()), length, length);
  }
};

class FixedArray : public GenericFixedArray<Object*, FixedArray, kPointerSize> {
 public:
  OBJECT_CAST(FixedArray*, Object*)
  OBJECT_CAST(const FixedArray*, const FixedArray*)
};

class U32FixedArray
    : public GenericFixedArray<Utf16CodePoint, U32FixedArray, kPointerSize> {
 public:
  OBJECT_CAST(U32FixedArray*, Object*)
  OBJECT_CAST(const U32FixedArray*, const U32FixedArray*)
};

class JSFunction : public HeapObject {
 public:
  enum {
    kOffset = HeapObject::kHeapObjectOffset,
    kLengthOffset = kOffset,
    kLengthSize = kOneByteSize,
    kNameOffset = kLengthOffset + kLengthSize,
    kNameSize = kPointerSize,
    kCodeOffset = kNameOffset + kNameSize,
    kCodeSize = kPointerSize,
    kSize = HeapObject::kSize + kCodeOffset + kCodeSize
  };

  static Handle<JSFunction> New(Isolate* isolate, JSString* name,
                                uint8_t length, BytecodeExecutable* be);

  Value<Object> Call(Isolate* isolate,
                     std::initializer_list<Object*> parameters);

  inline BytecodeExecutable* code() const {
    return *FIELD_PROPERTY(BytecodeExecutable**, this, kCodeOffset);
  }

  inline uint8_t length() const {
    return *FIELD_PROPERTY(uint8_t*, this, kCodeOffset);
  }

  inline JSString* name() const {
    return *FIELD_PROPERTY(JSString**, this, kCodeOffset);
  }

  OBJECT_CAST(JSFunction*, Object*)
  OBJECT_CAST(const JSFunction*, const JSFunction*)
};

class JSRegExp : public HeapObject {
 public:
  enum {
    kOffset = HeapObject::kHeapObjectOffset,
    kFlagOffset = kOffset,
    kFlagSize = sizeof(uint8_t),
    kCodeOffset = kFlagOffset + kFlagSize,
    kCodeSize = kPointerSize,
    kSize = kCodeOffset + kCodeSize,
  };
  static Handle<JSRegExp> New(Isolate* isolate, BytecodeExecutable* executable,
                              uint8_t flag);
  static JSRegExp* NewWithoutHandle(Isolate* isolate,
                                    BytecodeExecutable* executable,
                                    uint8_t flag);

  Value<JSSpecials> Test(Isolate* isolate, JSString*);
  Value<Object> Match(Isolate* isolate, JSString*);

  BytecodeExecutable* code() const {
    return *FIELD_PROPERTY(BytecodeExecutable**, this, kCodeOffset);
  }

 private:
  inline uint8_t flag() const {
    return *FIELD_PROPERTY(uint8_t*, this, kFlagOffset);
  }
};

class JSSpecials : public HeapObject {
 public:
  static const size_t kSize;
  static const uint8_t kSpecialsStartOffset = HeapObject::kHeapObjectOffset;
  static const uint8_t kSpecialsFlagSize = sizeof(uint8_t);

  enum Type { kFalse, kTrue, kUndefined, kNull };

  static Handle<JSSpecials> New(Isolate* isolate, Type type);
  static JSSpecials* NewWithoutHandle(Isolate* isolate, Type type);

  static JSSpecials* Cast(Object* o) {
    return reinterpret_cast<JSSpecials*>(o);
  }

  static const JSSpecials* Cast(const Object* o) {
    return reinterpret_cast<const JSSpecials*>(o);
  }

  inline static bool ToBoolean(Object* obj);

  inline bool ToBoolean() { return (type() & kTrue) == kTrue; }

  inline static bool IsNull(Object* obj) {
    return obj->IsHeapObject()
               ? JSSpecials::Cast(HeapObject::Cast(obj))->type() ==
                     JSSpecials::Type::kNull
               : false;
  }

  inline Type type() const {
    return static_cast<Type>(
        *reinterpret_cast<uint8_t*>(FIELD_ADDR(this, kSpecialsStartOffset)));
  }
};

class JSObject : public HeapObject {
 public:
  static const size_t kSize;

  static Object* GetLength(Isolate* isolate, Object* o);
};

class JSArray : public HeapObject {
 public:
  enum {
    kOffset = HeapObject::kHeapObjectOffset,
    kLengthOffset = kOffset,
    kLengthSize = sizeof(uint32_t),
    kPadding1Offset = kLengthOffset + kLengthSize,
    kPadding1Size = kPointerSize - kLengthSize,
    kElementOffset = kPadding1Offset + kPadding1Size,
    kElementSize = kElementOffset + kPointerSize,
    kSize = LUX_ALIGN_OFFSET(HeapObject::kSize + kElementOffset + kElementSize,
                             kPointerSize)
  };

  static Handle<JSArray> NewEmptyArray(Isolate* isolate, uint32_t length,
                                       size_t initial_capacity = kPointerSize *
                                                                 10);

  static Handle<JSArray> NewWithElement(Isolate* isolate, uint32_t length,
                                        FixedArray* elements);

  inline uint32_t length() const {
    return *reinterpret_cast<uint32_t*>(FIELD_ADDR(this, kLengthOffset));
  }

  inline FixedArray* element() const {
    return *reinterpret_cast<FixedArray**>(FIELD_ADDR(this, kElementOffset));
  }

  inline void Push(Isolate* isolate, Object* object);

  inline Object* Pop(Isolate* isolate);

  inline Object* Shift(Isolate* isolate);

  inline Object* Unshift(Isolate* isolate, Object* new_value);

  inline static JSArray* Cast(Object* o) {
    return reinterpret_cast<JSArray*>(o);
  }

 private:
  void set_length(uint32_t length) {
    auto r = reinterpret_cast<uint32_t*>(FIELD_ADDR(this, kLengthOffset));
    *r = length;
  }

  void set_element(FixedArray* arr) {
    auto ptr = reinterpret_cast<FixedArray**>(FIELD_ADDR(this, kElementOffset));
    *ptr = arr;
  }
};

class JSNumber : public HeapObject {
 public:
  enum {
    kOffset = HeapObject::kSize,
    kValueOffset = kOffset,
    kValueSize = sizeof(double),
    kSize = kValueOffset + kValueSize
  };

  OBJECT_CAST(JSNumber*, Object*)
  OBJECT_CAST(const JSNumber*, const Object*)

  static Handle<JSNumber> New(Isolate* isolate, double value);
  static JSNumber* NewWithoutHandle(Isolate* isolate, double value);

  static bool IsNaN(JSNumber* n) {
    Double d(n->value());
    return d.IsNaN();
  }

  static int64_t GetIntValue(Object* o) {
    INVALIDATE(o->IsSmi() || HeapObject::Cast(o)->shape()->IsJSNumber());
    if (o->IsSmi()) {
      return Smi::Cast(o)->value();
    }
    return JSNumber::Cast(o)->int_value();
  }

  bool IsNaN() { return IsNaN(this); }

  void set_value(double v) {
    (*FIELD_PROPERTY(double*, this, kValueOffset)) = v;
  }

  double value() const { return *FIELD_PROPERTY(double*, this, kValueOffset); }

  int64_t int_value() const { return static_cast<int64_t>(value()); }

  bool Equals(const JSNumber* number) const {
    Double a(value()), b(number->value());
    return a.Equals(b);
  }

  bool Equals(const Smi* smi) const {
    Double a(value()), b(static_cast<double>(smi->value()));
    return a.Equals(b);
  }

  bool GreaterThan(const JSNumber* number) const {
    Double a(value()), b(number->value());
    return a.GreaterThan(b);
  }

  bool GreaterThan(const Smi* smi) const {
    Double a(value()), b(static_cast<double>(smi->value()));
    return a.GreaterThan(b);
  }
};

class JSString : public HeapObject {
 public:
  enum {
    kStringStartOffset = HeapObject::kHeapObjectOffset,
    kStringLengthOffset = kStringStartOffset,
    kStringLengthSize = sizeof(uint32_t),
    kStringPtrOffset = kStringLengthOffset + kStringLengthSize
  };
  static const size_t kSize;

  using iterator = U32FixedArray::iterator;

  Handle<JSString> Clone(Isolate* isolate) const {
    return New(isolate, reinterpret_cast<Utf16CodePoint*>(data()->data()),
               length());
  }

  static JSString* Cast(Object* o) {
    auto h = HeapObject::Cast(o);
    INVALIDATE(h->shape()->instance_type() == InstanceType::JS_STRING);
    return reinterpret_cast<JSString*>(h);
  }

  static const JSString* Cast(const Object* o) {
    auto h = HeapObject::Cast(o);
    INVALIDATE(h->shape()->instance_type() == InstanceType::JS_STRING);
    return reinterpret_cast<const JSString*>(h);
  }

  class Utf8String {
   public:
    explicit Utf8String(JSString* str);
    Utf8String(Utf8String&& u8s)
        : length_(u8s.length_), buffer_(std::move(u8s.buffer_)) {}

    Utf8String(const Utf8String& u8s)
        : length_(u8s.length_), buffer_(u8s.buffer_) {}

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
  static Handle<JSString> New(Isolate* isolate, const Utf16CodePoint* p,
                              size_t length);
  static Handle<JSString> New(Isolate* isolate, size_t length);

  LUX_INLINE uint32_t length() const {
    return *reinterpret_cast<uint32_t*>(FIELD_ADDR(this, kStringLengthOffset));
  }

  LUX_INLINE Utf16CodePoint at(int index) const {
    INVALIDATE(index < length());
    return data()->at(index);
  }

  inline Handle<JSString> Slice(Isolate* isolate, uint32_t start, uint32_t end);

  bool Equals(const JSString* str) const;

  bool GreaterThan(const JSString* str) const;

  U32FixedArray::iterator begin() const {
    return data()->begin_with_size(length());
  }

  U32FixedArray::iterator end() const {
    return data()->end_with_size(length());
  }

  void write(int i, u32 v) { data()->write(i, Utf16CodePoint(v)); }

  void Append(Isolate* isolate, u32 v);

  friend class Utf8String;

 private:
  LUX_INLINE U32FixedArray* data() const {
    return *reinterpret_cast<U32FixedArray**>(
        FIELD_ADDR(this, kStringPtrOffset));
  }
};
}  // namespace lux

#include "./jsobject-inl.h"

#endif  // SRC_OBJECTS_JSOBJECT_H_
