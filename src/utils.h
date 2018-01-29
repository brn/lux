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

#ifndef SRC_UTILS_H_
#define SRC_UTILS_H_

#include <stdint.h>

#include <memory>

#include "./debug/stacktrace.h"

namespace lux {

#define KB * 1024
#define MB KB * 1024

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;

typedef uint8_t Byte;
#ifdef PLATFORM_64BIT
using Pointer = uint64_t;
using smi_t = uint64_t;
#elif defined(PLATFORM_32BIT)
using Pointer = uint32_t;
using smi_t = uint32_t;
#else
#error Unsupported platform
#endif

static const size_t kAlignment = sizeof(void*);
static const size_t kPointerSize = kAlignment;
static const size_t kSizeTSize = sizeof(size_t);


#define LUX_ALIGN_OFFSET(offset, alignment)           \
  (offset + (alignment - 1)) & ~(alignment - 1)

inline void Invalidate__(bool cond, const char* message, bool not_own_message) {
  if (!cond) {
      printf("========== ASSERTION FAILED ==========\n"
             "\n"
             "%s %s"
             "\n"
             "======================================\n"
             "\n"
             "            <Stack Trace>\n\n", message,
             (not_own_message? "is not valid\n": "\n"));
    lux::debug::StackTrace st;
    st.Print();
    exit(1);
  }
}

#define PRECONDITION(cond, message) Invalidate__(cond, message, false)
#define PRECONDITION_ASSERT(cond) Invalidate__(cond, #cond, true)

#ifdef DEBUG
#define INVALIDATE(cond) lux::Invalidate__(cond, #cond, true)
#define FATAL(message) lux::Invalidate__(false, message, true);
#define UNREACHABLE() FATAL("Unreachable block executed.");

#else
#define INVALIDATE(cond)
#define UNREACHABLE()
#endif

// The USE(x, ...) template is used to silence C++ compiler warnings
// issued for (yet) unused variables (typically parameters).
// The arguments are guaranteed to be evaluated from left to right.
struct Use__ {
  template <typename T>
  Use__(T&&) {}  // NOLINT(runtime/explicit)
};
#define USE(...)                                         \
  do {                                                   \
    lux::Use__ unused_tmp_array_for_use_macro[]{__VA_ARGS__};  \
    (void)unused_tmp_array_for_use_macro;                \
  } while (false)

/**
 * Inline macro.
 */
#if !defined(DEBUG) && defined(HAVE_FORCE_INLINE)
#define LUX_INLINE inline __forceinline __declspec(nothrow)
#elif !defined(DEBUG) && defined(HAVE_INLINE_ATTRIUTE)
#define LUX_INLINE inline __attribute__((always_inline))
#else
#define LUX_INLINE inline
#endif

template <typename T>
class LuxScoped__ {
 public:
  explicit LuxScoped__(T cb)
      : cb_(cb) {}

  ~LuxScoped__() {
    cb_();
  }

 private:
  T cb_;
};

#define LUX_SCOPED_INNER__L(exit_function, line)          \
  auto exit_function_of_scope##line = exit_function;      \
  LuxScoped__<decltype(exit_function_of_scope##line)>   \
  lux_scoped_once##line(exit_function_of_scope##line);
#define LUX_SCOPED_INNER__(exit_function, line) \
  LUX_SCOPED_INNER__L(exit_function, line);
#define LUX_SCOPED(exit_function)               \
  LUX_SCOPED_INNER__(exit_function, __LINE__)

#ifdef LUX_TEST
#define VISIBLE_FOR_TESTING public
#else
#define VISIBLE_FOR_TESTING private
#endif

#define LUX_GETTER(type, name, field)                    \
  LUX_INLINE type name() { return field; }

#define LUX_CONST_GETTER(type, name, field)            \
  LUX_INLINE type name() const { return field; }


#define LUX_SETTER(type, name, field)                              \
  LUX_INLINE void set_##name(type name) { field = name; }


#define LUX_PROPERTY(type, name, field)        \
  LUX_GETTER(type, name, field)                \
  LUX_SETTER(type, name, field)


#define LUX_CONST_PROPERTY(type, name, field)        \
  LUX_CONST_GETTER(type, name, field)                \
  LUX_SETTER(type, name, field)

template <typename T>
class BitsetBase {
 public:
  void set(uint32_t index) {
    bit_field_ |= (0x1 << index);
  }

  void unset(uint32_t index) {
    bit_field_ &= ~((0x1 << index));
  }

  void assign(uint32_t bit_value) {
    bit_field_ = bit_value;
  }

  bool get(uint32_t index) const {
    return bit_field_ & (~(0x1 << index));
  }

  bool is_full() const {
    return bit_field_ == ~static_cast<T>(0);
  }

  template <typename R = T>
  R mask(T mask) const {
    return static_cast<R>(bit_field_ & mask);
  }

 protected:
  T bit_field_;
};

template <typename T>
class Bitset: public BitsetBase<T> {
 public:
  LUX_INLINE uint32_t RightMostEmptySlot() {
    return Log2(BitsetBase<T>::bit_field_ & -BitsetBase<T>::bit_field_) - 1;
  }

 private:
  LUX_INLINE uint32_t Log2(T x) {
    if (x == 0) {
      return 0;
    }

    x |= (x >> 1);
    x |= (x >> 2);
    x |= (x >> 4);
    x |= (x >> 8);

    x -= ((x >> 1) & 0x5555);
    x = (((x >> 2) & 0x3333) + (x & 0x3333));
    x = (((x >> 4) +x) & (0x0f0f));
    x += (x >> 8);
    return (x & 0x3f) - 1;
  }
};

template <>
class Bitset<uint64_t>: public BitsetBase<uint64_t> {
 public:
  LUX_INLINE uint32_t RightMostEmptySlot() {
    return Log2(BitsetBase<uint64_t>::bit_field_
                & -BitsetBase<uint64_t>::bit_field_) - 1;
  }

 private:
  LUX_INLINE uint32_t Log2(uint64_t x) {
    static const int kTab64[64] = {
      63,  0, 58,  1, 59, 47, 53,  2,
      60, 39, 48, 27, 54, 33, 42,  3,
      61, 51, 37, 40, 49, 18, 28, 20,
      55, 30, 34, 11, 43, 14, 22,  4,
      62, 57, 46, 52, 38, 26, 32, 41,
      50, 36, 17, 19, 29, 10, 13, 21,
      56, 45, 25, 31, 35, 16,  9, 12,
      44, 24, 15,  8, 23,  7,  6,  5};

    x |= x >> 1;
    x |= x >> 2;
    x |= x >> 4;
    x |= x >> 8;
    x |= x >> 16;
    x |= x >> 32;
    return kTab64[static_cast<uint64_t>(
        (((x - (x >> 1)) * 0x07EDD5E59A4E28C2)) >> 58)];
  }
};

/**
 * Class traits.
 * Represent class which is not allowed to instantiation.
 */
class Static {
  Static() = delete;
  Static(const Static&) = delete;
  Static(Static&&) = delete;
  Static& operator = (const Static&) = delete;
};

template <typename T>
LUX_INLINE T PowerOf2(T x, uint32_t n) {
  T ret = 1;

  while (0 < n) {
    if ((n % 2) == 0) {
      x *= x;
      n >>= 1;
    } else {
      ret *= x;
      --n;
    }
  }

  return ret;
}

/**
 * Class traits.
 * Represent class which is not allowed to copy.
 */
class Uncopyable {
 public:
  Uncopyable() = default;
  virtual ~Uncopyable() = default;
  Uncopyable(const Uncopyable&) = delete;
  Uncopyable& operator = (const Uncopyable&) = delete;
};


class Unmovable {
 public:
  Unmovable() = default;
  virtual ~Unmovable() = default;
  Unmovable(Unmovable&&) = delete;
  Unmovable& operator = (Unmovable&&) = delete;
};

class StackObject {
 private:
  void* operator new(size_t size);
  void operator delete(void* ptr);
};

template <typename T>
using Shared = std::shared_ptr<T>;

typedef uint8_t byte;

typedef byte* Address;

static const size_t kOneByteSize = sizeof(byte);

template <int LowerBits, typename Type = uint32_t>
class Bitmask {
 public:
  static const Type full = ~(Type(0));
  static const Type upper = ~((Type(1) << LowerBits) - 1);
  static const Type lower = (Type(1) << LowerBits) - 1;
};

template <typename T>
class LazyInitializer {
 public:
  LazyInitializer()
      : ptr_(nullptr) {}

  ~LazyInitializer() {
    if (ptr_ != nullptr) {
      ptr_->~T();
    }
  }

  template <typename ... Args>
  T* operator()(Args ... args) {
    ptr_ = nullptr;
    return ptr_ = new(heap_) T(args...);
  }

  T* Get() {return ptr_;}

  const T* Get() const {return ptr_;}

  T& operator * () {
    return *ptr_;
  }

  const T& operator * () const {
    return *ptr_;
  }

  T* operator -> () {
    return ptr_;
  }


  const T* operator -> () const {
    return ptr_;
  }

  template <typename U>
  bool operator == (U u) const {
    return ptr_ == u;
  }

  template <typename U>
  bool operator != (U u) const {
    return ptr_ != u;
  }

  template <typename U>
  T& operator >> (U u) {
    return *ptr_ >> u;
  }

  template <typename U>
  const T& operator >> (U u) const {
    return *ptr_ >> u;
  }

  template <typename U>
  T& operator << (U u) {
    return *ptr_ << u;
  }

  template <typename U>
  const T& operator << (U u) const {
    return *ptr_ << u;
  }

  inline bool initialized() const {
    return ptr_ != nullptr;
  }

 private:
  Address heap_[sizeof(T) + 1];
  T* ptr_;
};
}  // namespace lux

#endif  // SRC_UTILS_H_
