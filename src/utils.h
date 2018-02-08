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
#define FATAL(message) lux::Invalidate__(false, message, false);
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


template <size_t size, typename T>
struct BitUtil {};

template <>
struct BitUtil<sizeof(uint8_t), uint8_t> {
  static int count(uint8_t v) {
    uint32_t count = (v & 0x55) + ((v >> 1) & 0x55);
    count = (count & 0x33) + ((count >> 2) & 0x33);
    return (count & 0x0f) + ((count >> 4) & 0x0f);
  }

  static bool msb(uint8_t v, int* out) {
    if (v == 0) return false;
    v |= (v >> 1);
    v |= (v >> 2);
    v |= (v >> 4);
    *out = count(v) - 1;
    return true;
  }
};

template <>
struct BitUtil<sizeof(uint16_t), uint16_t> {
  static int count(uint16_t v) {
    uint16_t count = (v & 0x5555) + ((v >> 1) & 0x5555);
    count = (count & 0x3333) + ((count >> 2) & 0x3333);
    count = (count & 0x0f0f) + ((count >> 4) & 0x0f0f);
    return (count & 0x00ff) + ((count >> 8) & 0x00ff);
  }

  static bool msb(uint16_t v, int* out) {
    if (v == 0) return false;
    v |= (v >> 1);
    v |= (v >> 2);
    v |= (v >> 4);
    v |= (v >> 8);
    *out = count(v) - 1;
    return true;
  }
};

template <>
struct BitUtil<sizeof(uint32_t), uint32_t> {
  static int count(uint32_t v) {
    uint32_t count = (v & 0x55555555) + ((v >> 1) & 0x55555555);
    count = (count & 0x33333333) + ((count >> 2) & 0x33333333);
    count = (count & 0x0f0f0f0f) + ((count >> 4) & 0x0f0f0f0f);
    count = (count & 0x00ff00ff) + ((count >> 8) & 0x00ff00ff);
    return (count & 0x0000ffff) + ((count >> 16) & 0x0000ffff);
  }

  static bool msb(uint32_t v, int* out) {
    if (v == 0) return false;
    v |= (v >> 1);
    v |= (v >> 2);
    v |= (v >> 4);
    v |= (v >> 8);
    v |= (v >> 16);
    *out = count(v) - 1;
    return true;
  }
};

template <>
struct BitUtil<sizeof(uint64_t), uint64_t> {
  static int count(uint64_t v) {
    uint64_t count
      = (v & 0x5555555555555555) + ((v >> 1) & 0x5555555555555555);
    count = (count & 0x3333333333333333) + ((count >> 2) & 0x3333333333333333);
    count = (count & 0x0f0f0f0f0f0f0f0f) + ((count >> 4) & 0x0f0f0f0f0f0f0f0f);
    count = (count & 0x00ff00ff00ff00ff) + ((count >> 8) & 0x00ff00ff00ff00ff);
    count = (count & 0x0000ffff0000ffff) + ((count >> 16) & 0x0000ffff0000ffff);
    return static_cast<int>(((count & 0x00000000ffffffff)
                             + ((count >> 32) & 0x00000000ffffffff)));
  }

  static bool msb(uint64_t v, int* out) {
    if (v == 0) return false;
    v |= (v >> 1);
    v |= (v >> 2);
    v |= (v >> 4);
    v |= (v >> 8);
    v |= (v >> 16);
    v |= (v >> 32);
    *out = count(v) - 1;
    return true;
  }
};

template <typename T>
class Bitset {
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
    return bit_field_ & (0x1 << index);
  }

  bool is_full() const {
    return bit_field_ == (~static_cast<T>(0));
  }

  template <typename R = T>
  R mask(T mask) const {
    return static_cast<R>(bit_field_ & mask);
  }

  uint32_t RightMostEmptySlot() {
    int out = 0;
    if (BitUtil<sizeof(T), T>::msb(bit_field_ ^ (bit_field_ + 1), &out)) {
      return out;
    }
    return 0;
  }

 protected:
  T bit_field_;
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

class Double {
 public:
  static const uint8_t kSignedShift = 63;
  static const uint8_t kExponentShift = 52;
  static const uint8_t kExponentMask = 1 << 1;
  static const uint64_t kFractionMask = uint64_t(~0) >> 12;
  static const uint16_t kMaxExponent = uint16_t(~0) >> 5;
  static const uint16_t kBias = 1023;

  static const double kNaN;

  explicit Double(double value) {
    value_.floating_value = value;
  }

  inline bool is_signed() const {
    return (value_.bit >> kSignedShift) == 1;
  }

  inline uint16_t exponent() const {
    return ((value_.bit >> kExponentShift) & kExponentMask) - kBias;
  }

  inline uint64_t fraction() const {
    return (value_.bit & kFractionMask) + 1;
  }

  inline bool Equals(const Double& d) const {
    return d.is_signed() == is_signed() &&
      d.exponent() == exponent() &&
      (d.fraction() > fraction()? d.fraction() - fraction():
      fraction() - d.fraction()) <= 4;
  }

  inline bool GreaterThan(const Double& d) const {
    if (!is_signed() && d.is_signed()) {
      return true;
    }
    return exponent() > d.exponent()
      || (fraction() - d.fraction()) > 4;
  }

  inline bool IsNaN() const {
    return exponent() == kMaxExponent
      && fraction() > 0;
  }

  inline double value() const {
    return value_.floating_value;
  }

 private:
  union Value {
    double floating_value;
    uint64_t bit;
  };

 private:
  Value value_;
};
}  // namespace lux

#endif  // SRC_UTILS_H_
