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
typedef uint64_t Pointer;
#elif defined(PLATFORM_32BIT)
typedef uint32_t Pointer;
#endif

static const size_t kAlignment = sizeof(void*);
static const size_t kPointerSize = kAlignment;
static const size_t kSizeTSize = sizeof(size_t);


#define LUX_ALIGN_OFFSET(offset, alignment)           \
  (offset + (alignment - 1)) & ~(alignment - 1)

#ifdef DEBUG
inline void Invalidate(bool cond, const char* message) {
  if (!cond) {
    printf("%s is not valid\n", message);
    lux::debug::StackTrace st;
    st.Print();
    exit(1);
  }
}
#define INVALIDATE(cond) lux::Invalidate(cond, #cond)
#define FATAL(message) lux::Invalidate(false, message);
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
class Bitset {
 public:
  void set(uint32_t index) {
    bit_field_ |= (~(0x1 << index));
  }

  void assign(uint32_t bit_value) {
    bit_field_ = bit_value;
  }

  bool get(uint32_t index) const {
    return bit_field_ & (~(0x1 << index));
  }

 private:
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

template <typename T>
using Shared = std::shared_ptr<T>;

typedef uint8_t byte;

typedef byte* Address;

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
