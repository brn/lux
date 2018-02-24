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

#ifndef SRC_MAYBE_H_
#define SRC_MAYBE_H_

#include <type_traits>
#include <utility>
#include "./utils.h"

namespace lux {

template <bool is_pointer, typename T>
class Result {};

// Value container for Maybe.
// value requirements:
//   T must be copy constructible.
//   T must be move constructible.
//
template <typename T>
class Result<false, T> {
 public:
  // Raw type of T.
  typedef typename std::remove_reference<
   typename std::remove_const<T>::type>::type Type;

  // Raw const type of T.
  typedef const Type ConstType;


  // Concept check.
  static_assert(std::is_move_constructible<T>::value,
                "The type T of Result<T> must be move constructible.");
  static_assert(std::is_copy_constructible<T>::value,
                "The type T of Result<T> must be copy constructible.");

  // Just constructor.
  LUX_INLINE explicit Result(Type value) {
    value_(value);
  }

  // Just(move) constructor
  LUX_INLINE explicit Result(Type&& value) {
    value_(std::move(value));
  }

  // Nothing constructor.
  Result() {}

  // Copy constructor.
  LUX_INLINE Result(const Result<false, T>& result) {
    if (!result.exists()) {return;}
    value_(result.value());
  }

  // Move constructor.
  LUX_INLINE Result(Result<false, T>&& result) {
    if (!result.exists()) {return;}
    value_(std::move(result.value()));
  }

  // Copy constructor for U that is compatible type of T.
  template <typename U>
  LUX_INLINE Result(const Result<false, U>& result) {
    static_assert(
        std::is_base_of<T, U>::value,
        "The value of Result<U> must be "
        "the derived class of The value of Result<T>.");
    if (!result.exists()) {
      return;
    }
    value_(result.value());
  }

  // Move constructor for U that is compatible type of T.
  template <typename U>
  LUX_INLINE Result(Result<false, U>&& result) {
    static_assert(std::is_base_of<T, U>::value,
                  "The value of Result<U> must be "
                  "the derived class of The value of Result<T>.");
    if (!result.exists()) {return;}
    value_(std::move(result.value()));
  }

  // Copy assginment operator.
  LUX_INLINE Result& operator = (const Result<false, T>& result) {
    if (!result.exists()) {return; *this;}
    value_(result.value());
    return *this;
  }

  // Move assginment operator.
  LUX_INLINE Result& operator = (Result<false, T>&& result) {
    if (!result.exists()) {return; *this;}
    value_(std::move(result.value()));
    return *this;
  }

  // Copy assginment operator for U that is compatible type of T.
  template <typename U>
  LUX_INLINE Result& operator = (const Result<false, U>& result) {
    static_assert(std::is_base_of<T, U>::value,
                  "The value of Result<U> must be "
                  "the derived class of The value of Result<T>.");
    if (!result.exists()) {return; *this;}
    value_(result.value());
    return *this;
  }

  // Move assginment operator for U that is compatible type of T.
  template <typename U>
  LUX_INLINE Result& operator = (Result<false, U>&& result) {
    static_assert(std::is_base_of<T, U>::value,
                  "The value of Result<U> must be "
                  "the derived class of The value of Result<T>.");
    if (!result.exists()) {return; *this;}
    value_(std::move(result.value()));
    return *this;
  }

  // Getter for the value_.
  LUX_GETTER(Type&, value, *value_)

  // Const getter for the value_.
  LUX_CONST_GETTER(ConstType&, value, *value_)

  // Return Result<T> is Just or not.
  LUX_CONST_GETTER(bool, exists, value_.initialized())

 private:
  LazyInitializer<Type> value_;
};



template <typename T>
class Result<true, T> {
 public:
  typedef T Type;

  typedef const typename std::remove_const<T>::type ConstType;

  LUX_INLINE explicit Result(T ptr)
      : ptr_(ptr) {}

  LUX_INLINE Result()
      : ptr_(nullptr) {}

  LUX_INLINE Result(const Result<true, T>& result)
      : ptr_(result.value()) {}

  LUX_INLINE Result(Result<true, T>&& result)
      : ptr_(result.value()) {}

  template <typename U>
  LUX_INLINE Result(const Result<true, U>& result)
      : ptr_(result.value()) {}


  template <typename U>
  LUX_INLINE Result(Result<true, U>&& result)
      : ptr_(result.value()) {}

  LUX_INLINE Result& operator = (const Result<true, T>& result) {
    ptr_ = result.value();
    return *this;
  }

  LUX_INLINE Result& operator = (Result<true, T>&& result) {
    ptr_ = result.value();
    return *this;
  }

  template <typename U>
  LUX_INLINE Result& operator = (const Result<true, U>& result) {
    ptr_ = result.value();
    return *this;
  }

  template <typename U>
  LUX_INLINE Result& operator = (Result<true, U>&& result) {
    ptr_ = result.value();
    return *this;
  }

  LUX_GETTER(T, value, ptr_)

  LUX_CONST_GETTER(const T, value, ptr_)

  LUX_CONST_GETTER(bool, exists, ptr_ != nullptr)

 private:
  T ptr_;
};

template <typename T>
class Maybe {
  template <typename Other>
  friend class Maybe;

  typedef Result<std::is_pointer<T>::value, T> SpecializedResult;
  typedef typename Result<std::is_pointer<T>::value, T>::Type ResultType;
  typedef typename Result<std::is_pointer<T>::value, T>::ConstType
  ConstResultType;

 public:
  LUX_INLINE explicit Maybe(T target) {
    result_(target);
  }

  template <typename R>
  LUX_INLINE explicit Maybe(Maybe<R> target) {
    if (target) {
      result_(target.value());
    } else {
      result_();
    }
  }

  LUX_INLINE Maybe() {
    result_();
  }

  LUX_INLINE Maybe(const Maybe& maybe) {
    result_(maybe.result());
  }

  template <typename Other>
  LUX_INLINE Maybe(const Maybe<Other>& maybe) {
    result_(maybe.result());
  }

  LUX_INLINE Maybe(Maybe&& maybe) {
    result_(std::move(maybe.result()));
  }

  template <typename Other>
  LUX_INLINE Maybe(Maybe<Other>&& maybe) {
    result_(std::move(maybe.result()));
  }

  LUX_INLINE Maybe<T>& operator = (const Maybe& maybe) {
    result_(maybe.result());
    return *this;
  }

  template <typename Other>
  LUX_INLINE Maybe<T>& operator = (const Maybe<Other>& maybe) {
    result_(maybe.result());
    return *this;
  }

  LUX_INLINE Maybe<T>& operator = (Maybe&& maybe) {
    result_(maybe.result());
    return *this;
  }

  template <typename Other>
  LUX_INLINE Maybe<T>& operator = (Maybe<Other>&& maybe) {
    result_(maybe.result());
    return *this;
  }

  LUX_INLINE ResultType value() {
    return result_->value();
  }

  LUX_INLINE ConstResultType value() const {
    return result_->value();
  }

  LUX_INLINE operator bool() {
    return just();
  }

  template <typename C>
  LUX_INLINE Maybe<C> Type() {
    return Maybe<C>(result_->value());
  }

  LUX_INLINE bool just() const {
    return result_->exists();
  }

  LUX_INLINE bool nothing() const {
    return !just();
  }

  LUX_INLINE ResultType operator ||(ResultType&& alternate) {
    if (!result_->exists()) {
      return std::move(alternate);
    }
    return value();
  }

  LUX_INLINE ResultType operator ||(ResultType&& alternate) const {
    if (!result_->exists()) {
      return std::move(alternate);
    }
    return value();
  }

  LUX_INLINE ResultType operator ||(const ResultType& alternate) {
    if (!result_->exists()) {
      return alternate;
    }
    return value();
  }

  LUX_INLINE ResultType operator ||(const ResultType& alternate) const {
    if (!result_->exists()) {
      return alternate;
    }
    return value();
  }

  template <typename Fn>
  LUX_INLINE auto bind(Fn fn);

  template <typename Fn>
  LUX_INLINE auto bind(Fn fn) const;

  template <typename Type, typename Fn>
  LUX_INLINE Maybe<Type> bind(Fn fn);

  template <typename Type, typename Fn>
  LUX_INLINE Maybe<Type> bind(Fn fn) const;

  template <typename Fn>
  LUX_INLINE auto operator >>= (Fn fn) {
    return bind(fn);
  }

  template <typename Fn>
  LUX_INLINE auto operator >>= (Fn fn) const {
    return bind(fn);
  }

 private:
  LUX_INLINE const SpecializedResult& result() const {
    return *result_;
  }

  LUX_INLINE SpecializedResult& result() {
    return *result_;
  }

  LazyInitializer<SpecializedResult> result_;
};

template <typename Ret>
struct Bind {
  template <typename T, typename Fn>
  LUX_INLINE Maybe<Ret> operator()(bool just, T value, Fn fn) {
    if (just) {
      return Maybe<Ret>(fn(value));
    }
    return Maybe<Ret>();
  }

  template <typename T, typename Fn>
  LUX_INLINE Maybe<Ret> operator()(bool just, T value, Fn fn) const {
    if (just) {
      return Maybe<Ret>(fn(value));
    }
    return Maybe<Ret>();
  }
};

template <>
struct Bind<void> {
  template <typename T, typename Fn>
  LUX_INLINE void operator()(bool just, T value, Fn fn) {
    if (just) {
      fn(value);
    }
  }

  template <typename T, typename Fn>
  LUX_INLINE void operator()(bool just, T value, Fn fn) const {
    if (just) {
      fn(value);
    }
  }
};

template <typename Ret>
struct Bind<Maybe<Ret>> {
  template <typename T, typename Fn>
  LUX_INLINE Maybe<Ret> operator()(bool just, T value, Fn fn) {
    if (just) {
      return Maybe<Ret>(fn(value));
    }
    return Maybe<Ret>();
  }

  template <typename T, typename Fn>
  LUX_INLINE Maybe<Ret> operator()(bool just, T value, Fn fn) const {
    if (just) {
      return Maybe<Ret>(fn(value));
    }
    return Maybe<Ret>();
  }
};

template <typename T>
template <typename Fn>
LUX_INLINE auto Maybe<T>::bind(Fn fn) {
  Bind<decltype(fn(std::declval<T>()))> bind;
  return bind(just(), value(), fn);
}

template <typename T>
template <typename Fn>
LUX_INLINE auto Maybe<T>::bind(Fn fn) const {
  Bind<decltype(fn(std::declval<T>()))> bind;
  return bind(just(), value(), fn);
}

template <typename T>
template <typename Type, typename Fn>
LUX_INLINE Maybe<Type> Maybe<T>::bind(Fn fn) {
  Bind<decltype(fn(std::declval<T>()))> bind;
  return bind(just(), value(), fn);
}

template <typename T>
template <typename Type, typename Fn>
LUX_INLINE Maybe<Type> Maybe<T>::bind(Fn fn) const {
  Bind<decltype(fn(std::declval<T>()))> bind;
  return bind(just(), value(), fn);
}

template <typename T>
Maybe<T> Just(T t) {
  return Maybe<T>(t);
}


template <typename T>
Maybe<T> Nothing() {
  return Maybe<T>();
}

}  // namespace lux

#endif  // SRC_MAYBE_H_
