#ifndef MFUNCTION_H
#define MFUNCTION_H

#include <cassert>
#include <cstddef>
#include <utility>

template <typename>
class Mfunction;

// A move-only std::function.
template <typename R, typename... Ps>
class Mfunction<R(Ps...)> {
 public:
  constexpr Mfunction() noexcept = default;
  template <typename T>
  Mfunction(T &&Func) noexcept
      : Func(reinterpret_cast<std::byte *>(
            new std::decay_t<T>(std::forward<T>(Func)))),
        Caller([](std::byte *Ptr, Ps &&...Args) -> decltype(auto) {
          return (*reinterpret_cast<std::decay_t<T> *>(Ptr))(
              std::forward<Ps>(Args)...);
        }),
        Deleter([](std::byte *Ptr) noexcept {
          delete reinterpret_cast<std::decay_t<T> *>(Ptr);
        }) {}
  constexpr Mfunction(Mfunction &&Other) noexcept
      : Func(std::exchange(Other.Func, nullptr)),
        Caller(std::exchange(Other.Caller, nullptr)),
        Deleter(std::exchange(Other.Deleter, nullptr)) {}
  constexpr Mfunction &operator=(Mfunction &&Other) noexcept {
    Func = std::exchange(Other.Func, nullptr);
    Caller = std::exchange(Other.Caller, nullptr);
    Deleter = std::exchange(Other.Deleter, nullptr);
    return *this;
  }
  ~Mfunction() {
    if (Deleter) Deleter(Func);
  }

  template <typename... Ts>
  constexpr decltype(auto) operator()(Ts &&...Args) noexcept {
    assert(Caller && "can't call null function");
    return Caller(Func, std::forward<Ts>(Args)...);
  }

 private:
  std::byte *Func = nullptr;
  R (*Caller)(std::byte *, Ps &&...) = nullptr;
  void (*Deleter)(std::byte *) = nullptr;
};

template <typename R, typename ...Ts>
Mfunction(R(*Func)(Ts&&...)) -> Mfunction<R(Ts...)>;

#endif
