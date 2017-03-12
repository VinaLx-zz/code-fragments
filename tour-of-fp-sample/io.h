#ifndef TOUR_OF_FP_IO_H_
#define TOUR_OF_FP_IO_H_

#include <iostream>
#include <string>
#include <type_traits>
#include <utility>

namespace tour_of_fp::io {

namespace detail {

template <template <typename, typename> class T, typename A>
struct Helper {
    static constexpr bool value = false;
};

template <template <typename, typename> class T, typename A, typename Ignore>
struct Helper<T, T<A, Ignore>> {
    using type = A;
    static constexpr bool value = true;
};

}  // namespace detail

template <typename A, typename Closure>
class IO {
  public:
    IO(Closure&& closure) : closure_(std::move(closure)) {
        static_assert(
            std::is_same<std::result_of_t<Closure()>, A>::value,
            "Function must have signature A()");
    }

    A Run() const {
        return closure_();
    }

    template <typename Func>
    auto Bind(Func f);

    template <typename Func>
    auto Fmap(Func f);

  private:
    Closure closure_;
};

template <typename Closure>
class IO<void, Closure> {
  public:
    IO(Closure&& closure) : closure_(std::move(closure)) {
        static_assert(
            std::is_same<std::result_of_t<Closure()>, void>::value,
            "Function must have signature void()");
    };

    template <
        typename std::enable_if_t<
            std::is_same<std::result_of_t<Closure()>, void>::value, int> = 0>
    IO(const Closure& closure) : closure_(closure){};

    template <typename Func>
    auto Bind(Func f) const;

    template <typename Func>
    auto Fmap(Func f);

    void Run() const {
        closure_();
    }

    auto Infinite() const;

  private:
    Closure closure_;
};

template <typename Closure>
auto Delay(Closure&& closure) {
    using Result = std::result_of_t<Closure()>;
    return IO<Result, Closure>(std::move(closure));
}

template <typename A>
auto Unit(const A& a) {
    return Delay([a]() { return a; });
}

template <typename A, typename Closure>
template <typename Func>
auto IO<A, Closure>::Bind(Func f) {
    static_assert(
        detail::Helper<tour_of_fp::io::IO, std::result_of_t<Func(A)>>::value,
        "Function must Have signature IO<T>(A)");
    return Delay(
        [ a = IO<A, Closure>(*this), f ]() { return f(a.Run()).Run(); });
}

template <typename Closure>
template <typename Func>
auto IO<void, Closure>::Bind(Func f) const {
    static_assert(
        detail::Helper<tour_of_fp::io::IO, std::result_of_t<Func()>>::value,
        "Function must Have signature IO<T>()");
    return Delay([ a = IO<void, Closure>(*this), f ]() {
        a.Run();
        return f().Run();
    });
}

template <typename A, typename Closure>
template <typename Func>
auto IO<A, Closure>::Fmap(Func f) {
    return Bind([f](const A& a) { return Unit(f(a)); });
}

template <typename Closure>
template <typename Func>
auto IO<void, Closure>::Fmap(Func f) {
    return Bind([f]() { return Unit(f()); });
}

template <typename Closure>
auto IO<void, Closure>::Infinite() const {
    return Delay([io = IO<void, Closure>(*this)]() {
        for (;;) {
            io.closure_();
        }
    });
}

inline auto Nothing() {
    return Delay([]() {});
}

template <typename A>
inline auto Read() {
    return Delay([]() {
        A a;
        std::cin >> a;
        return a;
    });
}

inline auto ReadLine() {
    return Delay([]() -> std::string {
        std::string s;
        std::getline(std::cin, s);
        return s;
    });
}

template <typename A>
inline auto Print(const A& a) {
    return Delay([a]() { std::cout << a; });
}

template <typename A>
inline auto PrintLine(const A& a) {
    return Delay([a]() { std::cout << a << '\n'; });
}

}  // namespace tour_of_fp::io

#endif
