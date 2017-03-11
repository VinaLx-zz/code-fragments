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
    template <
        typename std::enable_if_t<
            std::is_same<std::result_of_t<Closure()>, A>::value, int> = 0>
    IO(Closure&& closure) : closure_(std::move(closure)) {}

    A Run() const {
        return closure_();
    }

    template <
        typename V, std::enable_if_t<!std::is_same<V, void>::value, int> = 0>
    V RunImpl() const {
        return closure_();
    }

    template <
        typename Func,
        std::enable_if_t<
            detail::Helper<
                tour_of_fp::io::IO, std::result_of_t<Func(A)>>::value,
            int> = 0>
    auto Bind(Func f);

    template <typename Func>
    auto Fmap(Func f);

  private:
    Closure closure_;
};

template <typename Closure>
class IO<void, Closure> {
  public:
    template <
        typename std::enable_if_t<
            std::is_same<std::result_of_t<Closure()>, void>::value, int> = 0>
    IO(Closure&& closure) : closure_(std::move(closure)){};
    template <
        typename Func,
        std::enable_if_t<
            detail::Helper<tour_of_fp::io::IO, std::result_of_t<Func()>>::value,
            int> = 0>
    auto Bind(Func f);

    template <typename Func>
    auto Fmap(Func f);

    void Run() const {
        closure_();
    }

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
template <
    typename Func,
    std::enable_if_t<
        detail::Helper<tour_of_fp::io::IO, std::result_of_t<Func(A)>>::value,
        int>>
auto IO<A, Closure>::Bind(Func f) {
    return Delay(
        [ a = IO<A, Closure>(*this), f ]() { return f(a.Run()).Run(); });
}

template <typename Closure>
template <
    typename Func,
    std::enable_if_t<
        detail::Helper<tour_of_fp::io::IO, std::result_of_t<Func()>>::value,
        int>>
auto IO<void, Closure>::Bind(Func f) {
    return Delay([ a = IO<void, Closure>(*this), f ]() {
        a.Run();
        f().Run();
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

inline auto ReadLine() {
    return Delay([]() {
        std::string s;
        std::getline(std::cin, s);
        return s;
    });
}

inline auto PrintLine(const std::string& s) {
    return Delay([s]() { std::cout << s << '\n'; });
}

}  // namespace tour_of_fp::io

#endif
