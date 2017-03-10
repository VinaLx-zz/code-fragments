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

template <typename F, typename... Args>
struct ResultOf {
    using type = std::result_of_t<F(Args...)>;
};

template <typename F>
struct ResultOf<F, void> {
    using type = std::result_of_t<F()>;
};

}  // namespace detail

template <typename A, typename Closure>
class IO {
  public:
    template <
        typename std::enable_if_t<
            std::is_same<std::result_of_t<Closure()>, A>::value, int> = 0>
    IO(Closure&& closure) : closure_(std::move(closure)) {}
    A run() {
        return closure_();
    }

    template <
        typename Func,
        std::enable_if_t<
            detail::Helper<
                tour_of_fp::io::IO,
                typename detail::ResultOf<Func, A>::type>::value,
            int> = 0>
    auto bind(Func f);

    // template <
    // typename Func,
    // std::enable_if_t<
    // std::is_same<A, void>::value &&
    // detail::Helper<
    // tour_of_fp::io::IO, std::result_of_t<Func()>>::value,
    // int> = 0>
    // auto bind(Func f);

  private:
    Closure closure_;
};

template <typename Closure>
auto delay(Closure&& closure) {
    using Result = std::result_of_t<Closure()>;
    return IO<Result, Closure>(std::move(closure));
}

template <typename A>
auto unit(const A& a) {
    return delay([a]() { return a; });
}

template <typename A, typename Closure>
template <
    typename Func, std::enable_if_t<
                       detail::Helper<
                           tour_of_fp::io::IO,
                           typename detail::ResultOf<Func, A>::type>::value,
                       int>>
auto IO<A, Closure>::bind(Func f) {
    return delay([this, f]() { return f(run()).run(); });
}

// template <typename A, typename Closure>
// template <
// typename Func,
// std::enable_if_t<
// std::is_same<A, void>::value &&
// detail::Helper<tour_of_fp::io::IO, std::result_of_t<Func()>>::value,
// int>>
// auto IO<A, Closure>::bind(Func f) {
// return delay([this, f]() {
// run();
// return f().run();
// });
// }

template <typename Func, typename A, typename Closure>
auto fmap(const IO<A, Closure>& a, Func f) {
    return bind(a, [f = std::move(f)](const A& a) { return unit(f(a)); });
}

inline auto ReadLine() {
    return delay([]() {
        std::string s;
        std::getline(std::cin, s);
        return s;
    });
}

inline auto PrintLine(const std::string& s) {
    return delay([s]() { std::cout << s << '\n'; });
}

}  // namespace tour_of_fp::io

#endif
