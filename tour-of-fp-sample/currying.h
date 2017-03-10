#include <utility>

namespace tour_of_fp::curry {

#define FORWARD(a) std::forward<decltype(a)>(a)

template <typename BinaryFunc>
auto currying(BinaryFunc&& f) {
    return [=](auto a) {
        return [ f = std::move(f), a ](auto&& b) {
            return f(a, FORWARD(b));
        };
    };
}

}  // namespace tour_of_fp::curry
