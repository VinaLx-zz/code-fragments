#ifndef TOUR_OP_FP_OPTIONAL_MONAD_H_
#define TOUR_OP_FP_OPTIONAL_MONAD_H_

#include <experimental/optional>
#include <type_traits>
#include "monad.h"

namespace tour_of_fp::optional_monad {
using std::experimental::optional;

namespace detail {
template <typename T>
struct helper;

template <typename T>
struct helper<optional<T>> {
    using type = T;
};

} // namespace detail

template <typename A>
optional<A> unit(const A& a) {
    return optional<A>(a);
}
template <typename A, typename Func>
auto bind(const optional<A>& fa, Func f) {
    using Result = typename detail::helper<std::result_of_t<Func(A)>>::type;
    if (fa) {
        return f(*fa);
    }
    return optional<Result>{};
}

}  // namespace tour_of_fp::optional_monad

#endif  // TOUR_OF_FP_OPTIONAL_MONAD_H_
