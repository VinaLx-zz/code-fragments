#include <functional>

using std::function;

template <template <typename> class F>
class Monad {
    template <typename A>
    static F<A> unit(const A& a);
    // template <typename A, typename B>
    // static F<B> fmap(const F<A>& fa, function<B(A)> f);
    // template <typename A>
    // static F<A> join(const F<F<A>>& ffa);
    template <typename A, typename B>
    static F<B> bind(const F<A>& fa, function<F<B>(A)> f);
};

