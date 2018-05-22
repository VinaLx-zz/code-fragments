#include <functional>
#include <vector>

using std::vector;
using std::function;

template <typename A, typename B, typename Alloc>
vector<B, Alloc> VectorMap(const vector<A>& va, function<B(A)> f);

template <template <typename> class F>
class Functor {
    template <typename A>
    static F<A> unit(const A& a);
    template <typename A, typename B>
    static F<B> fmap(const F<A>& fa, function<B(A)> f);
};
