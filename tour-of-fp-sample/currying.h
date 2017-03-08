#include <functional>
#include <utility>

using std::function;

template <class A, class B, class C>
function<function<C(B)>(A)> currying(function<C(A, B)>);

template <class BinaryFunc>
auto currying(BinaryFunc f) {
    return [=](auto a) { return [=](auto b) { return f(a, b); }; };
}
