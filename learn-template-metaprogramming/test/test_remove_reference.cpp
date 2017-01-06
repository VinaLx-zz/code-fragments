#include "remove_reference.h"
#include <type_traits>

template <typename T> class TD;

using namespace vinalx::meta;

template <typename T>
void f(T&&) {
    TD<T&&> _1;
    TD<T&> _2;
    TD<T> _3;
    TD<remove_rvalue_reference_t<T&&>> a;
    TD<remove_rvalue_reference_t<T&>> b;
    TD<remove_lvalue_reference_t<T&&>> c;
    TD<remove_lvalue_reference_t<T&>> d;
    TD<remove_reference_t<T&&>> e;
    TD<remove_reference_t<T&>> f;
    TD<remove_reference_t<T>> g;
}

int main() {
    int a;
    f(a);
    f(1);
}
