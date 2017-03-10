#include "optional-monad.h"
#include <iostream>
#include <vector>
#include "experimental/optional"

using std::ostream;
using std::vector;
using std::experimental::optional;

template <typename T, typename Alloc>
optional<T> Get(const std::vector<T, Alloc>& v, size_t index) {
    if (index < v.size())
        return v[index];
    return {};
}

optional<double> Divide(int a, int b) {
    if (b == 0)
        return {};
    return static_cast<double>(a) / b;
}

optional<double> GetAndDivide(const std::vector<int>& v, size_t ia, size_t ib) {
    using tour_of_fp::optional_monad::bind;
    return bind(Get(v, ia), [ib, &v](int a) {
        return bind(
            Get(v, ib), [a](int b) { return Divide(a, b); });
    });
}

template <typename T>
ostream& operator<<(ostream& os, const optional<T>& o) {
    if (o) {
        os << *o;
    } else {
        os << "NONE";
    }
    return os;
}

int main() {
    std::vector<int> v{5, 4, 3, 2, 1, 0};
    std::cout << GetAndDivide(v, 0, 1) << '\n'
              << GetAndDivide(v, 0, 5) << '\n'
              << GetAndDivide(v, 0, 100) << '\n';
}
