#include "currying.h"
#include <iostream>

using std::cout;
using tour_of_fp::curry::currying;

int main(int argc, char **argv) {
    auto a = currying([](int a, int b) { return a - b; });
    cout << a(1)(2) << '\n';
}
