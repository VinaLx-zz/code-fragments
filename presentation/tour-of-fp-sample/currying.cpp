#include "currying.h"
#include <iostream>

using std::cout;
using tour_of_fp::curry::currying;

auto f() {
    std::string a("adfkjadf");
    return currying([](std::string a, std::string b) { return a + b; })(std::move(a));
}

int main(int argc, char **argv) {
    cout << f()("another") << '\n';
}
