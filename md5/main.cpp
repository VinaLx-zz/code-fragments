#include "md5.h"
#include <cassert>
#include <iostream>

std::string PrettyHash(const std::string &s) {
    return MD5::Pretty(MD5::Hash(s).get());
}

void SimpleTest() {
    auto test1 = PrettyHash("");
    std::cout << test1 << '\n';
    assert(test1 == "d41d8cd98f00b204e9800998ecf8427e");

    auto test2 = PrettyHash("The quick brown fox jumps over the lazy dog");
    std::cout << test2 << '\n';
    assert(test2 == "9e107d9d372bb6826bd81d3542a419d6");

    auto test3 = PrettyHash("The quick brown fox jumps over the lazy dog.");
    std::cout << test3 << '\n';
    assert(test3 == "e4d909c290d0fb1ca068ffaddf22cbd0");

    std::cout << "pass\n";
}

int main() {
    SimpleTest();
}