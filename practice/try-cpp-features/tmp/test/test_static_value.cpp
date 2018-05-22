#include <iostream>
#include "../static_value.h"

std::string s = "hello template";

void print(const std::string& str) {
    std::cout << str << '\n';
}

int main() {
    std::cout << static_value<int, 10>::value << '\n';
    std::cout << static_value<std::string&, s>::value << '\n';
    std::cout << *static_value<std::string*, &s>::value << '\n';
    print(static_value<std::string&, s>());
    print(*static_value<std::string*, &s>());
}

