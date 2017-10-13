#include "md5.h"
#include <iostream>

void PrintHash(const std::string& s) {
    std::cout << MD5::Pretty(MD5::Hash(s).get()) << '\n';
}

int main() {
    PrintHash("");
    PrintHash("The quick brown fox jumps over the lazy dog");
}