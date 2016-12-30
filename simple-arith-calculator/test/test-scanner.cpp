#include "Scanner.h"
#include "Token.h"
#include <iostream>
#include <string>
#include <vector>

using std::string;

using vinalx::calc::Scanner;
using vinalx::calc::Token;

int main(int argc, char **argv) {
    for (string input; getline(std::cin, input); ) {
        Scanner scanner(input);
        try {
            std::vector<Token> tokens = scanner.Scan();
            for (const auto& token : tokens) {
                std::cout << "[ " << token.content << " ] ";
            }
            std::cout << '\n';
        } catch (std::logic_error& e) {
            std::cout << e.what() << '\n';
            continue;
        }
    }
}

