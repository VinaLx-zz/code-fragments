#ifndef VINALX_CALC_USER_INTERFACE_H_
#define VINALX_CALC_USER_INTERFACE_H_

#include <cctype>
#include <iostream>
#include <string>
#include "Parser.h"
#include "Scanner.h"

namespace vinalx {
namespace calc {

class UserInterface {
  public:
    static void Go() {
        for (;;) {
            Prompt();
            std::string command = GetTrimmedLine();
            if (command.empty()) {
                std::cout << '\n';
                return;
            }
            if (command == "quit" or command == "q") {
                return;
            }
            try {
                Scanner scanner(command);
                Parser parser(scanner.Scan());
                std::unique_ptr<Expression> expr = parser.Parse();
                std::cout << expr->Value() << '\n';
            } catch (const std::logic_error& err) {
                std::cerr << err.what() << '\n';
            }
        }
    }

  private:
    static void Prompt() {
        std::cout << "> ";
    }
    static std::string GetTrimmedLine() {
        std::string line;
        for (; std::getline(std::cin, line);) {
            line = Trim(line);
            if (not line.empty()) {
                break;
            }
        }
        return line;
    }

    static std::string Trim(const std::string& s) {
        int i = 0, j = s.size();
        for (; i < j and isspace(s[i]); ++i)
            continue;
        for (; j > i and isspace(s[j - 1]); --j)
            continue;
        return s.substr(i, j - i);
    }
};

}  // namespace calc
}  // namespace vinalx

#endif  // VINALX_CALC_USER_INTERFACE_H_;
