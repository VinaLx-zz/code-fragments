#include "io.h"
#include <string>
#include <type_traits>

using std::string;
using namespace tour_of_fp::io;

int main(int argc, char** argv) {
    auto a =
        PrintLine("type a line I'll count how many characters")
            .Bind([]() { return ReadLine(); })
            .Fmap([](const std::string& s) {
                return "there are " + std::to_string(s.size()) + " characters";
            })
            .Bind([](const std::string& s) { return PrintLine(s); })
            .Infinite();
    a.Run();
}
