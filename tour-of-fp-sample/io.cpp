#include "io.h"
#include <string>
#include <type_traits>

using std::string;
using namespace tour_of_fp::io;


int main(int argc, char** argv) {
    auto a = ReadLine()
                 .Bind([](std::string line) { return PrintLine(line); })
                 .Bind([]() { return PrintLine("Wow!"); });
    a.Run();
}
