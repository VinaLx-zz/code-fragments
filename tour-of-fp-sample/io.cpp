#include "io.h"
#include <string>
#include <type_traits>

using std::string;
using namespace tour_of_fp::io;

int aaa() {
    return 100;
}

int main(int argc, char** argv) {
    auto a = ReadLine().bind([](std::string size) { return PrintLine(size); });
    a.run();
}

