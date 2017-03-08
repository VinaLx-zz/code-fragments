#include "lazy-impl.h"

#include <iostream>

using std::cout;

int main() {
    auto l = thunk([]() {
        cout << "I'm delayed executed!\n";
        return 42;
    });
    cout << "I'm executed first!\n";
    cout << l.result() << '\n';
}
