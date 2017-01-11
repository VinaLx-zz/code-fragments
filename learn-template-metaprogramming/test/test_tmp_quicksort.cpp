#include <iostream>
#include "../tmp-quicksort.h"

// template <int... ints>
// using Ints = std::integer_sequence<int, ints...>;

using namespace vinalx::meta;

template <int... ints>
void PrintSequence(Ints<ints...> i) {
    auto a = {(std::cout << ints << ' ', 0)..., 0};
    std::cout << '\n';
}

int main() {
    PrintSequence(Sort<0>::Result());
    PrintSequence(Sort<1, 3, 2, 4>::Result());
    PrintSequence(
        Sort<
        73, 82, 61, 91, 33, 69, 48, 72, 89, 34, 66, 52, 75, 23, 37,
        96, 63, 93, 45, 95, 6, 14, 85, 77, 88, 54, 55, 28, 29, 30, 78,
        71, 31, 20, 9, 3, 87, 68, 24, 25, 79, 80, 19, 26, 38, 40, 4,
        53, 67, 16, 44, 42, 56, 2, 41, 50, 86, 51, 70, 12, 36, 76, 74,
        98, 39, 46, 90, 84, 5, 92, 59, 10, 49, 58, 97, 43, 60, 11, 18,
        94, 81, 21, 22, 27, 1, 35, 0, 83, 13, 65, 15, 47, 17, 57, 62,
        99, 32, 8, 64, 7>::Result()
    ); // 0 to 99
}
