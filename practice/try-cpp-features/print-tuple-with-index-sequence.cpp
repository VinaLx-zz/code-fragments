#include <iostream>
#include <tuple>
#include <type_traits>
#include <utility>

using namespace std;

template <typename T>
void Print(const T& value) {
    cout << value << '\n';
}

template <typename... Args, typename Index, Index... index,
          typename enable_if<is_integral<Index>::value, int>::type = 0>
void PrintTuple(
    const tuple<Args...> tup, integer_sequence<Index, index...> indices) {
    auto x __attribute__((unused)) = {(Print(get<index>(tup)), 0)...};
}

template <typename... Args>
auto MakeTupleAndPrint(Args&&... args) {
    auto ret = make_tuple(std::forward<Args>(args)...);
    PrintTuple(ret, index_sequence_for<Args...>());
    return ret;
}

int main(int argc, char** argv) {
    MakeTupleAndPrint("abc", 1, 2.5);
}
