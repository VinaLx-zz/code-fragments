#include <vector>
#include <iostream>

using std::vector;

template <typename T>
using Vector = vector<T>;

template <typename T>
void printVector(const Vector<T>& v) {
    for (auto a : v) {
        std::cout << a << '\n';
    }
}

template <template <typename> class V, typename T>
void printVector2(const V<T>& v) {
    for (auto a : v) {
        std::cout << a << '\n';
    }
}

int main() {
   Vector<int> v{1,2,3,4,5,6}; 
   printVector(v);
   // printVector2(v); // compiler error;
}
