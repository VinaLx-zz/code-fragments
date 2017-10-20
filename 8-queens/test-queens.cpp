#include "./hill-climb.h"
#include "./n-queens.h"
#include "./simulated-annealing.h"
#include <string>

template <int N = 8, typename Searcher>
void Test(Searcher searcher, std::string method, int total = 1000) {
    queens::Queens<N> base;
    int success = 0;
    std::cout << "Test " << method << ":\n";
    for (int i = 0; i < total; ++i) {
        base.Scramble();
        auto result = searcher(base);
        auto finish = result.Finish();
        success += finish ? 1 : 0; // std::printf("case %d: %s\n", i, finish ? "success" : "fail");
        // result.Print();
    }
    auto succ_rate = static_cast<double>(success) / total;
    std::printf(
        "total cases: %d, success cases: %d\nsuccess rate: %.3f\n", total,
        success, succ_rate);
}

int main() {
    Test(climb::SearchSteepest<8>, "Steepest Hill Climbing");
    Test(climb::SearchFirstChoice<8>, "First Choice Hill Climbing");
    Test(sa::Search<8>, "Simulated Annealing");
}