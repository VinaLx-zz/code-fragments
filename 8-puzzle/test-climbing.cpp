#include "./hill-climbing.h"
#include "./simulated-annealing.h"
#include <cstdio>
#include <string>

template <typename Searcher, int Total = 1000>
void Test(Searcher searcher, std::string method) {
    int success = 0;
    puzzle::Puzzle<3> p;
    for (int i = 0; i < Total; ++i) {
        p.Scramble();
        auto result = searcher(p);
        if (result.success) {
            ++success;
        }
    }
    std::printf(
        "method %s:\ntotal cases: %d, success: %d\nsuccess rate: %.3f\n",
        method.data(), Total, success, static_cast<double>(success) / Total);
}

int main() {
    puzzle::Puzzle<3> p;
    Test(climb::SearchSteepest<3>, "Steepest Hill Climbing");
    Test(climb::SearchFirstChoice<3>, "First Choice Hill Climbing");
    Test(sa::Search<3>, "Simulated Annealing");
    // std::printf("test steepest climbing\n");
    // for (int i = 0; i < 50; ++i) {
    //     p.Scramble();
    //     std::printf("case %d: ", i);
    //     climb::SearchSteepest(p).Print();
    // }
    // std::printf("test first choice climbing\n");
    // for (int i = 0; i < 50; ++i) {
    //     p.Scramble();
    //     std::printf("case %d: ", i);
    //     climb::SearchFirstChoice(p).Print();
    // }
    // std::printf("test simulated annealing\n");
    // for (int i = 0; i < 50; ++i) {
    //     p.Scramble();
    //     std::printf("case %d: ", i);
    //     sa::Search(p).Print();
    // }
}