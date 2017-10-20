#include "./hill-climbing.h"
#include "./simulated-annealing.h"
#include <cstdio>

int main() {
    puzzle::Puzzle<3> p;
    std::printf("test steepest climbing\n");
    for (int i = 0; i < 50; ++i) {
        p.Scramble();
        std::printf("case %d: ", i);
        climb::SearchSteepest(p).Print();
    }
    std::printf("test first choice climbing\n");
    for (int i = 0; i < 50; ++i) {
        p.Scramble();
        std::printf("case %d: ", i);
        climb::SearchFirstChoice(p).Print();
    }
    std::printf("test simulated annealing\n");
    for (int i = 0; i < 50; ++i) {
        p.Scramble();
        std::printf("case %d: ", i);
        sa::Search(p).Print();
    }
}