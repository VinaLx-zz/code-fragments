#include "./a-star.h"
#include <iostream>

int main() {
    puzzle::Puzzle<3> p;
    p.Scramble();
    std::cout << astar::Search(p) << '\n';
}