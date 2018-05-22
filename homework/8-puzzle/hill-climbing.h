#ifndef HILL_CLIMBING_
#define HILL_CLIMBING_

#include "./puzzle.h"
#include <algorithm>
#include <iostream>
#include <random>
#include <vector>

namespace climb {

template <size_t N>
std::vector<puzzle::Direction> ScrambledMovable(const puzzle::Puzzle<N> &p) {
    auto m = p.Movable();
    std::shuffle(begin(m), end(m), std::random_device());
    return m;
}

template <size_t N>
puzzle::SearchResult SearchFirstChoice(const puzzle::Puzzle<N> &p) {
    auto current = p;
    int depth = 0;
    int current_heuristic = puzzle::ManhattanDistance(p);
    int flat_move = 0;
    for (; not current.Finish(); ++depth) {
        bool found = false;
        for (auto d : ScrambledMovable(current)) {
            auto tmp = current.MoveCopy(d);
            auto h = puzzle::ManhattanDistance(tmp);
            if (h <= current_heuristic) {
                if (h == current_heuristic) {
                    if (flat_move < 50) {
                        ++flat_move;
                    } else {
                        continue;
                    }
                } else {
                    flat_move = 0;
                }
                current_heuristic = h;
                current.Move(d);
                found = true;
            }
        }
        if (not found) {
            return puzzle::SearchResult{false, depth};
        }
    }
    return puzzle::SearchResult{true, depth};
}

template <size_t N>
puzzle::SearchResult SearchSteepest(const puzzle::Puzzle<N> &p) {
    auto current = p;
    int depth = 0;
    int current_heuristic = puzzle::ManhattanDistance(p);
    int flat_move = 0;
    for (; not current.Finish(); ++depth) {
        puzzle::Direction next = puzzle::Direction(-1);
        int best = current_heuristic;
        for (auto d : ScrambledMovable(current)) {
            auto tmp = current.MoveCopy(d);
            auto h = puzzle::ManhattanDistance(tmp);
            if (h <= best) {
                best = h;
                next = d;
            }
        }
        if (next == puzzle::Direction(-1)) {
            return puzzle::SearchResult{false, depth};
        }
        if (current_heuristic == best) {
            if (flat_move < 50) {
                ++flat_move;
            } else {
                return puzzle::SearchResult{false, depth};
            }
        } else {
            flat_move = 0;
        }
        current.Move(next);
        current_heuristic = best;
    }
    return puzzle::SearchResult{true, depth};
}

} // namespace climb

#endif // HILL_CLIMBING_