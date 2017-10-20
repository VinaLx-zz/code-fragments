#ifndef ASTAR_H_
#define ASTAR_H_

#include "./puzzle.h"
#include <iostream>
#include <queue>
#include <unordered_set>

namespace astar {

template <size_t N>
struct State {
    puzzle::Puzzle<N> puzzle;
    int heuristic;
    size_t depth;
    void Print() const {
        puzzle.Print();
        std::cout << "heuristic: " << heuristic << ", depth: " << depth << '\n';
    }
};

template <size_t N>
struct CompByHeuristic {
    bool operator()(const State<N> &lhs, const State<N> &rhs) {
        return lhs.heuristic > rhs.heuristic;
    }
};

template <size_t N>
struct PuzzleEqual {
    bool
    operator()(const puzzle::Puzzle<N> &l, const puzzle::Puzzle<N> &r) const {
        return l == r;
    }
};

template <size_t N>
struct PuzzleHash {
    size_t operator()(const puzzle::Puzzle<N> &p) const {
        size_t n = 0;
        for (const auto &row : p.Data()) {
            for (auto v : row) {
                n = n * 10 + v;
            }
        }
        std::hash<size_t> h;
        return h(n);
    }
};

template <size_t N>
size_t Search(const puzzle::Puzzle<N> &p) {
    std::priority_queue<State<N>, std::vector<State<N>>, CompByHeuristic<N>> pq;
    std::unordered_set<puzzle::Puzzle<N>, PuzzleHash<N>, PuzzleEqual<N>>
        visited;
    pq.push(State<N>{p, puzzle::ManhattanDistance(p), 0});
    visited.insert(p);
    for (; not pq.empty();) {
        auto state = pq.top();
        pq.pop();
        if (state.puzzle.Finish()) {
            return state.depth;
        }
        for (auto d : state.puzzle.Movable()) {
            auto new_puzzle = state.puzzle.MoveCopy(d);
            if (visited.count(new_puzzle)) {
                continue;
            }
            visited.insert(new_puzzle);
            pq.push(State<N>{new_puzzle, puzzle::ManhattanDistance(new_puzzle),
                             state.depth + 1});
        }
    }
    return -1;
}

} // namespace astar

#endif