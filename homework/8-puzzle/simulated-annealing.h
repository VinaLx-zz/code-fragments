#ifndef SIMULATED_ANNEALING_
#define SIMULATED_ANNEALING_

#include "./puzzle.h"
#include <algorithm>
#include <random>

namespace sa {

template <size_t N>
puzzle::Puzzle<N> RandomPickNext(const puzzle::Puzzle<N> &p) {
    auto dirs = p.Movable();
    std::shuffle(begin(dirs), end(dirs), std::random_device());
    return p.MoveCopy(dirs.front());
}

template <size_t N>
auto NewSchedule() {
    return [n = 1000.0]() mutable {
        auto ret = n;
        if (n > 0) {
            n -= 1;
        }
        return ret;
    };
}

inline double P(double t, double de) {
    return std::exp(t / de);
}

inline auto RandomGen() {
    return [
        gen = std::mt19937((std::random_device())()),
        dist = std::uniform_real_distribution<double>(0, 1)
    ]() mutable {
        return dist(gen);
    };
}

template <size_t N>
puzzle::SearchResult Search(const puzzle::Puzzle<N> &p) {
    auto current = p;
    auto cur_energy = puzzle::ManhattanDistance(p);
    int depth = 0;

    auto schedule = NewSchedule<N>();
    auto randomGen = RandomGen();

    for (;;) {
        double t = schedule();
        if (t <= 0) {
            break;
        }
        puzzle::Puzzle<N> next = RandomPickNext(p);
        auto next_energy = puzzle::ManhattanDistance(next);
        if (next_energy < cur_energy) {
            double prob = P(t, next_energy - cur_energy);
            if (randomGen() > prob) {
                continue;
            }
        }
        current = std::move(next);
        cur_energy = next_energy;
        ++depth;
    }

    return puzzle::SearchResult{current.Finish(), depth};
}

} // namespace sa

#endif // SIMULATED_ANNEALING_