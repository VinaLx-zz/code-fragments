#ifndef SIMULATED_ANNEALING_
#define SIMULATED_ANNEALING_

#include "./n-queens.h"
#include <random>

namespace sa {

inline double Temperature(int n) {
    return 100.0 / n;
}

template <int N, typename Generator>
queens::Queens<N> PickNext(const queens::Queens<N> &q, Generator &&gen) {
    auto nexts = q.Neighbours();
    std::uniform_int_distribution<int> dist(0, nexts.size() - 1);
    return nexts[dist(gen)];
}

template <typename Generator>
bool Try(double p, Generator &&gen) {
    std::uniform_real_distribution<double> dist(0, 1);
    return dist(gen) < p;
}

inline double P(int de, double t) {
    return std::exp(de / t);
}

template <int N>
queens::Queens<N> Search(const queens::Queens<N> &q) {
    auto current = q;
    auto current_heuristic = q.Heuristic();
    std::mt19937 gen((std::random_device())());
    for (int i = 0; i < 3000; ++i) {
        if (current.Finish()) {
            break;
        }
        double t = Temperature(i);
        auto next = PickNext(current, gen);
        auto h = next.Heuristic();
        if (h >= current_heuristic &&
            not Try(P(current_heuristic - h, t), gen)) {
            continue;
        }
        current_heuristic = h;
        current = std::move(next);
    }
    return current;
}

} // namespace sa

#endif // SIMULATED_ANNEALING_