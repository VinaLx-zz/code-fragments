#ifndef HILL_CLIMB_
#define HILL_CLIMB_

#include <random>
#include <algorithm>
#include "./n-queens.h"

namespace climb {

template <typename Iter>
void Shuffle(Iter b, Iter e) {
    return std::shuffle(b, e, std::random_device());
}

template <int N>
queens::Queens<N> SearchFirstChoice(const queens::Queens<N>& q) {
    auto current = q;
    auto current_heurisic = q.Heuristic();
    for (;;) {
        if (current.Finish()) {
            return current;
        }
        std::vector<queens::Queens<N>> nexts = current.Neighbours();
        Shuffle(begin(nexts), end(nexts));
        for (auto& now : nexts) {
            auto h = now.Heuristic();
            if (h <= current_heurisic) {
                current = std::move(now);
                current_heurisic = h;
                continue;
            }
        }
        return current;
    }
}

template <int N>
queens::Queens<N> SearchSteepest(const queens::Queens<N>& q) {
    auto current = q;
    auto current_heurisic = q.Heuristic();
    int flat_move = 0;
    int moves = 0;
    for (;;++moves) {
        if (current.Finish()) {
            break;
        }
        std::vector<queens::Queens<N>> nexts = current.Neighbours();
        Shuffle(begin(nexts), end(nexts));
        int best = -1;
        int best_heuristic = current_heurisic;
        for (int i = 0; i < nexts.size(); ++i) {
            auto now = nexts[i].Heuristic();
            if (now <= best_heuristic) {
                best = i;
                best_heuristic = now;
            }
        }
        if (best == -1) {
            break;
        }
        if (current_heurisic == best_heuristic) {
            if (flat_move >= 100) {
                break;
            }
            ++flat_move;
        } else {
            flat_move = 0;
        }
        current = std::move(nexts[best]);
        current_heurisic = best_heuristic;
    }
    return current;
}

} // namespace climb

#endif // HILL_CLIMB_