#ifndef N_QUEENS_
#define N_QUEENS_

#include <array>
#include <iostream>
#include <random>
#include <vector>

namespace queens {

constexpr int abs(int i) {
    return i >= 0 ? i : -i;
}

template <int N>
class Queens {
  public:
    Queens() {
        Scramble();
    }
    void Scramble() {
        std::mt19937 gen((std::random_device())());
        std::uniform_int_distribution dist(0, N - 1);
        for (int i = 0; i < N; ++i) {
            board_[i] = dist(gen);
        }
    }
    int Heuristic() const {
        int count = 0;
        for (int i = 0; i < N; ++i) {
            for (int j = i + 1; j < N; ++j) {
                if (Attacking({i, board_[i]}, {j, board_[j]})) {
                    ++count;
                }
            }
        }
        return count;
    }
    bool Finish() const {
        return Heuristic() == 0;
    }
    void Set(int x, int y) {
        board_[x] = y;
    }

    std::vector<Queens<N>> Neighbours() const {
        std::vector<Queens<N>> v;
        for (int i = 0; i < N; ++i) {
            for (int j = 0; j < N; ++j) {
                if (board_[i] == j) {
                    continue;
                }
                auto neighbour = *this;
                neighbour.Set(i, j);
                v.push_back(std::move(neighbour));
            }
        }
        return v;
    }

    void Print() const {
        for (int i = 0; i < N; ++i) {
            for (int j = 0; j < N; ++j) {
                std::cout << (j == board_[i] ? 'Q' : '.');
            }
            std::cout << '\n';
        }
    }

    static bool Attacking(std::pair<int, int> a, std::pair<int, int> b) {
        return a.first == b.first || a.second == b.second ||
               abs(a.first - b.first) == abs(a.second - b.second);
    }

  private:
    std::array<int, N> board_;
};

} // namespace queens

#endif // N_QUEENS_