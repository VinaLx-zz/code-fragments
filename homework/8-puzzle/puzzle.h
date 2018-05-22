#ifndef PUZZLE_H_
#define PUZZLE_H_

#include <array>
#include <cmath>
#include <initializer_list>
#include <iostream>
#include <random>

namespace puzzle {

enum Direction { UP, DOWN, LEFT, RIGHT };

template <size_t N>
class Puzzle {
  public:
    using Board = std::array<std::array<int, N>, N>;
    static constexpr Board InitBoard() {
        std::array<std::array<int, N>, N> board;
        for (size_t i = 1; i < N * N; ++i) {
            board[(i - 1) / N][(i - 1) % N] = static_cast<int>(i);
        }
        board[N - 1][N - 1] = 0;
        return board;
    }

    Puzzle() : board_(InitBoard()) {}

    bool Movable(Direction d) const {
        switch (d) {
        case UP:
            return zx_ != 0;
        case DOWN:
            return zx_ != N - 1;
        case LEFT:
            return zy_ != 0;
        case RIGHT:
            return zy_ != N - 1;
        }
        return false;
    }
    bool Move(Direction d) {
        if (not Movable(d)) {
            return false;
        }
        switch (d) {
        case UP:
            MoveUp();
            break;
        case DOWN:
            MoveDown();
            break;
        case LEFT:
            MoveLeft();
            break;
        case RIGHT:
            MoveRight();
            break;
        }
        return true;
    }
    Puzzle<N> MoveCopy(Direction d) const {
        auto copy = *this;
        copy.Move(d);
        return copy;
    }
    std::vector<Direction> Movable() const {
        std::vector<Direction> v;
        for (auto d : {UP, LEFT, DOWN, RIGHT}) {
            if (Movable(d)) {
                v.push_back(d);
            }
        }
        return v;
    }

    Puzzle<N> &Scramble(int n = N * N * N) {
        std::mt19937 ms((std::random_device())());
        std::uniform_int_distribution gen(0, 3);
        for (int i = 0; i < n;) {
            if (Move(Direction(gen(ms))))
                ++i;
        }
        return *this;
    }

    const Board &Data() const {
        return board_;
    }

    bool Finish() const {
        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < N; ++j) {
                if (board_[i][j] == 0) {
                    continue;
                }
                if (board_[i][j] != i * N + j + 1) {
                    return false;
                }
            }
        }
        return true;
    }

    bool operator==(const Puzzle<N> &rhs) const {
        for (int i = 0; i < N; ++i) {
            for (int j = 0; j < N; ++j) {
                if (board_[i][j] != rhs.board_[i][j]) {
                    return false;
                }
            }
        }
        return true;
    }

    void Print() const {
        for (const auto &row : board_) {
            for (auto i : row) {
                std::cout << i << ' ';
            }
            std::cout << '\n';
        }
    }

  private:
    void MoveUp() {
        board_[zx_][zy_] = board_[zx_ - 1][zy_];
        --zx_;
        ResetZero();
    }
    void MoveLeft() {
        board_[zx_][zy_] = board_[zx_][zy_ - 1];
        --zy_;
        ResetZero();
    }
    void MoveDown() {
        board_[zx_][zy_] = board_[zx_ + 1][zy_];
        ++zx_;
        ResetZero();
    }
    void MoveRight() {
        board_[zx_][zy_] = board_[zx_][zy_ + 1];
        ++zy_;
        ResetZero();
    }
    void ResetZero() {
        board_[zx_][zy_] = 0;
    }
    Board board_;
    int zx_ = N - 1;
    int zy_ = N - 1;
};

template <typename I>
I Abs(I a) {
    return a < 0 ? -a : a;
}

template <size_t N>
int ManhattanDistance(const puzzle::Puzzle<N> p) {
    int d = 0;
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < N; ++j) {
            auto c = p.Data()[i][j];
            if (c == 0) {
                continue;
            }
            int oi = (c - 1) / N, oj = (c - 1) % N;
            d += Abs(i - oi) + Abs(j - oj);
        }
    }
    return d;
}

struct SearchResult {
    bool success;
    int depth;
    void Print() const {
        std::cout << std::boolalpha << "Success: " << success
                  << ", moves: " << depth << '\n';
    }
};

} // namespace puzzle

#endif // PUZZLE_H_