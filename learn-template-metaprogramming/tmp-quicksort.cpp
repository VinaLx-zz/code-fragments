#include <algorithm>
#include <utility>

template <int... ints>
using Ints = std::integer_sequence<int, ints...>;

template <typename IntSeq>
struct Sort;

template <bool b, int num, typename AppendTrue, typename AppendFalse>
struct AppendIf;

template <int num, int... append_true, int... append_false>
struct AppendIf<true, num, Ints<append_true...>, Ints<append_false...>> {
    using FirstSeq = Ints<append_true..., num>;
    using SecondSeq = Ints<append_false...>;
};

template <int num, int... append_true, int... append_false>
struct AppendIf<false, num, Ints<append_true...>, Ints<append_false...>> {
    using FirstSeq = Ints<append_true...>;
    using SecondSeq = Ints<append_false..., num>;
};

template <typename IntSeq>
struct Partition;

template <>
struct Partition<Ints<>> {
    using Left = Ints<>;
    using Right = Ints<>;
};

template <int pivot, typename LeftResult, typename RightResult, typename IntSeq>
struct PartitionImpl;

template <int pivot, typename LeftResult, typename RightResult>
struct PartitionImpl<pivot, LeftResult, RightResult, Ints<>> {
    using Left = LeftResult;
    using Right = RightResult;
};

template <int Pivot, typename LeftResult, typename RightResult, int next_value,
          int... seq_ints>
struct PartitionImpl<Pivot, LeftResult, RightResult,
                     Ints<next_value, seq_ints...>> {
    using AppendResult =
        AppendIf<(next_value < Pivot), next_value, LeftResult, RightResult>;

    using PartitionResult =
        PartitionImpl<Pivot, typename AppendResult::FirstSeq,
                      typename AppendResult::SecondSeq, Ints<seq_ints...>>;

    using Left = typename PartitionResult::Left;
    using Right = typename PartitionResult::Right;
    static constexpr int pivot = Pivot;
};

template <int single_value>
struct Partition<Ints<single_value>> {
    using Left = Ints<>;
    using Right = Ints<>;
    static constexpr int pivot = single_value;
};

template <int Pivot, int... ints>
struct Partition<Ints<Pivot, ints...>> {
    using PartitionResult = PartitionImpl<Pivot, Ints<>, Ints<>, Ints<ints...>>;
    using Left = typename PartitionResult::Left;
    using Right = typename PartitionResult::Right;
    static constexpr int pivot = PartitionResult::pivot;
};

template <typename IntSeq1, int pivot, typename IntSeq2>
struct Merge;

template <int pivot, int... ints1, int... ints2>
struct Merge<Ints<ints1...>, pivot, Ints<ints2...>> {
    using Result = Ints<ints1..., pivot, ints2...>;
};

template <>
struct Sort<Ints<>> {
    using Result = Ints<>;
};

template <int... ints>
struct Sort<Ints<ints...>> {
    using PartionResult = Partition<Ints<ints...>>;
    using Result = typename Merge<
        typename Sort<typename PartionResult::Left>::Result,
        PartionResult::pivot,
        typename Sort<typename PartionResult::Right>::Result>::Result;
};

#include <iostream>

// template <int... ints>
// using Ints = std::integer_sequence<int, ints...>;

template <int... ints>
void PrintSequence(Ints<ints...> i) {
    auto a = {(std::cout << ints << ' ', 0)..., 0};
    std::cout << '\n';
}

int main() {
    PrintSequence(Sort<Ints<0>>::Result());
    PrintSequence(Sort<Ints<1, 3, 2, 4>>::Result());
    PrintSequence(
        Sort<Ints<
        73, 82, 61, 91, 33, 69, 48, 72, 89, 34, 66, 52, 75, 23, 37,
        96, 63, 93, 45, 95, 6, 14, 85, 77, 88, 54, 55, 28, 29, 30, 78,
        71, 31, 20, 9, 3, 87, 68, 24, 25, 79, 80, 19, 26, 38, 40, 4,
        53, 67, 16, 44, 42, 56, 2, 41, 50, 86, 51, 70, 12, 36, 76, 74,
        98, 39, 46, 90, 84, 5, 92, 59, 10, 49, 58, 97, 43, 60, 11, 18,
        94, 81, 21, 22, 27, 1, 35, 0, 83, 13, 65, 15, 47, 17, 57, 62,
        99, 32, 8, 64, 7>>::Result()
    ); // 0 to 99
}
