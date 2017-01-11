#include <algorithm>
#include <utility>

namespace vinalx {

namespace meta {

template <int... ints>
using Ints = std::integer_sequence<int, ints...>;

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

template <int pivot, typename LeftResult, typename RightResult, int next_value,
          int... seq_ints>
struct PartitionImpl<pivot, LeftResult, RightResult,
                     Ints<next_value, seq_ints...>> {
    using AppendResult =
        AppendIf<(next_value < pivot), next_value, LeftResult, RightResult>;

    using PartitionResult =
        PartitionImpl<pivot, typename AppendResult::FirstSeq,
                      typename AppendResult::SecondSeq, Ints<seq_ints...>>;

    using Left = typename PartitionResult::Left;
    using Right = typename PartitionResult::Right;
    static constexpr int Pivot = pivot;
};

template <int single_value>
struct Partition<Ints<single_value>> {
    using Left = Ints<>;
    using Right = Ints<>;
    static constexpr int Pivot = single_value;
};

template <int pivot, int... ints>
struct Partition<Ints<pivot, ints...>> {
    using PartitionResult = PartitionImpl<pivot, Ints<>, Ints<>, Ints<ints...>>;
    using Left = typename PartitionResult::Left;
    using Right = typename PartitionResult::Right;
    static constexpr int Pivot = PartitionResult::Pivot;
};

template <typename IntSeq1, int pivot, typename IntSeq2>
struct Merge;

template <int pivot, int... ints1, int... ints2>
struct Merge<Ints<ints1...>, pivot, Ints<ints2...>> {
    using Result = Ints<ints1..., pivot, ints2...>;
};

template <typename IntSeq>
struct SortImpl;

template <>
struct SortImpl<Ints<>> {
    using Result = Ints<>;
};

template <int... ints>
struct SortImpl<Ints<ints...>> {
    using PartionResult = Partition<Ints<ints...>>;
    using Result = typename Merge<
        typename SortImpl<typename PartionResult::Left>::Result,
        PartionResult::Pivot,
        typename SortImpl<typename PartionResult::Right>::Result>::Result;
};

template <int... ints>
struct Sort {
    using Result = typename SortImpl<Ints<ints...>>::Result;
};

} // namespace meta

} // namespace vinalx
