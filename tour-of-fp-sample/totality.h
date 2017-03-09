#include <experimental/optional>
#include <vector>

using std::experimental::optional;

template <typename T, typename Alloc>
T get(const std::vector<T, Alloc>& v, size_t index);

template <typename T, typename Alloc>
optional<T> get(const std::vector<T, Alloc>& v, size_t index);
