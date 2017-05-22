#ifndef VINALX_MIPS_UTIL_
#define VINALX_MIPS_UTIL_

#include <type_traits>

namespace vinalx {
namespace mips {
namespace util {

template <bool B>
class Require;

template <>
class Require<true> {};

template <typename T>
constexpr bool LessEq(T lhs, T rhs) {
    return lhs <= rhs;
}

template <typename T>
constexpr bool Less(T lhs, T rhs) {
    return lhs < rhs;
}

template <typename T, typename... Ts>
struct In;

template <typename T, typename... Ts>
struct In<T, T, Ts...> : std::true_type {};

template <typename T, typename T2, typename... Ts>
struct In<T, T2, Ts...> : In<T, Ts...> {};

template <typename T>
struct In<T> : std::false_type {};

template <typename T, template <typename...> class Temp>
struct is_template : std::false_type {};

template <template <typename...> class Temp, typename... Params>
struct is_template<Temp<Params...>, Temp> : std::true_type{};

}  // namespace util
}  // namespace mips
}  // namespace vinalx

#endif  // VINALX_MIPS_UTIL_
