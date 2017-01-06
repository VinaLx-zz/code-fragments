#ifndef VINALX_STATIC_VALUE_H_
#define VINALX_STATIC_VALUE_H_

namespace vinalx {

namespace meta {

template <typename T, T kVal>
class static_value_helper {
  public:
    using type = T;
    static constexpr T value = kVal;
};

template <typename T, T kVal>
class static_value : public static_value_helper<T, kVal> {
  public:
    constexpr operator T() const {
        return kVal;
    }
};

template <bool kVal>
class static_bool : public static_value<bool, kVal> {};

using static_true = static_bool<true>;
using static_false = static_bool<false>;

}  // namespace meta

}  // namespace vinalx

#endif
