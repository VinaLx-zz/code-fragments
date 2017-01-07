#ifndef VINALX_STATIC_LOGICAL_H_
#define VINALX_STATIC_LOGICAL_H_

#include "static_value.h"

namespace vinalx {

namespace meta {

template <bool... kBoolPack>
struct static_or;

template <>
struct static_or<> : static_false {};

template <bool... kBoolPack>
struct static_or<true, kBoolPack...> : static_true {};

template <bool... kBoolPack>
struct static_or<false, kBoolPack...> : static_or<kBoolPack...> {};

template <bool... kBoolPack>
struct static_and;

template <>
struct static_and<> : static_true {};

template <bool... kBoolPack>
struct static_and<true, kBoolPack...> : static_and<kBoolPack...> {};

template <bool... kBoolPack>
struct static_and<false, kBoolPack...> : static_false {};

template <bool... kBoolPack>
struct static_nand : static_bool<!static_and<kBoolPack...>::value> {};

template <bool... kBoolPack>
struct static_nor : static_bool<!static_or<kBoolPack...>::value> {};

}  // namespace meta

}  // namespace vinalx

#endif  // VINALX_STATIC_LOGICAL_H_
