#ifndef VINALX_TYPE_JUDGE_H_
#define VINALX_TYPE_JUDGE_H_

#include "static_value.h"

namespace vinalx {
namespace meta {

template <typename T1, typename T2>
struct is_same : static_false {};

template <typename T>
struct is_same<T, T> : static_true {};

} // namespace meta

} // namespace vinalx

#endif  // VINALX_TYPE_JUDGE_H_
