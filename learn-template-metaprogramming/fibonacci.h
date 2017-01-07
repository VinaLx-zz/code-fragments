#ifndef VINALX_FIBONACCI_H_
#define VINALX_FIBONACCI_H_

#include <cstdint>

namespace vinalx {
namespace meta {

template <uint64_t kN, uint64_t kModulo = 1>
struct Fibonacci {
    static constexpr uint64_t value =
        (Fibonacci<kN - 1, kModulo>::value + 
         Fibonacci<kN - 2, kModulo>::value) % kModulo;
};

template <uint64_t kN>
struct Fibonacci<kN, 0> {};

template <uint64_t kModulo>
struct Fibonacci<0, kModulo> {
    static constexpr uint64_t value = 0;
};

template <uint64_t kModulo>
struct Fibonacci<1, kModulo> {
    static constexpr uint64_t value = 1;
};

} // namespace meta

} // namespace vinalx

#endif
