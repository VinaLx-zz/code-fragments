#ifndef VINALX_GCD_H_
#define VINALX_GCD_H_

#include <cstdint>

namespace vinalx {

namespace meta {

template <uint64_t kA, uint64_t kB>
struct Gcd {
    static constexpr uint64_t value = Gcd<kB, kA % kB>::value;
};

template <uint64_t kA>
struct Gcd<kA, 0> {
    static constexpr uint64_t value = kA;
};

template <uint64_t kB>
struct Gcd<0, kB>;

template <uint64_t kA, uint64_t kB>
struct Lcm {
    static constexpr uint64_t value = kA * kB / Gcd<kA, kB>::value;
};

}  // namespace meta

}  // namespace vinalx

#endif  // VINALX_GCD_H_
