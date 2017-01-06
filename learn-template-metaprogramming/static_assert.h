#ifndef VINALX_STATIC_ASSERT_H_
#define VINALX_STATIC_ASSERT_H_

#include <string>

namespace vinalx {

namespace meta {

template <bool kAssertion>
class static_assertion {
  public:
    static constexpr bool result = true;

    static_assertion() {}

    template <typename T>
    static_assertion(T) {}
};

// tried to use deleted default ctor
// while error messages were too long..
template <>
class static_assertion<false>;

#ifndef NDEBUG
#define STATIC_ASSERTION(expr) \
    static_assertion<(expr)>::result 
#else
#define STATIC_ASSERTION(expr);
#endif

}  // namespace meta

}  // namespace vinalx

#endif  // VINALX_STATIC_ASSERT_H_
