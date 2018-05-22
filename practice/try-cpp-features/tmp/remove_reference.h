#ifndef VINALX_TYPE_TRAITS_H_
#define VINALX_TYPE_TRAITS_H_

namespace vinalx {

namespace meta {

template <typename T>
struct remove_lvalue_reference {
    using type = T;
};

template <typename T>
struct remove_lvalue_reference<T&> {
    using type = T;
};

template <typename T>
using remove_lvalue_reference_t = typename remove_lvalue_reference<T>::type;

template <typename T>
struct remove_rvalue_reference {
    using type = T;
};

template <typename T>
struct remove_rvalue_reference<T&&> {
    using type = T;
};

template <typename T>
using remove_rvalue_reference_t = typename remove_rvalue_reference<T>::type;

template <typename T>
struct remove_reference {
    using type = T;
};

template <typename T>
struct remove_reference<T&> {
    using type = T;
};

template <typename T>
struct remove_reference<T&&> {
    using type = T;
};

template <typename T>
using remove_reference_t = typename remove_reference<T>::type;

}  // namespace meta

}  // namespace vinalx

#endif
