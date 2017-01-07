#ifndef VINALX_TYPE_LIST_H_
#define VINALX_TYPE_LIST_H_

#include <cstdint>
#include "static_value.h"

namespace vinalx {

namespace meta {

struct list_tail {};  // tail of the list

template <typename T1, typename T2>
struct type_link;  // main body of the list

template <typename T1, typename T2, typename T3>
struct type_link<T1, type_link<T2, T3>> {
    using type = T1;
    using next = type_link<T2, T3>;
};

template <typename T>
struct type_link<T, list_tail> {
    using type = T;
    using next = list_tail;
};

/**
 * add a type to the head of the type list
 */
template <typename TypeList, typename T>
struct type_push;

template <typename T, typename T1, typename T2>
struct type_push<type_link<T1, T2>, T> {
    using type = type_link<T, type_link<T1, T2>>;
};

template <typename T>
struct type_push<list_tail, T> {
    using type = type_link<T, list_tail>;
};

/**
 * remove the first type from the type list
 * didn't specialize list_tail for type_pop, leave it a compiler(linkage) error
 */
template <typename TypeList>
struct type_pop;

template <typename T1, typename T2>
struct type_pop<type_link<T1, T2>> {
    using type = T2;
};

/**
 * search the index of the first occurence of the type in the type list
 * -1 if not found
 */
template <typename TypeList, typename T>
struct type_search;

template <typename Found, typename Next>
struct type_search<type_link<Found, Next>, Found> : static_value<int64_t, 0> {};

template <typename T>
struct type_search<list_tail, T> : static_value<int64_t, -1> {};

template <>
struct type_search<list_tail, list_tail> : static_value<int64_t, 0> {};

template <typename NotFound, typename Next, typename T>
struct type_search<type_link<NotFound, Next>, T> {
  private:
    static constexpr int64_t search_result = type_search<Next, T>::value;

  public:
    static constexpr int64_t value =
        search_result == -1 ? -1 : search_result + 1;
};

/**
 * size of the list, simple wrapper of type_search
 */
template <typename TypeList>
struct type_list_size: type_search<TypeList, list_tail> {};

/**
 * get the type at the specifed index in the type list
 * list_tail if out of range
 */
template <typename TypeList, uint64_t index>
struct type_get;

template <typename This, typename Next>
struct type_get<type_link<This, Next>, 0> {
    using type = This;
};

template <typename This, typename Next, uint64_t index>
struct type_get<type_link<This, Next>, index> {
    using type = typename type_get<Next, index - 1>::type;
};

template <uint64_t index>
struct type_get<list_tail, index> {
    using type = list_tail;
};

}  // namespace meta

}  // namespace vinalx

#endif
