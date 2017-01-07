#include "../static_assert.h"
#include "../type_judge.h"
#include "../type_list.h"

using namespace vinalx::meta;

template <typename T1, typename T2>
using L = type_link<T1, T2>;

using SomeList = L<int, L<double, L<float, list_tail>>>;

int main(int argc, char **argv) {
    static_assertion<
        is_same<
            L<char, L<int, L<double, L<float, list_tail>>>>,
            type_push<SomeList, char>::type
        >::value
    > assert1;

    static_assertion<
        is_same<
            L<double, L<float, list_tail>>, type_pop<SomeList>::type
        >::value
    > assert2;

    static_assertion<type_search<SomeList, int>::value == 0>();
    static_assertion<type_search<SomeList, char>::value == -1>();
    static_assertion<type_list_size<SomeList>::value == 3>();

    static_assertion<
        is_same<type_get<SomeList, 0>::type, int>::value
    >();

    static_assertion<
        is_same<type_get<SomeList, 2>::type, float>::value
    >();

    static_assertion<
        is_same<type_get<SomeList, 100>::type, list_tail>::value
    >();
}
