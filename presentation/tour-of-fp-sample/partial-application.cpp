#include <algorithm>
#include <functional>
#include <vector>
#include "currying.h"

using std::vector;

vector<int>::const_iterator FindEqualTo1(const vector<int>& v, int num) {
    auto i(v.begin());
    for (; i != end(v); ++i) {
        if (*i == num)
            return i;
    }
    return i;
}

vector<int>::const_iterator FindEqualTo2(const vector<int>& v, int num) {
    return std::find_if(begin(v), end(v), [num](auto e) { return e == num; });
}

vector<int>::const_iterator FindEqualTo3(const vector<int>& v, int num) {
    return std::find_if(begin(v), end(v), currying(std::equal_to<int>())(num));
}
