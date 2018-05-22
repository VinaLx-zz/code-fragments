#include "../static_assert.h"

using namespace vinalx::meta;

int main() {
    static_assertion<false> this_asssert_fail;
    static_assertion<false>("this assert fail");
    static_assertion<true> this_asssert_success;
    static_assertion<true>("this assert success");
    STATIC_ASSERTION(1 == 2);
    STATIC_ASSERTION(1 == 1);
}
