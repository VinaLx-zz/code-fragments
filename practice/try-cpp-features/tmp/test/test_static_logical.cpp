#include "../static_logical.h"
#include "../static_assert.h"

using namespace vinalx::meta;

int main(int argc, char **argv) {
    static_assertion<static_nor<false, false, true, false>::value>(); // fire
    static_assertion<static_nor<false>::value>(); 
    static_assertion<static_nor<true>::value>(); // fire
    static_assertion<static_nor<false, false>::value>();
    static_assertion<static_nor<>::value>();

    static_assertion<static_nand<>::value>(); // fire
    static_assertion<static_nand<true>::value>(); // fire
    static_assertion<static_nand<true, false>::value>();
    static_assertion<static_nand<false>::value>();

}

