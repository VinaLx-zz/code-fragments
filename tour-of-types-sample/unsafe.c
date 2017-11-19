#include <stdio.h>

void Unsafe() {
    double d = 2.5;
    // ???
    printf("%d\n", d);

    int array[5] = {1, 0, 0, 8, 6};
    int n = 10086;
    // out-of-range indexing is not type error
    printf("%d\n", array[n]);
}
