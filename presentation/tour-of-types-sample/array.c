#include <stdio.h>

void Array() {
    // fixed length array
    // with first 3 elements initialized
    int fix_length_array[5] = {1, 2, 3};
    // 1 2 3 0 0
    for (int i = 0; i < 5; ++i) {
        printf("%d ", fix_length_array[i]);
    }
}
