#include <stdio.h>
#include <stdint.h>

int main() {
    char a = 'a';
    // ?
    printf("%d\n", a);

    int64_t i = 0x000A214F4C4C4548;
    // ???
    printf("%s", (char*)&i);
}
