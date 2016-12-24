#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <limits.h>

int main() {

    /*
    */
    unsigned int x = 1, y = 2, z = 0;
    z = x - y;
    printf("%u\n", z);
    z = 0;
    z--;
    printf("%s\n", (z == UINT_MAX ? "equal" : "different"));

    /*
    uint16_t i = 0;
    while (true) {
        i++;
        if (i < 10000) {
            printf("%u\n", i);
        } else {
            if (i % 10000 == 0) {
                printf("%u\n", i);
            }
        }
    }
    */
}
