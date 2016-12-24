#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct data_s {
  uint32_t ui32;
} data_t;

int main() {
    const size_t size = 1048576;
    uint32_t a[size];
    printf("sizeof(a) = %lu\n", sizeof(a));

    data_t data = { 0 };
    printf("sizeof(uint32_t) = %lu\n", sizeof(uint32_t));
    printf("sizeof(data_t) = %lu\n", sizeof(data_t));
    printf("sizeof(data) = %lu\n", sizeof(data));
    return EXIT_SUCCESS;
}
