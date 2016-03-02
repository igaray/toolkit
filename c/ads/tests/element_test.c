#include <stdio.h>
#include <stdlib.h>
#include "element.h"

int main() {
    element_t e;
    unsigned int i;
    int rv;

    printf("testing element_init\n");
    element_init(&e);

    printf("testing element_get\n");
    printf("value: %d\n", element_get(e));

    printf("testing set\n");
    rv = element_set(e, 1);
    if (rv == 0) {
        printf("element_set successful\n");
    }
    else {
        printf("element_set failed. return value: %d\n", rv);
    }

    printf("testing element_get\n");
    printf("value: %d\n", element_get(e));

    printf("testing element_set\n");
    rv = element_set(e, 2);
    if (rv == 0) {
        printf("element_set successful\n");
    }
    else {
        printf("element_set failed. return value: %d\n", rv);
    }

    printf("testing element_get\n");
    printf("value: %d\n", element_get(e));

    printf("testing element_free\n");
    element_free(&e);
    exit(EXIT_SUCCESS);
}
