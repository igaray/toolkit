#include <stdio.h>
#include <stdlib.h>

// Uncomment this to enable bounds checking.
//#define NDEBUG

#include <assert.h>
#include "dynamic_array.h"

/*
dynarr_declare(type) var;
dynarr_init(type, var, size);
dynarr_free(type, var);
dynarr_resize(type, var, size);
dynarr_get(type, var, idx);
dynarr_set(type, var, idx, val);
*/

dynarr_declare(int);
dynarr_declare(float);

// The preprocessor can't concatenate "void*" to make the array type
// constructor, so we have to define a name for this type and then pass that to
// the macro.
typedef void* void_ptr;
dynarr_declare(void_ptr);

int main() {

    size_t i;

    dynarr_type(int)      ilist;
    dynarr_type(float)    flist;
    dynarr_type(void_ptr) vplist;

    dynarr_init(int, ilist, 32);
    for (i = 0; i < ilist.size; i++) {
        dynarr_set(int, ilist, i, 100);
    }
    // Watch out! dynarr_get returns a pointer to the data, not the data itself.
    for (i = 0; i < ilist.size; i++) {
        printf("%d ", *(dynarr_get(int, ilist, i)));
    }
    printf("\n");

    printf("size: %d\n", ilist.size);
    dynarr_set(int, ilist, 100, 666);
    printf("size: %d\n", ilist.size);
    dynarr_free(int, ilist);
    printf("size: %d\n", ilist.size);
    printf("\n");

    dynarr_init(float, flist, 16);
    for (i = 0; i < flist.size; i++) {
        dynarr_set(float, flist, i, 2.0);
    }
    for (i = 0; i < flist.size; i++) {
        printf("%.2f ", flist.data[i]);
    }
    dynarr_resize(float, flist, flist.size * 3);
    dynarr_free(float, flist);
    printf("\n");

    dynarr_init(void_ptr, vplist, 16);
    for (i = 0; i < vplist.size; i++) {
        dynarr_set(void_ptr, vplist, i, NULL);
    }
    
    return 0;
}
