#include <stdlib.h>
#include "disjoint_sets.h"

void disjoint_set_init(disjoint_set_t* s, size_t n) {
    (*s)         = malloc(sizeof(disjoint_set_t));
    (*s)->sets   = malloc(n * sizeof(unsigned int));
    (*s)->height = malloc(n * sizeof(unsigned int));
    (*s)->size   = n;
    zero_int_array((*s)->sets, n);
    zero_int_array((*s)->height, n);
}

void disjoint_set_free(disjoint_set_t set) {
    free(set->sets);
    free(set->height);
    free(set);
}

int find(disjoint_set_t s, int n, int x) {
    int* sets = s->sets;
    int r, i, j;

    r = x;
    while (sets[r] != r) {
        r = sets[r];
    }
    i = x;
    while (i != r) {
        j = sets[i];
        sets[i] = r;
        i = j;
    }
    return r;
}

void merge(disjoint_set_t s, int a, int b) {
    int* sets = s->sets;
    int* height = s->height;

    if (height[a] == height[b]) {
        height[a] += 1;
        sets[b] = a;
    }
    else {
        if (height[a] > height[b]) {
            sets[b] = sets[a];
        }
        else {
            sets[a] = sets[b];
        }
    }
}

/* EOF: disjoint_sets.c */
