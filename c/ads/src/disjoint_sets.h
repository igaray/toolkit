#ifndef _DISJOINT_SETS_H_
#define _DISJOINT_SETS_H_
#include <stdlib.h>

typedef struct {
  int label;
  void *data;
} disjoint_set_element_t;

typedef struct {
  size_t size;
  int *sets;
  int *height;
} disjoint_set_struct_t;
typedef disjoint_set_struct_t *disjoint_set_t;

/* Create and initialize a set of disjoint sets. */
void disjoint_set_init(disjoint_set_t *s, size_t n);

/* Free the memory used by a disjoint set. */
void disjoint_set_free(disjoint_set_t set);

/* Find the label of the set containing element x. */
int find(disjoint_set_t s, int n, int x);

/* Merges the sets labelled a and b, assumes a != b. */
void merge(disjoint_set_t s, int a, int b);

#endif

/* EOF: disjoint_sets.h */
