#ifndef __DS_UTILS_H__
#define __DS_UTILS_H__

/* TYPES */

typedef unsigned short int byte;
typedef unsigned int uint;
typedef unsigned long int ulint;

typedef struct point { int x, y, z; } point_t;

typedef struct line { point_t p0, p1; } line_t;

/*
 * point_t p1, p2;
 * line_t l1;
 *
 * p0.x = 1;
 * p0.y = 1;
 * p0.z = 1;
 * p1.x = 2;
 * p1.y = 2;
 * p1.z = 2;
 * l.p0 = p0;
 * l.p1 = p1;
 */

/* ALGORITHMS */

/* Zeroes an array of integer types.
 * Example use:
 *
 * uint a[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
 * zero_array(a, 10);
 */
void zero_array(int a[], size_t size) {
  size_t i = 0;

  for (i = 0; i < size; i++) {
    a[i] = 0;
  }
}

/* Prints an array of integer types to stdout.
 * Example use:
 *
 * uint a[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
 * print_array("a: ", a, 10);
 */
void print_array(char *name, int a[], size_t size) {
  size_t i = 0;

  printf("%s: [", name);
  for (i = 0; i < (size - 1); i++) {
    printf("%d, ", a[i]);
  }
  if (size > 0) {
    printf("%d", a[size - 1]);
  }
  printf("]\n");
}

/* Compares two integers. */
int cmp(int a, int b) {
  if (a < b)
    return -1;
  if (a == b)
    return 0;
  if (a > b)
    return 1;
}

#endif

/* EOF: ds_utils.h */
