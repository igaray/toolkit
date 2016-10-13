#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include "matrix_aa_i.h"
#include "file.h"

matrix_aa_i_t matrix_aa_i_alloc(uint32_t m, uint32_t n) {
  matrix_aa_i_t x = {.c = m, .r = n};
  x.v = malloc(m * sizeof(T *));
  for (int i = 0; i < m; i++) {
    x.v[i] = (T *)malloc(n * sizeof(T *));
  }
  return x;
}

void matrix_aa_i_free(matrix_aa_i_t *m) {
  for (int i = 0; i < m->c; i++) {
    free(m->v[i]);
  }
  free(m->v);
}

void matrix_aa_i_zero(matrix_aa_i_t *m) {
  for (int i = 0; i < m->c; i++) {
    for (int j = 0; j < m->r; j++) {
      m->v[i][j] = 0;
    }
  }
}

void matrix_aa_i_print(matrix_aa_i_t *m) {
  for (int i = 0; i < m->c; i++) {
    for (int j = 0; j < m->r; j++) {
      printf("%d ", m->v[i][j]);
    }
    printf("\n");
  }
}

T matrix_aa_i_get(matrix_aa_i_t *m, uint32_t i, uint32_t j) {
  return m->v[i][j];
}

void matrix_aa_i_set(matrix_aa_i_t *m, uint32_t i, uint32_t j, T v) {
  m->v[i][j] = v;
}

int matrix_aa_i_read(matrix_aa_i_t *x, const char *file_name) {
  int fd, file_size, lines;
  char *p = NULL;

  errno = open_file(file_name, &fd, &file_size, &p);
  if (errno) {
    return errno;
  }

  errno = 0;
  long m = strtod(p, &p);
  if (errno) {
    return errno;
  }
  errno = 0;
  long n = strtod(p, &p);
  if (errno) {
    return errno;
  }

  *x = matrix_aa_i_alloc(m, n);
  for (int i = 0; i < x->c; i++) {
    for (int j = 0; j < x->r; j++) {
      x->v[i][j] = strtod(p, &p);
    }
  }

  close(fd);
  return 0;
}
