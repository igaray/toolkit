#include <stdio.h>
#include <stdint.h>
#include "mastrix_fa_i.h"
#include "file.c"

typedef uint32_t T;

#define AT(x, m, i, j) (x + i * m + j)

/*
 * Parameters:
 * Return value:
 * Description:
 */
matrix_fa_t matrix_alloc(uint32_t m, uint32_t n) {
  matrix_fa_t x = {.c = m, .r = n};
  x.v = (T*)malloc(m * n * sizeof(T*));
  return x;
}

/*
 * Parameters:
 * Return value:
 * Description:
 */
void matrix_free(matrix_fa_fa_t* m) {
  free(m->v);
}

/*
 * Parameters:
 * Return value:
 * Description:
 */
void matrix_zero(matrix_fa_t* m) {
  for (int i = 0; i < m->c; i++) {
    for (int j = 0; j < m->r; j++) {
      *(AT(m->v, m->c, i, j)) = 0;
    }
  }
}

/*
 * Parameters:
 * Return value:
 * Description:
 */
void matrix_print(matrix_fa_t* m) {
  for (int i = 0; i < m->c; i++) {
    for (int j = 0; j < m->r; j++) {
      printf("%d ", *(AT(m->v, m->c, i, j)));
    }
    printf("\n");
  }
}

/*
 * Parameters:
 * Return value:
 * Description:
 */
T matrix_get(matrix_fa_t* m, uint32_t i, uint32_t j) {
  return *(AT(m->v, m->c, i, j));
}

/*
 * Parameters:
 * Return value:
 * Description:
 */
void matrix_set(matrix_fa_t* m, uint32_t i, uint32_t j, T v) {
  *(AT(m->v, m->c, i, j)) = v;
}

/*
 * Parameters:
 * Return value:
 * Description:
 */
void matrix_read(matrix_fa_t* x, const char* file_name) {
  int err = 0;
  int fd, file_size, lines;
  char* p = NULL;

  err = open_file(file_name, &fd, &file_size, &p);
  if (err) {
    printf("Error opening file %s\n", file_name);
    exit(EXIT_FAILURE); 
  }

  errno = 0;
  long int m = strtod(p, &p);
  errno = 0;
  long int n = strtod(p, &p);

  *x = matrix_alloc(m, n);
  for (int i = 0; i < x->c; i++) {
    for (int j = 0; j < x->r; j++) {
       x->v[i][j] = strtod(p, &p);
    }
  }

  close_file(fd);
}

