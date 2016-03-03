#include <stdlib.h>
#include <stdio.h>
#include "matrix_fa_i.h"

int main(int argc, char* argv[]) {
  int err = 0;

  matrix_fa_i_t matrix;

  printf("Test 1:\n");
  matrix = matrix_fa_i_alloc(3, 3);
  matrix_fa_i_zero(&matrix);
  matrix_fa_i_set(&matrix, 1, 1, 1);
  matrix_fa_i_print(&matrix);
  matrix_fa_i_free(&matrix);

  printf("Test 2:\n");
  matrix = matrix_fa_i_alloc(4, 4);
  matrix_fa_i_zero(&matrix);
  matrix_fa_i_set(&matrix, 1, 1, 1);
  matrix_fa_i_print(&matrix);
  matrix_fa_i_free(&matrix);

  printf("Test 3:\n");
  err = matrix_fa_i_read(&matrix, "matrix_fa_i_test.txt");
  if (err) {
    return EXIT_FAILURE;
  }
  matrix_fa_i_print(&matrix);
  matrix_fa_i_free(&matrix);

  return EXIT_SUCCESS;
}

