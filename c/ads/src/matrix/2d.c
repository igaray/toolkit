#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

const uint32_t xmax = 10;
const uint32_t ymax = 10;

void print_matrix(uint32_t grid[xmax][ymax]) {

  uint32_t x, y;

  printf("M = [\n");
  for (x = 0; x < xmax; x++) {
    for (y = 0; y < ymax; y++) {
      printf("%5d,", grid[x][y]);
    }
    printf(";\n");
  }
  printf("]\n");
}

int main() {

  uint32_t grid[xmax][ymax];
  uint32_t i, x, y, steps = 1;

  for (i = 1; i <= steps; i++) {
    printf("step: %d\n", i);
    for (x = 0; x < xmax; x++) {
      for (y = 0; y < ymax; y++) {
        grid[x][y] = x * y;
      }
    }
  }
  print_matrix(grid);

  return 0;
}
