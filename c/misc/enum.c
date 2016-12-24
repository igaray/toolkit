#include <stdio.h>

typedef enum {
  A, B, C, N
} enum_t;

int main() {
  enum_t a = A, b = B, c = C, n = N;

  printf("a: %d b: %d c: %d n: %d\n", a, b, c, n);
}

