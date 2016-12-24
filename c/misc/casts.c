#include <stdio.h>

int main() {
  int i;
  float f;
  double d;

  i = 5;
  f = i;
  printf("i: %d f: %f\n", i, f);

  f = 3.6;
  i = f;
  printf("i: %d f: %f\n", i, f);
  return 0;
}
