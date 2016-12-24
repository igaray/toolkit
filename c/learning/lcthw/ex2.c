#include <stdio.h>

void fibo(size_t n) {

  unsigned int f[n];
  for (size_t i = 0; i < n; i++)
  {
    f[i] = i;
  }
  for (size_t i = 0; i < n; i++)
  {
    printf("%u ", f[i]);
  }
  printf("\n");
}

int main()
{
  int array[] = {0, 1, 1, 2, 3, 5, 8, 13, 21, 34};
  size_t sz = sizeof(array) / sizeof(int);
  printf("sz: %zu\n", sz);

  fibo(10);
}
