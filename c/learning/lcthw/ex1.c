#include <stdio.h>
#include <stdlib.h>

int main()
{
  unsigned short fibo[] = {0, 1, 1, 2, 3, 5, 8, 13, 21, 34};
  printf("sizeof(fibo): %zu\n", sizeof(fibo));

  unsigned short array[10];
  printf("sizeof(array): %zu\n", sizeof(array));

  unsigned short* heap = malloc(100 * sizeof(unsigned short));
  printf("sizeof(heap): %zu\n", sizeof(heap));

  void* vp = NULL;
  printf("sizeof(vp): %zu\n", sizeof(vp));

  char* name1 = "name1";
  char name2[] = "name2";
  printf("sizeof(name1): %s %zu\n", name1, sizeof(name1));
  printf("sizeof(name2): %s %zu\n", name2, sizeof(name2));

  // compiles but causes segfault
  /*
  printf("name: %s", name1);
  name1[0] = 'N';
  printf("name: %s", name1);
  */


  }
