#include <stdio.h>
#include "stack_array.h"

stckarr_declare(int);

int main() {

  stckarr_type(int)ds;
  stckarr_init(int, ds, 16);

  stckarr_push(int, ds, 1);
  stckarr_push(int, ds, 2);
  stckarr_push(int, ds, 3);
  printf("%d\n", *(stckarr_pop(int, ds)));
  printf("%d\n", *(stckarr_pop(int, ds)));
  printf("%d\n", *(stckarr_pop(int, ds)));

  // This can't be done, because the pop operation returns a null pointer if
  // the stack is empty, so we would be dereferencing a null pointer.
  // printf("%d\n", *(stckarr_pop(int, ds)));

  return 0;
}
