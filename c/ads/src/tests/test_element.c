#include <stdio.h>
#include <stdlib.h>
#include "element.h"

int main() {
  int err = 0;
  element_t e;

  element_init(&e);

  if (0 != element_get(e)) {
    err = 1;
  }

  if (0 != element_set(e, 1)) {
    err = 2;
  }

  if (1 != element_get(e)) {
    err = 3;
  }

  if (0 != element_set(e, 2)) {
    err = 4;
  }

  if (2 != element_get(e)) {
    err = 5;
  }

  element_free(&e);

  if (err) {
    printf("element: ERROR (%d)\n", err);
  } else {
    printf("element: OK\n");
  }
  exit(EXIT_SUCCESS);
}
