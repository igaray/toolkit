#include <stdio.h>
#include <stdlib.h>
#include "linked_list.h"

int main() {
  list_t *l = NULL;
  data_t d;

  l = linked_list_new();
  linked_list_destroy(l);
  free(l);

  l = linked_list_new();
  linked_list_print(l);
  linked_list_push(l, 1);
  linked_list_print(l);
  linked_list_push(l, 2);
  linked_list_print(l);
  linked_list_push(l, 3);
  linked_list_print(l);
  linked_list_pop(l, &d);
  linked_list_print(l);
  linked_list_pop(l, &d);
  linked_list_print(l);
  linked_list_pop(l, &d);
  linked_list_print(l);
  linked_list_destroy(l);
  free(l);

  /*
  l = linked_list_new();
  for (int i = 0; i < 5; i++) {
    linked_list_insert();
  }
  free(l);
  */

  printf("linked_list: OK\n");
  return EXIT_SUCCESS;
}
