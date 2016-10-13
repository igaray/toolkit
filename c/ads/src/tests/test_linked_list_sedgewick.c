#include <stdio.h>
#include <stdlib.h>
#include "linked_list_sedgewick.h"

int main() {
  linked_list_init(32);
  list_t l = NULL;
  linked_list_print(l);

  l = linked_list_new(0);
  for (int i = 5; i > 0; i--) {
    list_t t = linked_list_new(i);
    linked_list_insert(l, t);
  }

  linked_list_print(l);
  int i = 0;
  do {
    printf("%d.\n", i);
    linked_list_print(l);
    if (l != NULL) {
      l = linked_list_delete(l);
    }
    i++;
  } while (l != NULL);
  linked_list_print(l);

  printf("linked_list_sedgewick: OK\n");
  return EXIT_SUCCESS;
}
