#include <stdio.h>
#include <stdlib.h>
#include "linked_list_simple.h"

int main()
{
  list_t l = NULL;

  linked_list_print(l);
  l = linked_list_new(0);
  linked_list_print(l);

  for (int i = 5; i > 0; i--)
  {
    list_t t = linked_list_new(i);
    linked_list_insert(l, t);
  }
  linked_list_print(l);

  do {
    linked_list_print(l);
    l = linked_list_delete(l);
  } while (l != NULL);
  linked_list_print(l);

  printf("linked_list_simple: OK\n");
  return EXIT_SUCCESS;
}
