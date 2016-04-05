#include <stdio.h>
#include "linked_list_simple.h"

typedef struct node_s {
  data_t data;
  list_t next;
} node_t;

list_t linked_list_new(data_t d) {
  list_t x = malloc(sizeof(node_t));
  x->data = d;
  x->next = NULL;
  return x;
}

void linked_list_free(list_t x) { free(x); }

void linked_list_insert(list_t l1, list_t l2) {
  l2->next = l1->next;
  l1->next = l2;
}

list_t linked_list_delete(list_t l) {
  list_t t = l->next;
  l->next = t->next;
  return t;
}

list_t linked_list_next(list_t l) { return l->next; }

int linked_list_item(list_t l) { return l->data; }

void linked_list_print(list_t l) {
  printf("l: [");
  while (l != NULL) {
    printf("%d", l->data);
    l = l->next;
    if (l != NULL) {
      printf(" ");
    }
  }
  printf("]\n");
}

// EOF linked_list_simple.c
