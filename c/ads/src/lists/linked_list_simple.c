#include <stdio.h>
#include "linked_list_simple.h"

typedef struct node_s {
  data_t data;
  list_t next;
} node_t;

list_t linked_list_new(data_t d) {
  list_t x = malloc(sizeof(node_t));
  if (x != NULL) {
    x->data = d;
    x->next = NULL;
  }
  else {
    return NULL;
  }
  return x;
}

void linked_list_free(list_t l) { free(l); }

void linked_list_insert(list_t l1, list_t l2) {
  if ((l1 != NULL) && (l2 != NULL)) {
    l2->next = l1->next;
    l1->next = l2;
  }
}

list_t linked_list_delete(list_t l) {
  list_t t = NULL;
  if (l != NULL) {
    t = l->next;
    if (t != NULL) {
      l->next = t->next;
    }
    free(l);
  }
  return t;
}

list_t linked_list_next(list_t l) {
  if (l != NULL) { return l->next; }
  else {return NULL; }
}

int linked_list_item(list_t l) {
  if (l != NULL) { return l->data; }
  else { return NULL; }
}

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
