#ifndef _LINKED_LIST_SIMPLE_H_
#define _LINKED_LIST_SIMPLE_H_

#include <stdlib.h>

/* This file implements the simplest linked list possible.
 * A list is a pointer to a a node.
 * Each node in a list is a struct with a data element and single link to the
 * next element, and must be allocated and freed by the owner.
 */
typedef int data_t;

typedef struct node_s node_t;

typedef node_t *list_t;

/* Creates a new linked list with a single element, and returns it.
 */
list_t linked_list_new(data_t d);

/* Frees a node.
 */
void linked_list_free(list_t l);

/* Inserts a list node l2 into a list l1. The tail of l1 will become the tail
 * of l2.
 */
void linked_list_insert(list_t l1, list_t l2);

/*
 */
list_t linked_list_delete(list_t l);

/*
 */
list_t linked_list_next(list_t l);

/*
 */
int linked_list_item(list_t l);

/*
 */
void linked_list_print(list_t l);

#endif

// EOF linked_list_simple.h
