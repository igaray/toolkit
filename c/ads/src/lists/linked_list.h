#ifndef _LINKED_LIST_H_
#define _LINKED_LIST_H_

#include <stdlib.h>

/* This file implements a general-purpose linked list.
 */

/* Types */

typedef int data_t;

typedef struct node_s *link_t;

typedef struct list_s list_t;

/* Creators & Destroyers */

/* Allocates a new empty list on the heap.
 */
list_t *linked_list_new();

/* Initializes a new list.
 * TODO: test
 * TODO: add a parameter size_t cap to initialize the pool to a certain capacity
 */
void linked_list_init(list_t *l);

/* Reclaims a list and all its nodes.
 */
void linked_list_destroy(list_t *l);

/* Getters */

/* Returns the numer of items in the list.
 */
size_t linked_list_count(list_t *l);

/* Returns a link to the first element in the list.
 */
link_t linked_list_first(list_t *l);

/* Returns a link to the last element in the list.
 */
link_t linked_list_last(list_t *l);

/* Returns a link to the i-th element in the list, indexing is 0-based.
 */
link_t linked_list_index(list_t *l, size_t i);

/* Returns a link to the node next to a given link.
 */
link_t linked_list_next(link_t l);

/* Returns the data stored in a given node.
 * d is an out parameter, the return value is an error code.
 */
int linked_list_item(link_t l, data_t *d);

/* Setters */

/*
 */
void linked_list_push(list_t *l, data_t d);

/*
 */
int linked_list_pop(list_t *l, data_t *d);

/*
 */
int linked_list_insert(list_t *l, link_t k, data_t d);

/*
 */
link_t linked_list_delete(list_t *l, link_t k);

/* Misc */

/*
 */
void linked_list_print(list_t *l);

#endif

// EOF linked_list.h
