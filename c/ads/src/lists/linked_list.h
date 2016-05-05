#ifndef _LINKED_LIST_H_
#define _LINKED_LIST_H_

#include <stdlib.h>

/* This file implements a general-purpose linked list.
 */

typedef int data_t;

typedef struct node_s* link_t;

typedef struct node_s {
  data_t data;
  link_t next;
} node_t;

typedef struct list_s {
  link_t first;
  link_t last;
  size_t count;
} list_t;

/* Creators & Destroyers */

/*
 */
list_t *linked_list_new();

/*
 */
void linked_list_init(list_t *l);

/*
 */
void linked_list_destroy(list_t *l);

/* Getters */

/*
 */
size_t linked_list_count(list_t *l);

/*
 */
link_t linked_list_first(list_t *l);

/*
 */
link_t linked_list_last(list_t *l);

/*
 */
link_t linked_list_index(list_t *l, size_t i);

/*
 */
link_t linked_list_next(link_t l);

/*
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
