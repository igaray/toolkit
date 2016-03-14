
#ifndef _LINKED_LIST_SIMPLE_H_
#define _LINKED_LIST_SIMPLE_H_

#include <stdlib.h>

typedef int data_t;

typedef struct node_s node_t;

typedef node_t* list_t;

/*
 */
void   linked_list_init(int);

/*
 */
list_t linked_list_new(data_t d);

/*
 */
void   linked_list_free(list_t l);

/*
 */
void   linked_list_insert(list_t l1, list_t l2);

/*
 */
list_t linked_list_delete(list_t l);

/*
 */
list_t linked_list_next(list_t l);

/*
 */
int    linked_list_item(list_t l);

/*
 */
void   linked_list_print(list_t l);

#endif

// EOF linked_list_simple.h
