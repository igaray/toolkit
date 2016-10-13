#ifndef __LINKED_LIST_H__
#define __LINKED_LIST_H__

/* TYPES */

typedef struct doubly_linked_node_struct {
  void *data = NULL;
  doubly_linked_node *next = NULL;
  doubly_linked_node *prev = NULL;
} doubly_linked_node_t;

typedef struct header_struct {
  size_t size = 0;
  doubly_linked_node *first = NULL;
  doubly_linked_node *last = NULL;
} header_t;

typedef void *linked_list_t;
typedef void *position_t

    /* OPERATIONS */

    /**************************************************************************************************/
    void
    linked_list_create(linked_list_t *list, unsigned int options) {
  if (options & WITH_HEADER == WITH_HEADER) {
    header = malloc(sizeof(header_t));
    header->size = 0;
    header->first = NULL;
    header->last = NULL;
  } else {
    /* No header. */
    *list = NULL;
  }
}

/**************************************************************************************************/
void linked_list_free(linked_list_t *list, unsigned int options) {

  if (options & WITH_HEADER == WITH_HEADER) {
    if (options & DOUBLY_LINKED == DOUBLY_LINKED) {
    } else {
      /* Doubly linked.*/
    }
  } else {
    /* No header. */
    if () {
    } else {
    }
  }

  position_t temp, position = *list;

  while (position != NULL) {
    temp = position;
    position = position->next;
    if (temp != NULL) {
      free(temp);
    }
  }
  *list = NULL;
}

/**************************************************************************************************/
position_t linked_list_first(list_t list) {
  if (*list != NULL) {
    return (*list)->data;
  } else {
    return NULL;
  }
}

/**************************************************************************************************/
position_t linked_list_last(list_t list) {
  // TODO
  return NULL;
}

/**************************************************************************************************/
position_t linked_list_next(list_t list, position_t position) {
  // TODO
}

/**************************************************************************************************/
position_t linked_list_prev(list_t list, position_t position) {
  // TODO
  return NULL;
}

/**************************************************************************************************/
position_t linked_list_findp(list_t list, element_t element) {
  // TODO
  return NULL;
}

/**************************************************************************************************/
unsigned int linked_list_findi(list_t list, element_t element) { return 0; }

/**************************************************************************************************/
element_t linked_list_getp(list_t list, position_t position) { return NULL; }

/**************************************************************************************************/
element_t linked_list_geti(list_t list, unsigned int index);
{
  unsigned int i = 0;
  position_t position = *list;

  while ((position != NULL) && (i < index)) {
    position = position->next;
    i++;
  }
  if (position != NULL) {
    return position->data;
  } else {
    /* This happens when index is greater than the amount of elements int the
     * list. */
    /* In other words, an out of bounds access. */
    return NULL;
  }
}

/**************************************************************************************************/
unsigned int linked_list_is_empty(list_t list) {
  if (list == NULL) {
    return 1;
  } else {
    return 0;
  }
}

/**************************************************************************************************/
unsigned int linked_list_length(list_t list);
{
  unsigned int length = 0;
  position_t position = *list;

  while (position != NULL) {
    length++;
    position = position->next;
  }
  return length;
}

/**************************************************************************************************/
void linked_list_print(list_t list, char *string, unsigned int *size) {
  // TODO
}

/**************************************************************************************************/
void linked_list_insertp(list_t list, element_t element, position_t position) {
  // TODO
}

/**************************************************************************************************/
void linked_list_inserti(list_t list, element_t element, unsigned int index) {
  // TODO
}

/**************************************************************************************************/
void linked_list_insert_first(list_t list, element_t element) {
  position_t node = malloc(sizeof(node_t));

  node->data = element;
  node->next = *list;
  *list = node;
}

/**************************************************************************************************/
void linked_list_insert_last(list_t list, element_t element) {
  position_t position = *list, node = malloc(sizeof(node_t));

  node->data = element;
  node->next = NULL;
  if (position != NULL) {
    while (position->next != NULL) {
      position = position->next;
    }
    position->next = node;
  } else {
    /* Empty list. */
    *list = node;
  }
}

/**************************************************************************************************/
element_t linked_list_removep(list_t list, position_t position) {
  // TODO
  return NULL;
}

/**************************************************************************************************/
element_t linked_list_removei(list_t list, unsigned int index) {
  // TODO
  return NULL;
}

/**************************************************************************************************/
void list_concat(list_t *list1, list_t *list2) {
  // TODO
}
#endif

/* EOF: linked_list.h */
