#include <stdio.h>
#include "linked_list.h"

list_t* linked_list_new()
{
  list_t* l = (list_t*)malloc(sizeof(list_t));
  linked_list_init(l);
  return l;
}

void linked_list_init(list_t* l)
{
  if (l != NULL)
  {
    l->first = NULL;
    l->last = NULL;
    l->count = 0;
  }
}

void linked_list_destroy(list_t* l)
{
  if (l != NULL)
  {
    link_t k = NULL;

    k = l->first;
    while (k != NULL)
    {
      link_t t = k;
      k = k->next;
      free(t);
    }
    l->first = NULL;
    l->last = NULL;
    l->count = 0;
  }
}

size_t linked_list_count(list_t* l)
{
  if (l != NULL)
  {
    return l->count;
  }
  else
  {
    return 0;
  }
}

link_t linked_list_first(list_t* l)
{
  if (l != NULL)
  {
    return l->first;
  }
  else
  {
    return NULL;
  }
}

link_t linked_list_last(list_t* l)
{
  if (l != NULL)
  {
    return l->last;
  }
  else
  {
    return NULL;
  }
}

link_t linked_list_index(list_t* l, size_t idx)
{
  if (l != NULL)
  {
    link_t k = l->first;
    for (size_t i = 0; (k != NULL) && (i < idx); i++)
    {
      k = k->next;
    }
    return k;
  }
  else
  {
    return NULL;
  }
}

link_t linked_list_next(link_t l)
{
  if (l != NULL)
  {
    return l->next;
  }
  else
  {
    return NULL;
  }
}

int linked_list_item(link_t l, data_t* d)
{
  if (l != NULL)
  {
    *d = l->data;
    return 0;
  }
  else
  {
    return 1;
  }
}

void linked_list_push(list_t* l, data_t d)
{
  if (l != NULL)
  {
    link_t n = (node_t*)malloc(sizeof(node_t));
    n->data = d;
    n->next = l->first;
    l->first = n;
    l->count++;
  }
}

int linked_list_pop(list_t* l, data_t* d)
{
  if (l != NULL)
  {
    *d = l->first->data;
    link_t t = l->first;
    l->first = l->first->next;
    l->count--;
    free(t);
    return 0;
  }
  else
  {
    return 1;
  }
}

int linked_list_insert(list_t* l, link_t k, data_t d)
{
  //TODO handle first and last
  link_t n = (node_t*)malloc(sizeof(node_t));

  if (l == NULL)
  {
    return 1;
  }
  if (k == NULL)
  {
    return 2;
  }
  if (n == NULL)
  {
    return 3;
  }
  n->data = d;
  n->next = k->next;
  k->next = n;
  l->count++;
  return 0;
}

link_t linked_list_delete(list_t* l, link_t k)
{
  //TODO handle errors
  //TODO handle first and last
  link_t t = k->next;
  k->next = t->next;
  free(t);
  l->count--;
  return k->next;
}

void linked_list_print(list_t* l)
{
  if (l != NULL)
  {
    link_t k = l->first;
    printf("c: %zu l: [", linked_list_count(l));
    while (k != NULL)
    {
      printf("%d", k->data);
      k = k->next;
      if (k != NULL)
      {
        printf(" ");
      }
    }
    printf("]\n");
  }
}

// EOF linked_list.c
