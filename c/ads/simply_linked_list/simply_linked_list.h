#ifndef _SIMPLY_LINKED_LIST_H_
#define _SIMPLY_LINKED_LIST_H_

/* TYPES */

typedef struct sll_node_struct {
    void*       head = NULL;
    sll_node_t* next = NULL;
} sll_node_t;

typedef struct sll_header_struct {
    size_t      size = 0;
    sll_node_t* head = NULL;
    sll_node_t* last = NULL;
} header_t;

typedef header_t* simply_linked_list_t;

/* OPERATIONS */

void         sll_init(simply_linked_list_t* list);                          /* Initialize a list. O(1) */
void         sll_free(simply_linked_list_t* list);                          /* Destroy a list and free all its memory. O(n) where n: number of elements in the list */
sll_node_t*  sll_first(list_t list);                                        /* Extract the element at the beginning of the list. */
sll_node_t*  sll_last(list_t list);
sll_node_t*  sll_next(list_t list, sll_node_t* position);
sll_node_t*  sll_prev(list_t list, sll_node_t* position);
sll_node_t*  sll_findp(list_t list, void* element);                         /* Find the position of an element in a list. */
unsigned int sll_findi(list_t list, void* element);                         /* Find the position of an element in a list. */
void*        sll_getp(list_t list, sll_node_t* position);                   /* Retrieve an element at a given position. */
void*        sll_geti(list_t list, unsigned int index);                     /* Retrieve an element at a given position. */
unsigned int sll_is_empty(list_t list);                                     /* Inspect whether the list is empty. */
unsigned int sll_length(list_t list);                                       /* Inspect the number of elements in the list. */
void         sll_insertp(list_t list, void* element, sll_node_t* position);
void         sll_inserti(list_t list, void* element, unsigned int index);   /* Insert an element at a given position. */
void         sll_insert_first(list_t list, void* element);                  /* Insert an element at the beginning of the list. */
void         sll_insert_last(list_t list, void* element);                   /* Insert an element at the end of the list. */
void*        sll_removep(list_t list, sll_node_t* position);                /* Expunge an element at a given position from the list.*/
void*        sll_removei(list_t list, unsigned int index);                  /* Expunge an element at a given position from the list.*/
void         list_concat(list_t* list1, list_t* list2);                     /* Concatenate two lists. */
void         list_sort(list_t* list)                                        /* Sort a list in-place. */
void         sll_print(list_t list, char *string, unsigned int *size);      /* Prints out the elements in a list, mostly for debugging purposes. */

/*
x in s                              member(list, element)
x not in s
s + t                               concatenate(list, list, list) copy semantics?
s[i]
s[i:j]                              slice(list, i, j, 1)
s[i:j:k]                            slice(list, i, j, k)
len(s)                              length(list)
min(s)                              min(list, cmpfunc)
max(s)                              max(list, cmpfunc)
s[i] = x                            item i of s is replaced by x     
s[i:j] = t                          slice of s from i to j is replaced by the contents of the iterable t     
del s[i:j]                          same as s[i:j] = []      
s[i:j:k] = t                        the elements of s[i:j:k] are replaced by those of t
del s[i:j:k]                        removes the elements of s[i:j:k] from the list
s.append(x)                         same as s[len(s):len(s)] = [x]
s.extend(x)                         same as s[len(s):len(s)] = x
s.count(x)                          return number of i‘s for which s[i] == x
s.index(x[, i[, j]])                return smallest k such that s[k] == x and i <= k < j
s.insert(i, x)                      same as s[i:i] = [x]
s.pop([i])                          same as x = s[i]; del s[i]; return x
s.remove(x)                         same as del s[s.index(x)]
s.reverse()                         reverses the items of s in place
s.sort([cmp[, key[, reverse]]])     sort the items of s in place
*/

/*************************************************************************/
void sll_init( simply_linked_list_t* list ) 
{
    *list = malloc(sizeof(header_t));
    list->size  = 0;
    list->first = NULL;
    list->last  = NULL;
}

/*************************************************************************/
void sll_free( simply_linked_list_t* list )
{
    sll_node_t* temp     = NULL;
    sll_node_t* position = (*list)->head;
    
    while (position != NULL) 
    {
        temp     = position;
        position = position->next;
        if (temp != NULL) 
        {
            free(temp);
        }
    }
    *list = NULL;
}

/**************************************************************************************************/
sll_node_t* sll_first( simply_linked_list_t list )
{
    if (*list != NULL) 
    {
        return list->first;
    }
    else 
    {
        return NULL;
    }
}

/**************************************************************************************************/
sll_node_t* sll_last( simply_linked_list_t list )
{
    if (*list != NULL)
    {
        return list->last;
    }
    else
    {
        return NULL;
    }
}

/**************************************************************************************************/
sll_node_t* simply_linked_list_next(list_t list, sll_node_t* position)
{
    // TODO
}

/**************************************************************************************************/
sll_node_t* simply_linked_list_prev(list_t list, sll_node_t* position)
{
    // TODO
    return NULL;
}

/**************************************************************************************************/
sll_node_t* simply_linked_list_findp(list_t list, void* element)
{
    // TODO
    return NULL;
}

/**************************************************************************************************/
unsigned int simply_linked_list_findi(list_t list, void* element)
{
    return 0;
}

/**************************************************************************************************/
void* simply_linked_list_getp(list_t list, sll_node_t* position)
{
    return NULL;
}

/**************************************************************************************************/
void* linked_list_geti(list_t list, unsigned int index);
{
    unsigned int i = 0;
    sll_node_t* position = *list;

    while ((position != NULL) && (i < index)) 
    {
        position = position->next;
        i++;
    }
    if (position != NULL) 
    {
        return position->data;
    }
    else 
    {
        /* This happens when index is greater than the amount of elements int the list. */
        /* In other words, an out of bounds access. */
        return NULL;
    }
}

/**************************************************************************************************/
unsigned int simply_linked_list_is_empty(list_t list)
{
    if (list == NULL)
    {
        return 1;
    }
    else
    {
        return 0;
    }
}

/**************************************************************************************************/
unsigned int simply_linked_list_length(list_t list);
{
    unsigned int length = 0;
    sll_node_t* position = *list;

    while (position != NULL) 
    {
        length++;
        position = position->next;
    }
    return length;
}

/**************************************************************************************************/
void simply_linked_list_print(list_t list, char *string, unsigned int *size)
{
    // TODO
}

/**************************************************************************************************/
void simply_linked_list_insertp(list_t list, void* element, sll_node_t* position)
{
    // TODO
}

/**************************************************************************************************/
void simply_linked_list_inserti(list_t list, void* element, unsigned int index)
{
    // TODO
}

/**************************************************************************************************/
void simply_linked_list_insert_first(list_t list, void* element)
{
    sll_node_t* node = malloc(sizeof(node_t));

    node->data = element;
    node->next = *list;
    *list = node;
}

/**************************************************************************************************/
void simply_linked_list_insert_last(list_t list, void* element)
{
    sll_node_t* position = *list,
               node     = malloc(sizeof(node_t));

    node->data = element;
    node->next = NULL;
    if (position != NULL) 
    {
        while (position->next != NULL) 
        {
            position = position->next;
        }
        position->next = node;
    }
    else 
    {
        /* Empty list. */
        *list = node;
    }
}

/**************************************************************************************************/
void* simply_linked_list_removep(list_t list, sll_node_t* position)
{
    // TODO
    return NULL;
}

/**************************************************************************************************/
void* simply_linked_list_removei(list_t list, unsigned int index)
{
    // TODO
    return NULL;
}

/**************************************************************************************************/
void list_concat(list_t* list1, list_t* list2)
{
    // TODO
}
#endif

/* EOF: simply_linked_list.h */

