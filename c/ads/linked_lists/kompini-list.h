/* Originally distributed as part of the Kompimi C Data Structure Library.
   For updates, see: https://sourceforge.net/projects/kompimi-cdsl/

   All content in this document is granted into the public
   domain. Where this is not legally possible, the copyright owner
   releases all rights. This notice may be modified or removed.
*/

/** @defgroup list list module
    Structures, macros, and methods supporting the list data structure.

   list is a data structure that supports efficient insertion and
   removal anywhere in the list in constant time, but unlike arrays
   does not support indexing. It supports heterogeneous data - all
   insertions and retrievals are in the form of untyped void pointers.

   Its implementation is based on unrolled doubly-linked linked lists
   with external storage, which are more space-efficient and
   cache-efficient than simple linked lists.

   See tests/list_test.c for example code.

    @{
*/

#ifndef _LIST_

#include <assert.h>

#include "config.h"

/*! \brief (Internal) A list node.

   Holds a small array containing a fixed number of list elements,
   along with a count of how many are in use, and pointers to the
   previous and next nodes. Should not be accessed directly.
*/
typedef struct list_node_t
{
    /*! \brief Count of how many logical elements are stored in this node. */
    int count;
    /*! \brief Pointer to next node in list, or NULL if this is the last node. */
    struct list_node_t* next;
    /*! \brief Pointer to previous node in list, or NULL if this is the first node. */
    struct list_node_t* prev;
    /*! \brief Fixed-size array containing pointers to data.
        See ::ELEMENTS_PER_LIST_NODE. */
    void* data[ELEMENTS_PER_LIST_NODE];
} list_node;

/*! \brief A list data structure.

   The structure is intended to be stack-allocated or embedded in
   other data structures; it uses external storage for its nodes.
*/
typedef struct
{
    /*! \brief The current logical number of elements in the list.
        Read-only. */
    int size;
    /*! \brief (Internal) Pointer to first node, or NULL if list is empty. */
    list_node* first_node;
    /*! \brief (Internal) Pointer to last node, or NULL if list is empty. */
    list_node* last_node;
} list;

/*! \brief (Internal) A list iterator, referring to a position in a list.

  A list node iterator refers to the position of a specific element in
  the list. In this implementation's representation, it needs to store
  the current node as well as the offset into that node. For the end
  iterator (one past the last element) the node pointer is NULL and
  the offset is irrelevant.

  A consequence of this representation is that insertions or removals
  in the same node may invalidate iterators by causing a node split or
  merge. Only insertions at the end are safe against this
  problem. Design question: should the data structure be fixing up
  iterators when it splits or merges nodes? Would a list chop
  primitive help?
*/
typedef struct
{
    /*! \brief The list into which this iterator points. */
    list* lst;
    /*! \brief The node containing the current element. */
    list_node* node;
    /*! \brief The offset into the node's data array of the current element. */
    int offset;
}  list_iter;

/*! \brief Creates a new empty list. */
list list_create(void);

/*! \brief Destroys a list.
    Must be called on a list before it goes out of scope.
    \param lst Pointer to the list to destroy.
*/
void list_destroy(list* lst);

/*! \brief Inserts a value into a list after the element referred to by the given iterator.

   Requires constant (O(1)) time. Invalidates all iterators into the
   list, except the supplied one which is updated as necessary.

   \param iter A pointer to the iterator to insert after.
   \param value The value to insert.

   \return Zero if out of memory, nonzero if successful.
*/
int list_insert_after(list_iter* iter, void* value);

/*! \brief Inserts a value into a list before the element referred to by the given iterator.

   Requires constant (O(1)) time. Invalidates all iterators into the
   list, except the supplied one which is updated as necessary.

   \param iter A pointer to the iterator to insert before.
   \param value The value to insert.

   \return Zero if out of memory, nonzero if successful.
*/
int list_insert_before(list_iter* iter, void* value);

/*! \brief Inserts a value at the beginning of a list.

   Requires constant (O(1)) time. Invalidates all iterators into the
   list.

   \param lst Pointer to the list to insert into.
   \param value The value to insert.

   \return Zero if out of memory, nonzero if successful.
*/
int list_insert_beginning(list* lst, void* value);

/*! \brief Inserts a value at the end of a list.

   Requires constant (O(1)) time.

   \param lst Pointer to the list to insert into.
   \param value The value to insert.

   \return Zero if out of memory, nonzero if successful.
*/
int list_insert_end(list* lst, void* value);

/*! \brief Removes an element from a list.

   Requires constant (O(1)) time. Invalidates all iterators
   except the supplied one, which is updated to refer to the
   following element.

   \param iter A pointer to the iterator to remove at.
*/
void list_remove(list_iter* iter);

/*! \brief Removes a value from the beginning of a nonempty list.

   Requires constant (O(1)) time. Invalidates all iterators
   into the list.

   \param lst Pointer to the list to remove from.
*/
void list_remove_beginning(list* lst);

/*! \brief Removes a value from the end of a nonempty list.

   Requires constant (O(1)) time. Invalidates no iterators.

   \param lst Pointer to the list to remove from.
*/
void list_remove_end(list* lst);

/*! \brief Retrieves an iterator referring to the first element of a list.

   If the list is empty, this returns the end iterator.

   \param lst Pointer to the list. Is not modified by this call.

   \return An iterator referring to the first element of the list.
*/
list_iter list_first(list* lst);

/*! \brief Retrieves an iterator referring to the last element of a list.

   If the list is empty, this returns the end iterator; otherwise it
   does not.

   \param lst Pointer to the list. Is not modified by this call.

   \return An iterator referring to the last element of the list.
*/
list_iter list_last(list* lst);

/*! \brief Moves an iterator to the next element of the list.

   If the iterator is already the end iterator, fails.

   \param iter Pointer to the iterator to be updated.
*/
#define /*void*/ list_next(/* list_iter* */ iter) \
    (assert ((iter) != NULL && !list_at_end(*(iter))), \
     (iter)->offset++, \
     ((iter)->offset >= (iter)->node->count) ? \
         ((iter)->offset = 0, \
	  (iter)->node = (iter)->node->next) : 0)

/*! \brief Determines if an iterator is the end iterator.

    The end iterator points one location past the end of the list.
    Once it is reached, list_next will have no further effect.

    \param iter The iterator being tested.

    \return Nonzero if iterator is at end of list, else zero.
*/
#define /*int*/ list_at_end(/*list_iter*/ iter) \
            ((iter).node == NULL)

/*! \brief Determines if an iterator is pointing to the first element.

    \param iter The iterator being tested.

    \return Nonzero if iterator is at beginning of list, else zero.
*/
#define /*int*/ list_at_beginning(/*list_iter*/ iter) \
            ((iter).node == (iter).lst->first_node && (iter).offset == 0)

/*! \brief Gets the data referred to by a given iterator.

    Has undefined results if invoked on an invalidated iterator
    or the end iterator. May evaluate its argument more than once.

    \param iter The iterator being dereferenced.

    \return The data referred to by the given iterator.
*/
#define list_get_data(iter) \
            ((iter).node->data[(iter).offset])

/*! \brief Moves an iterator to the previous element of the list.

   If the iterator is already at the beginning, fails.

   \param iter Pointer to the iterator to be updated.
*/
#define /*void*/ list_prev(/*list_iter**/ iter) \
    (assert ((iter) != NULL && !list_at_end(*(iter))), \
     (iter)->offset--, \
     ((iter)->offset < 0) ? \
         ((iter)->node = (iter)->node->prev, \
          (iter)->offset = (iter)->node->count - 1) : 0)

/*! \brief A macro used to help iterate through a list easily.

   Begins the loop that iterates through the list. The loop
   is ended with the matching LIST_ITERATE_END() macro.

   \param lst The list to iterate over.
   \param iterator The name to use for the iterator which will refer successively to each value in the array.
*/
#define LIST_ITERATE(lst, iterator) \
    { \
        list_iter iterator; \
        for ((iterator) = list_first(lst); !list_at_end(iterator); list_next(&iterator)) \
        {

/*! \brief Closes an array iteration loop opened by LIST_ITERATE(). */
#define LIST_ITERATE_END() \
        } \
    }

/*! \brief Cheaply swaps one list's contents with another's. */
void list_swap(list* lst1, list* lst2);

#endif /* #ifndef _LIST_ */

/** @} */ /* end of group list */
