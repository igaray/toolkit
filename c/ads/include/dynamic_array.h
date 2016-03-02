#ifndef _DYNAMIC_ARRAY_H_
#define _DYNAMIC_ARRAY_H_

#include <stdlib.h>

/* A dynamic array is a struct with two members: size, of type size_t, and
 * data, which is a pointer to the first element of a dynamically allocated
 * array. The array can be directly accessed, but when using the set and get
 * macros and defining DYNAMIC_ARRAY_BOUNDS_CHECKING, access is checked for 
 * bounds. In the case of SET, if the access index is larger or equal than 
 * size, the array is resized.
 *
 * To use, first the macro darray_declare must be used, which takes as parameter
 * a type. This expands into the functions with the type plugged in.
 * To declare dynamic arrays, use the DARRAY_TYPE macro. 
 * darray_init takes as parameters a type, a dynamic array, and an initial size.
 * darray_set takes a type, a dynamic array, an index, and a value. 
 * darray_get takes a type, a dynamic array, and an index and returns a pointer
 * to the indexed element. If the index is out of bounds a NULL pointer is 
 * returned.
 * darray_free takes a type and a dynamic array, and frees the array contained
 * within it.
 * darray_resize resizes an array to a specified size. Responsibility regarding
 * whether the new size is larger or smaller than the current size and whether
 * the new size is less than the amount of elements lies with the user.
 */

/* The short name 'dynarr' is used to signify 'dynamic array'.
 * 'da' was considered, is nice because it's short, but not representative
 * enough. 'darray' was considered, but it is still not obvious what the 'd' at
 * the beginning stands for. 'dynarr' is relatively short and obviously means
 * 'dynamic array'.
 * The variable name 'a' is always an array.
 * The variable name 'i' is always an index into the array, always of type
 * size_t.
 * The variable name 'v' is always a value, of type TYPE.
 *
 * Some macros were considered and rejected, like one for retrieving the size of
 * a dynarr. It is obvious to the user that the dynarr is always a struct with
 * a 'size' component. No abstraction is needed, and instead confuses.
 *
 * A resizing macro is provided because no function shrinks the array on its
 * own. 
 * The point is to keep the macro collection simple enough to understand
 * quickly, and to save the programmer the effort of copying these very basic
 * functions for each type of dynamic array.
 */

#define dynarr_declare(type)                                                    \
    typedef struct dynarr_##type##_s {                                          \
        size_t  size;                                                           \
        type   *data;                                                           \
    } dynarr_##type##_t;                                                        \
    void dynarr_init_##type(dynarr_##type##_t* a, size_t init_size)             \
    {                                                                           \
        a->size = init_size;                                                    \
        a->data = malloc( sizeof(type) * init_size );                           \
    };                                                                          \
    void dynarr_free_##type(dynarr_##type##_t* a)                               \
    {                                                                           \
        free(a->data);                                                          \
        a->size = 0;                                                            \
        a->data = NULL;                                                         \
    };                                                                          \
    void dynarr_resize_##type(dynarr_##type##_t* a, size_t newsize)             \
    {                                                                           \
        type *tmp = NULL;                                                       \
        tmp = realloc(a->data, newsize);                                        \
        if (tmp != NULL) {                                                      \
            a->size = newsize;                                                  \
            a->data = tmp;                                                      \
        }                                                                       \
    }                                                                           \
    type *dynarr_get_##type(dynarr_##type##_t* a, int i)                        \
    {                                                                           \
        assert(0 <= i && i < a->size);                                          \
        return &(a->data[i]);                                                   \
    };                                                                          \
    int dynarr_set_##type(dynarr_##type##_t* a, size_t i, type v)               \
    {                                                                           \
        assert(0 <= i && i < a->size);                                          \
        type *tmp = NULL;                                                       \
        size_t newsize = a->size;                                               \
        if (i < a->size) {                                                      \
            a->data[i] = v;                                                     \
        } else {                                                                \
            while (newsize <= i) { newsize *= 2; }                              \
            tmp = realloc(a->data, newsize);                                    \
            if (tmp == NULL) { return 1; }                                      \
            a->size = newsize;                                                  \
            a->data = tmp;                                                      \
            a->data[i] = v;                                                     \
        }                                                                       \
        return 0;                                                               \
    }                                                                           \

#define dynarr_type(type)                                                       \
    dynarr_##type##_t

/* a must be of type dynarr_type(TYPE)
 * init_size must be of type size_t
 */
#define dynarr_init(type, a, init_size)                                         \
    dynarr_init_##type(&(a), (init_size))

/* a must be of type dynarr_type(type) */
#define dynarr_free(type, a)                                                    \
    dynarr_free_##type(&(a))

/* a must be of type dynarr_type(type)
 * newsize must be of type size_t
 */
#define dynarr_resize(type, a, newsize)                                         \
    dynarr_resize_##type(&(a), (newsize))

/* a must be of type dynarr_type(type)
 * i must be of type size_t
 */
#define dynarr_get(type, a, i)                                                  \
    dynarr_get_##type(&(a), (i))

/* a must be of type dynarr_type(type)
 * i must be of type size_t
 * v must be of type type
 */
#define dynarr_set(type, a, i, value)                                           \
    dynarr_set_##type(&(a), (i), (value))

#endif

/* EOF dynamic_array.h */
