#ifndef _STACK_ARRAY_H_
#define _STACK_ARRAY_H_

#include <stdlib.h>
  
#define stckarr_declare(type)                                                   \
    typedef struct stckarr_##type##_s {                                         \
        size_t size;                                                            \
        size_t top;                                                             \
        type *data;                                                             \
    } stckarr_##type##_t;                                                       \
    void stckarr_init_##type(stckarr_##type##_t *s, size_t init_size)           \
    {                                                                           \
        s->top  = 0;                                                            \
        s->size = init_size;                                                    \
        s->data = malloc( sizeof(type) * init_size );                           \
    }                                                                           \
    void stckarr_free_##type(stckarr_##type##_t *s)                             \
    {                                                                           \
        free(s->data);                                                          \
        s->size = 0;                                                            \
        s->top  = 0;                                                            \
        s->data = NULL;                                                         \
    }                                                                           \
    type *stckarr_pop_##type(stckarr_##type##_t *s)                             \
    {                                                                           \
        if (s->top > 0) {                                                       \
            s->top -= 1;                                                        \
            return &(s->data[s->top + 1]);                                      \
        } else {                                                                \
            return NULL;                                                        \
        }                                                                       \
    }                                                                           \
    int stckarr_push_##type(stckarr_##type##_t *s, type v)                      \
    {                                                                           \
        type *tmp = NULL;                                                       \
        if (s->top < s->size) {                                                 \
            s->top += 1;                                                        \
            s->data[s->top] = v;                                                \
        } else {                                                                \
            tmp = realloc(s->data, s->size * 2);                                \
            if (tmp == NULL) { return 1; };                                     \
            s->size *= 2;                                                       \
            s->top  += 1;                                                       \
            s->data  = tmp;                                                     \
            s->data[s->top] = v;                                                \
        }                                                                       \
        return 0;                                                               \
    }

#define stckarr_type(type)                                                      \
    stckarr_##type##_t

#define stckarr_init(type, s, init_size)                                        \
    stckarr_init_##type(&(s), (init_size))

#define stckarr_free(type, s)                                                   \
    stckarr_free_##type(&(s))

#define stckarr_push(type, s, v)                                                \
    stckarr_push_##type(&(s), (v))

#define stckarr_pop(type, s)                                                    \
    stckarr_pop_##type(&(s))

#define stckarr_empty(s)                                                        \
    ((s)->top == 0)

#define stckarr_size(s)                                                         \
    ((s)->top + 1)

#endif

/* EOF: stack_array.h*/
