#ifndef _ELEMENT_H_
#define _ELEMENT_H_

typedef struct element_struct element_struct_t;
typedef element_struct_t *element_t;

/*
 * Example usage:
 * element_t e;
 * eleemnt_init(&e);
 */
void element_init(element_t *element);

/*
 * Example usage:
 * element_t e;
 * element_free(&e);
 */
void element_free(element_t *element);

unsigned int element_get(element_t element);

unsigned int element_set(element_t element, unsigned int value);

#endif

/* element.h EOF */
