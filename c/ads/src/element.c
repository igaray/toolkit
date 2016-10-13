#include <stdlib.h>
#include "element.h"

struct element_struct {
  unsigned int value;
};

void element_init(element_t *element) {
  element_t temp;
  temp = malloc(sizeof(element_struct_t));
  temp->value = 0;
  *element = temp;
}

void element_free(element_t *element) {
  free(*element);
  *element = NULL;
}

unsigned int element_get(element_t element) {
  if (element != NULL) {
    return element->value;
  } else {
    return 0;
  }
}

unsigned int element_set(element_t element, unsigned int value) {
  if (element != NULL) {
    element->value = value;
    return 0;
  } else {
    return -1;
  }
}

/* element.c EOF */
