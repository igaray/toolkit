/* Usage:
 * $ gcc -o text text.c text_text_test.c
 * $ ./text FILE
 */

#include <stdio.h>
#include <stdlib.h>
#include "text.h"

int main(int argc, char *argv[]) {
  int err;
  text_t text;

  err = (argc != 2);
  if (err) {
    printf("ERROR: Wrong number of arguments.\n");
    exit(EXIT_FAILURE);
  }

  /* STACK TEST */
  err = text_from_file(&text, argv[1]);
  if (err) {
    printf("ERROR: Error reading text file.\n");
    exit(EXIT_FAILURE);
  }

  text_print(&text);
  text_free(&text, false);

  /* HEAP TEST */
  text_t *textp = text_alloc();
  err = text_from_file(textp, argv[1]);
  if (err) {
    printf("ERROR: Error reading text lines.\n");
    exit(EXIT_FAILURE);
  }

  text_print(textp);
  text_free(textp, true);

  return EXIT_SUCCESS;
}
