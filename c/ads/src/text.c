#include "text.h"
#include "file.h"

int text_num_lines(int size, char *data);
int text_load(text_t *text, int file_size, char *data);

text_t *text_alloc() {
  text_t *textp = malloc(sizeof(text_t));
  return textp;
}

void text_free(text_t *textp, bool heap) {
  for (int i = 0; i < textp->size; i++) {
    free(textp->lines[i]);
  }
  free(textp->lines);
  if (heap) {
    free(textp);
  }
}

int text_init(text_t *textp, int lines) {
  if (textp == NULL) {
    return -1;
  }

  textp->size = lines;
  textp->lines = malloc(sizeof(char *) * lines);
  if (textp->lines == NULL) {
    return -1;
  }

  for (int i = 0; i < lines; i++) {
    textp->lines[i] = NULL;
  }
  return 0;
}

void text_print(text_t *textp) {
  printf("size: %zu\n", textp->size);
  for (int i = 0; i < textp->size; i++) {
    printf("text[%d] = %s", i, textp->lines[i]);
  }
  printf("\n");
}

int text_from_file(text_t *textp, const char *file_name) {
  char *data;
  int err, fd, file_size, num_lines;

  err = open_file(file_name, &fd, &file_size, &data);
  if (err) {
    return -1;
  }

  num_lines = text_num_lines(file_size, data);

  err = text_init(textp, num_lines);
  if (err) {
    return -1;
  }

  err = text_load(textp, file_size, data);
  if (err) {
    return -1;
  }

  close(fd);
  return 0;
}

/* Auxiliary and private functions */

/*
 * Parameters:
 * Return value:
 * Description:
 */
int text_num_lines(int size, char *data) {
  int lines = 0;
  for (int i = 0; i < size; i++) {
    if (data[i] == '\n') {
      lines++;
    }
  }
  return lines;
}

/*
 * Parameters:
 * Return value:
 * Description:
 */
int text_load(text_t *text, int file_size, char *data) {
  int i = 0;
  for (int line = 0; line < text->size; line++) {
    // find the current line length
    int l = 0;
    int j = i;
    while (data[j] != '\n' && j < file_size) {
      j++;
      l++;
    }
    l++;

    // allocate memory for the current line
    text->lines[line] = malloc(sizeof(char) * (l + 1));
    if (text->lines[line] == NULL) {
      return -1;
    }

    // copy the line into the array
    for (int k = 0; k < l; k++) {
      text->lines[line][k] = data[i + k];
    }
    text->lines[line][l] = '\0';

    // advance the data index to the beginning of the next line
    while (data[i] != '\n' && i < file_size) {
      i++;
    }
    i++;
  }
  return 0;
}
