#ifndef _TEXT_H_
#define _TEXT_H_

#include <stdbool.h>
#include <stdio.h> 
#include <stdlib.h> 

typedef struct text_s {
    size_t size;
    char** lines;
} text_t;

/*
 * Return value: a pointer to a text_t struct newly allocated on the heap.
 * Description: allocates a text_t struct on the heap.
 */
text_t* text_alloc();

/*
 * Parameters:
 * - in text: a pointer to a text_t struct
 * - in heap: a boolean indicating whether the struct pointed to by the text
 *   parameter is located on the stack or on the heap. If the caller declared
 *   the struct as a variable local to the caller function, false should be
 *   passed in. If the struct was allocated via the text_alloc function, true
 *   should be passed in.
 * Description: frees the memory used by a text_t object.
 */
void text_free(text_t* textp, bool heap);

/*
 * Parameters: 
 * - out textp: a pointer to a text_t struct.
 * - in lines: an integer indicating how many lines the text has.
 * Return value: an error code, 0 if successful, -1 otherwise.
 * Description: initializes the memory necessary for a text_t object.
 */
int text_init(text_t* textp, int lines);

/*
 * Parameters:
 * - in textp: a pointer to a text_t struct.
 * Description: prints the text_t struct's contents on standard output.
 */
void text_print(text_t* textp);

/*
 * Parameters:
 * - out textp: a pointer to a text_t struct.
 * - in file_name: a string containing a file name.
 * Return value: an error code, 0 if successful, -1 otherwise.
 * Description: reads the contents of the indictated file into a text_t struct.
 */
int text_from_file(text_t* textp, const char* file_name);

#endif
