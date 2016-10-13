#ifndef _MATRIX_FA_I_H_
#define _MATRIX_FA_I_H_

#include <stdint.h>

typedef uint32_t T;

// Flat-Array Matrix
typedef struct matrix_fa_i_s {
  uint32_t c, r; // Columns & Rows
  T *v;          // Values
} matrix_fa_i_t;

/*
 * Parameters:
 * Return value:
 * Description:
 */
matrix_aa_i_t matrix_aa_i_alloc(uint32_t m, uint32_t n);

/*
 * Parameters:
 * Return value:
 * Description:
 */
void matrix_aa_i_free(matrix_aa_i_t *m);

/*
 * Parameters:
 * Return value:
 * Description:
 */
void matrix_aa_i_zero(matrix_aa_i_t *m);

/*
 * Parameters:
 * Return value:
 * Description:
 */
void matrix_aa_i_print(matrix_aa_i_t *m);

/*
 * Parameters:
 * Return value:
 * Description:
 */
T matrix_aa_i_get(matrix_aa_i_t *m, uint32_t i, uint32_t j);

/*
 * Parameters:
 * Return value:
 * Description:
 */
void matrix_aa_i_set(matrix_aa_i_t *m, uint32_t i, uint32_t j, T v);

/*
 * Parameters:
 * Return value:
 * Description:
 */
int matrix_aa_i_read(matrix_aa_i_t *x, const char *file_name);

#endif
