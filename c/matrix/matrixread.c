/* Usage:
 * $ gcc -o matrixread matrixread.c
 * $ ./matrixread matrix.txt
 */

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>

/* TODO:
 * - error checking in init_text
 * - error checking in read_lines
 */

typedef struct text_s {
    size_t sz;
    char** lines;
} text_t;

int open_file(const char* filename, int *fd, int* file_size, char** data) {
    struct stat sb;

    int _fd = open(filename, O_RDONLY);
    if (_fd == -1) { return -1; }

    int status = fstat(_fd, &sb);
    if (status == -1) { return -1; }

    int _file_size = sb.st_size;
    char *_data = mmap(0, _file_size, PROT_READ, MAP_SHARED, _fd, 0);
    if (_data == NULL) { return -1; }

    *fd = _fd;
    *file_size = _file_size;
    *data = _data;
    return 0;
}

void close_file(int fd) {
    close(fd);
}

text_t* init_text(int lines) {
    text_t* textp = malloc(sizeof(text_t));
    textp->sz = lines;
    textp->lines = malloc(sizeof(char*) * lines);
    for (int i = 0; i < lines; i++) {
        textp->lines[i] = NULL;
    }
    return textp;
}

void free_text(text_t* text) {
    for (int i = 0; i < text->sz; i++) {
        free(text->lines[i]);
    }
    free(text->lines);
    free(text);
}

void print_text(text_t* text) {
    for (int i = 0; i < text->sz; i++) {
        printf("text[%d] = %s", i, text->lines[i]);
    }
}

void read_lines(char* data, int file_size, text_t* text) {
    int i = 0;
    for (int line = 0; line < text->sz; line++) {
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
}

void read_matrix(int m[], text_t* text) {
    for (int i = 0; i < 4; i++) {
        sscanf(text->lines[i], "%d %d %d %d\n", 
	       &m[i][0], &m[i][1], &m[i][2], &m[i][3]);
    }
} 

void print_matrix(int m[]) {
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            printf("m[%d][%d] = %d\n", i, j, m[i][j]);
        }
    }
}

int main(int argc, char* argv[]) {
    int err = 0;
    int fd;
    char *data;
    int file_size, lines;
    text_t* text;

    err = (argc != 2);
    if (err) { exit(EXIT_FAILURE); }

    err = open_file(argv[1], &fd, &file_size, &data);
    if (err) { exit(EXIT_FAILURE); }

    /*
    char s[] = "123 456 789";
    int a = 0;
    int b = 0;
    int c = 0;
    sscanf(s, "%d %d %d", &a, &b, &c);
    printf("%d %d %d\n", a, b, c);
    */

    int m[4][4];
    read_matrix(m, text);
    print_matrix(m);

    free_text(text);
    close_file(fd);
    return EXIT_SUCCESS;
}
