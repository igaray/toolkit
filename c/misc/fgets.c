#include <stdio.h>
#include <stdlib.h>

#define BUFSIZE 128

char buf[BUFSIZE];

int main() {
    int ret = fgets(buf, BUFSIZE, stdin);
    printf("%d\n", ret);
    printf("%s\n", buf);

    return EXIT_SUCCESS;
}
