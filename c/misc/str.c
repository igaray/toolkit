#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    char s[] = "1.2 3.4 5.6 7.8";
    char * t = strtok(s, " ");
    float f = 0.0;
    while (t != NULL)  {
        //printf("\"%s\"\n", t);
        f = strtof(t, NULL);
        //printf("%f\n", f);
        t = strtok(NULL, " ");
    }


    char s2[] = "1.2 3.4\n    ";
    char* p = NULL;
    float f2;
    p = s2;
    f2 = strtof(p, &p);
    printf("%f\n", f2);
    f2 = strtof(p, &p);
    printf("%f\n", f2);
    f2 = strtof(p, &p);
    printf("%f\n", f2);
    if (*p == '\n') {
        printf("true\n");
    } else {
        printf("false\n");
    }

    return 0;
}
