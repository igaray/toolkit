
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_ACCOUNTS 100
#define NAMELEN 4
#define BUFSIZE 100

char buf[BUFSIZE];
char acctname[MAX_ACCOUNTS][NAMELEN];
float balance[MAX_ACCOUNTS];
int numaccts;

int samename(int n) {
    for (int i = 0; i < NAMELEN; i++) {
        if (acctname[n][i] != buf[i]) {
            return 0;
        }
    }
    return 1;
}

void copyname(int n) {
    for (int i = 0; i < NAMELEN; i++) {
        acctname[n][i] = buf[i];
    }
}

int findacct() {
    int i;

    for (i = 0; i < numaccts; i++) {
        if (samename(i)) {
            return i;
        }
    }
    if (i >= MAX_ACCOUNTS) {
        printf("Error: Too many accounts");
        abort();
    }
    /* at this point, i == numaccts */
    copyname(numaccts);
    return numaccts++;
}

void get_lines() {
    int account;
    float amount;

    while (fgets(buf, BUFSIZE, stdin)) {
        account = findacct();
        sscanf(&buf[NAMELEN + 1], "%f", &amount);
        balance[account] += amount;
    }
}

void print_name(int n) {
    for (int i = 0; i < NAMELEN; i++) {
        putchar(acctname[n][i]);
    }
}

void print_balance() {
    printf("\nBalance:\n");
    for (int i = 0; i < numaccts; i++) {
        print_name(i);
        printf(" %g\n", balance[i]);
    }
}

int main() {
    get_lines();
    print_balance();
    return EXIT_SUCCESS;
}
