#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define N 2

int a[] = {4, 3};

void print() {
  printf("[%d", a[0]);
  for (int i = 1; i < N; i++) {
    printf(" %d", a[i]);
  }
  printf("]\n");
}

void swap(int i, int j) {
  int t = a[i];
  a[i] = a[j];
  a[j] = t;
}

void sort() {
  if (a[1] < a[0]) {
    swap(0, 1);
  }
}

int main() {
  print();
  sort();
  print();
  return EXIT_SUCCESS;
}
