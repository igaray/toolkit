#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

void print_cli_args(int argc, char **argv) {
  for (int i = 0; i < argc; i++) {
    printf("  %s\n", argv[i]);
  }
}

int main(int argc, char **argv) {
  print_cli_args(argc, argv);
  return EXIT_SUCCESS;
}
