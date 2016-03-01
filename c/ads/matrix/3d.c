#include <stdio.h>
#include <stdlib.h>

int main() {

    const unsigned int xmax = 100;
    const unsigned int ymax = 100;
    const unsigned int zmax = 100;
    unsigned int grid[xmax][ymax][zmax];
    unsigned int i, x, y, z, steps = 10;

    for (i = 1; i <= steps; i++) {
        printf("step: %d\n", i);
        for (x = 0; x < xmax; x++) {
            for (y = 0; y < ymax; y++) {
                for (z = 0; z < zmax; z++) {
                    grid[x][y][z] = x * y * z;
                    printf("%d\r", grid[x][y][z]);
                }
            }
        }
    }

    return 0;
}

