// https://stackoverflow.com/questions/51056210/2-4-1-when-int-value-assigned-to-pointer-in-c/51056858

#include <stdio.h>

int main() {
    printf("p - q = ??\n");
    printf("==========\n");

    for (int i = 0; i < 5; ++i) {
        for (int j = 0; j < 5; ++j) {
            int* p = i;
            int* q = j;

            printf("%d - %d = %2d\n", p, q, (p - q));
        }
    }

    return 0;
}

