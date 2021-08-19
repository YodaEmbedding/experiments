#include <stdio.h>

int main(int argc, char* argv[]) {
    printf("argc: %d\n", argc);
    printf("argv:\n");

    for (int i = 0; i < argc; i++) {
        printf("%s\n", argv[i]);
    }

    return 0;
}
