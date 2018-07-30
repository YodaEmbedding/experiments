// https://stackoverflow.com/questions/51586149/function-that-finds-length-of-a-number-is-returning-an-invalid-output-when-numbe

#include <stdio.h>

int find_num_len(long long int num) {
    return num > 0 ? 1 + find_num_len(num / 10) : 0;
}

int main(void) {
    printf("%d\n", find_num_len(999999999999999));
}
