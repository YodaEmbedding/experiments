#include <iostream>

int power2(int num, int power)
{
    int _return;

    asm (
    // asm volatile (
        // "mov $420, %%eax;"
        "mov %1,   %0;"
        "mov %2,   %%ecx;"
        "shl %%cl, %0;"
        : "=r" (_return)
        : "r" (num)
        , "c" (power)
        : "eax"
        // , "0"
    );

    return _return;
}

int main() {
    std::cout
        << "3 times 2 to the power of 5 is "
        << power2(3, 5)
        << std::endl;
}

