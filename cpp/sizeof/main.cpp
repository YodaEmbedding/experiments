#include <iostream>
#include <limits>


int main() {
    std::cout << "char:     " << sizeof(char)   << std::endl;
    std::cout << "int:      " << sizeof(int)    << std::endl;
    std::cout << "long:     " << sizeof(long)   << std::endl;
    std::cout << "float:    " << sizeof(float)  << std::endl;
    std::cout << "double:   " << sizeof(double) << std::endl;

    std::cout << "\nNumeric properties:" << std::endl;
    std::cout << "float:    "
        << "is_iec559="
        << std::boolalpha
        << std::numeric_limits<float>::is_iec559
        << ", digits="
        << std::numeric_limits<float>::digits
        << std::endl;
}
