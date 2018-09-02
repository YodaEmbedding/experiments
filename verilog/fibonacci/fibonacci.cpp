#include <iostream>
#include "Vfibonacci.h"
#include "verilated.h"

int main() {
    auto top = new Fibonacci();

    top->a_in = 0;
    top->b_in = 1;

    while (!Verilated::gotFinish()) {
        top->clk = !top->clk;
        top->eval();

        std::cout << top->a_out << std::endl;
    }

    return 0;
}
